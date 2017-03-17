// Script to download MODIS MOD13Q1 data for Q. pyrenaica forests in Sierra Nevada. 
// author: @ajpelu 
// 2017 February 


// --- Load coordinates and distribution of Q. pyrenaica pixel

// Add centroid of selected pixels 
var iv_malla_modis_qp = ee.FeatureCollection('ft:1mLqHNuoqK0KN5PqTyxEzja1SweEnnQ9KpZ_qS_2b');


// Add shape with distribution of the Q. pyrenaica forests 
var qp = ee.FeatureCollection('ft:1fwblBGuNzm8h-wI6qMwG1uhiNrrhPH3NLduePp-a', 'geometry');
Map.centerObject(iv_malla_modis_qp,11);

// Map.addLayer(qp);
// Map.addLayer(iv_malla_modis_qp);


// --- Load MODIS MOD13Q1 

// Define study period 
var startdate = ee.Date('2001-01-01');
var enddate = ee.Date('2016-12-31'); 

// filter by date 
// select bands: EVI, NDVI, DOY, SummaryQA
var mod13 = ee.ImageCollection('MODIS/006/MOD13Q1')
  .filterDate(startdate, enddate)
  .select(['EVI', 'NDVI', 'DayOfYear','SummaryQA','DetailedQA']); 

print(mod13);

var qa = mod13.select(['DetailedQA']);

var qa_quality = ee.ImageCollection(qa.map(function(image) {
  // compute the qa bands 
  // see https://lpdaac.usgs.gov/sites/default/files/public/modis/docs/MODIS_LP_QA_Tutorial-2.pdf 
  var qa_quality = image.bitwiseAnd(0x3).rename('qa_quality');
  var qa_use  = image.rightShift(2).bitwiseAnd(0xF).rename('qa_use');
  var qa_aerosol = image.rightShift(6).bitwiseAnd(0x3).rename('qa_aerosol');
  var qa_adj_cloud = image.rightShift(8).bitwiseAnd(0x1).rename('qa_adj_cloud');
  var qa_atmos = image.rightShift(9).bitwiseAnd(0x1).rename('qa_atmos');
  var qa_mix_cloud = image.rightShift(10).bitwiseAnd(0x1).rename('qa_mix_cloud');
  var qa_landwater = image.rightShift(11).bitwiseAnd(0x7).rename('qa_landwater');
  var qa_snow = image.rightShift(14).bitwiseAnd(0x1).rename('qa_snow');
  var qa_shadow = image.rightShift(15).bitwiseAnd(0x1).rename('qa_shadow')
  
  // concatenate all the bands to qa_quality and return it 
  return image.select()
    .addBands(qa_quality)
    .addBands(qa_use) 
    .addBands(qa_aerosol)
    .addBands(qa_adj_cloud)
    .addBands(qa_atmos)
    .addBands(qa_mix_cloud)
    .addBands(qa_landwater)
    .addBands(qa_snow)
    .addBands(qa_shadow); 
}));

print(qa_quality);

// Define an inner join to merge the two imageCollections
var innerJoin = ee.Join.inner();

// Specify an identifier to join the imageCollections 
var idsForJoin  = ee.Filter.equals({
  leftField: 'system:index',
  rightField: 'system:index'
});

// Apply the join 
var combinedBands = innerJoin.apply(mod13, qa_quality, idsForJoin);

// Display the join result: a FeatureCollection.
print('Combined Bands:', combinedBands);


// Map a function to merge the results in the output FeatureCollection.
var combinedBandsQA = combinedBands.map(function(feature) {
  return ee.Image.cat(feature.get('primary'), feature.get('secondary'));
});

// Print the result of merging.
print('CombinedBandsQA:', combinedBandsQA);

// set the result as imageCollection 
var combinedBandsQA = ee.ImageCollection(combinedBandsQA);

// Apply a function to every image of the collection (use .map()) 
// Get value of each band b of each temporal image t of MODQ13Q1
// for each of the coordinate provide in iv_malla_modis_qp
var mydata = combinedBandsQA.map(function(i) {
  return i.reduceRegions(iv_malla_modis_qp, 'first');
});

print(mydata); 


// TODO: Complete doc 
var extract = ee.FeatureCollection(mydata.flatten());

var extract2 = extract.select([
  'DayOfYear','EVI', 'NDVI','DetailedQA','SummaryQA', 
  'iv_malla_modi_id', 'pop',
  'qa_quality', 'qa_use', 'qa_aerosol', 'qa_adj_cloud', 'qa_atmos', 
  'qa_mix_cloud', 'qa_landwater', 'qa_snow', 'qa_shadow']); 


var extract3 = extract2.map(function(f) { 
  var g = f.geometry().coordinates();
  return f.set('lat', g.get(1),'long', g.get(0));
});


var extract4 = extract3.map(function(feature) {
  return ee.Feature(feature.select([".*"],null, false));
});

// Export table
Export.table.toDrive({
  collection: extract4,
  description: 'iv_qp_raw_qa_2017',
  fileFormat: 'CSV',
});

