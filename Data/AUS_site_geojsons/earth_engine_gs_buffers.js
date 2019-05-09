/*
// imports
var population = ee.ImageCollection("JRC/GHSL/P2016/POP_GPW_GLOBE_V1"),
    settlement = ee.ImageCollection("JRC/GHSL/P2016/SMOD_POP_GLOBE_V1"),
    greenspaces = ee.FeatureCollection("users/mitchest/ebird_hotspots/ugii_aus_greenspaces");
*/

/*
The GHSL relies on the design and implementation of new spatial data mining technologies allowing to automatically 
process and extract analytics and knowledge from large amount of heterogeneous data including: global, fine-scale 
satellite image data streams, census data, and crowd sources or volunteered geographic information sources.

Resolution: 250 meters
Bands: population_count
Bands: 0 Inhabited areas
       1 RUR (rural grid cells)
       2 LDC (low density clusters)
       3 HDC (high density clusters)
*/

// choose right epochs
population = population.filterDate("2015-01-01").mean()
settlement = settlement.filterDate("2015-01-01").mean()
//print(population)


// Buffer the green spaces
// (map a buffer function)
var buffer_5 = function(feature) {
  return feature.buffer(5000)
}
var buffer_25 = function(feature) {
  return feature.buffer(25000)
}
var buffer_50 = function(feature) {
  return feature.buffer(50000)
}

var gs_5 = greenspaces.map(buffer_5)
var gs_25 = greenspaces.map(buffer_25)
var gs_50 = greenspaces.map(buffer_50)

Map.addLayer(greenspaces, {}, 'greenspaces')
Map.addLayer(gs_5, {}, '5km buffer')
Map.addLayer(gs_25, {}, '25km buffer')
Map.addLayer(gs_50, {}, '50km buffer')

// now calculate the buffer metrics
// function to do the calculation
var reduce_buffer_mean = function(image_in, buffer) {
  return image_in.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.mean(),
    scale: 250
  })
}
var reduce_buffer_sum = function(image_in, buffer) {
  return image_in.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.mean(),
    scale: 250
  })
}

var pop_mean_5 = reduce_buffer_mean(population, gs_5)
var pop_mean_25 = reduce_buffer_mean(population, gs_25)
var pop_mean_50 = reduce_buffer_mean(population, gs_50)

var pop_sum_5 = reduce_buffer_sum(population, gs_5)
var pop_sum_25 = reduce_buffer_sum(population, gs_25)
var pop_sum_50 = reduce_buffer_sum(population, gs_50)

var set_mean_5 = reduce_buffer_mean(settlement, gs_5)
var set_mean_25 = reduce_buffer_mean(settlement, gs_25)
var set_mean_50 = reduce_buffer_mean(settlement, gs_50)

/*
var addArea = function(feature) {
    var with_total_area = feature.set({area_m2: ee.Number(feature.geometry().area())})
    return with_total_area.set({area_value_m2: ee.Number(with_total_area.get('sum')).multiply(30 * 30)})
  }
count_value.map(addArea)
*/

// Export results
// function to do the export
var export_buffer_stats = function(buf_stats, out_name) {
  Export.table.toDrive({
    collection: buf_stats,
    description: out_name,
    folder: 'ebird',
    fileFormat: 'CSV'
  });
}

export_buffer_stats(pop_mean_5, "pop_mean_5")
export_buffer_stats(pop_mean_25, "pop_mean_25")
export_buffer_stats(pop_mean_50, "pop_mean_50")

export_buffer_stats(pop_sum_5, "pop_sum_5")
export_buffer_stats(pop_sum_25, "pop_sum_25")
export_buffer_stats(pop_sum_50, "pop_sum_50")

export_buffer_stats(set_mean_5, "set_mean_5")
export_buffer_stats(set_mean_25, "set_mean_25")
export_buffer_stats(set_mean_50, "set_mean_50")