# Air Quality Project

## About
The University of Chicago Center for Spatial Data Science (CSDS) and the Chicago Department of Public Health (CDPH) worked alongside the [Partnership for Healthy Cities](https://partnershipforhealthycities.bloomberg.org/), a global network of cities committed to reducing noncommunicable diseases that is supported by Bloomberg Philanthropies in partnership with the World Health Organization and Vital Strategies, and with partners at City Tech Collaborative, to collect and visualize baseline air quality data onto the [Chicago Health Atlas](https://www.chicagohealthatlas.org/).

In the next step of this initiative, CDPH and CSDS will continue to work with the Partnership to analyze air quality data by taking community characteristics into account.

Considering health status and demographics are crucial to understanding the disproportionate effect air pollution has on some communities. Together, these air quality and community data can inform strategies that will improve health outcomes for all residents.

## Dashboard
An interactive dashboard will provide user access to sensor locations and aggregated data products of sensor and covariates by community area.

## Data
We group data into the following categories: (1) Monitoring data, (2) AOD data, and (3) Covariate data. An overview of key features for each category follows.

### Monitoring Data

Name | Type | Measured parameter | Spatial resolution | Time resolution | Comments
--- | --- | --- | --- | --- | --- 
AirNow | EPA's sensor network | Criteria pollutants | Sensor location | Hourly data | Limited spatial coverage
AoT | Sensor network | NO2, O3, H2S, CO, SO2, PM (~10% of nodes) | Sensor location | ~30 seconds | Non-calibrated air quality data
AirCasting | Personal monitoring | PM, Sound level, Relative humidity, Temperature | User location (fixed or mobile session) | 1s for mobile session, 1mn for fixed session | Low-cost sensor (non-reliable data)


### Aerosol Optical Depth (AOD) Data
AOD data is being made available via ftp service to CSDS, as shared via NASA. Data will need to be processed from raw HDF format to AOD measurements for each grid square within our spatial infrastructure.

Name | Type | Measured parameter | Spatial resolution | Time resolution | Comments
--- | --- | --- | --- | --- | --- 
Aerosol Optical Depth (AOD) | MAIAC adjusted MODIS satellite data from NASA | light attenuation in the column (~ pm) | 1km2 grid | daily | Most reliable when site is clear

### Covariate (ie. Drivers, Spatio-Temporal Predictor) Data
Multiple variables for both modelling approaches are required. The drivers and spatio-temporal predictors are additionally available in differing spatial and temporal formats. We log the available data in our data inventory (with features in the following table), though additionally processing is required in most cases for each approach (ie. buffer analysis with join for LUR model, or aggregation of data to grid-square for hybrid model). Note that there are multiple options for several categories; these will be finalized in the next stage, pending feedback.


Name | Source(s) | File | Spatial Resolution | Time Resolution | Comments
--- | --- | --- | --- | --- | --- 
Point Emission Data | NEI and CDPH | CSV | address-level | varies | Options for use: (1) Distance to nearest facility (either all or subset of high polluters), (2) Raster surface (over 100 available for Chicago area)
Area Emission Data: building heights as proxy or heating fuel | OSM, LiDar from Cook County GIS | varies | varies | varies | NEI area data not available below county-level
Road Emissions: (1) road length summary, (2) traffic volume summary | OSM, IDOT | SHP | varies | varies| Multiple proxies
Meteorological Data | NOAA monitoring stations | CSV | address-level | Daily, Monthly | few validated weather stations in metro area; weather underground data not reliabale and historical data is costly
NDVI - Greenness Index | MODIS | raster | 1km, 500m, 250, grid | Monthly, Yearly | 
Land Cover | SAL product, CMAP | raster | 30m | 2016, 2011, 2006 |
Land Use | CMAP | raster | parcel | 2013, 2010, 2005, 2001 | | 
Elevation | National Land Elevation Model | raster | 1m | yearly | alt: building height dataset
Demographic and SES data | ACS | CSV, SHP | tract | 5-year average | 


## Analysis
Our analytic approach involves data collection and spatial analysis. We break this down into the following stages; (1) Methods and Data Review with Infrastructure Set-Up, (2) Model Implementations, and (3) Comparison with Sampling Recommendations. As soon as calibrated PM2.5 measures become available, we will spatially estimate an interpolated surface of these measures and identify unusual values.

In the first stage, the CSDS team conducted a literature review to identify and prioritize covariate (ie. driver, predictor) data as well as better understand and refine modeling approaches in a contemporary methodological landscape. We developed a data inventory based on project demands and best practices, and have refined the spatial data infrastructure required to store and maintain the data clearinghouse. We implemented a postgres/POSTGIS infrastructure on the University of Chicago Remote Computing Cluster (RCC) server, and interface with the R ecosystem for data cleaning, transformation, and management. This system is being further refined and optimized for not only data management, but also spatial analytic operations (ie. buffer calculation) as well as modeling. 

In the second stage, we will implement two models: Land Use Regression (LUR) and a Hybrid model that incorporates available satellite data. Both approaches are dependent on available sensor data (including regulatory, AoT availability, and citizen sensors).


## Resources
### Trainings
[NASA Applied Remote Sensing Training](https://arset.gsfc.nasa.gov) (ARSET) program offers satellite remote sensing training that builds the skills to integrate NASA Earth Science data into an agency’s decision-making activities. Trainings are offered in air quality, climate, disaster, health, land, water resources, and wildfire management.
* [Fundamentals of Remote Sensing](https://arset.gsfc.nasa.gov/webinars/fundamentals-remote-sensing)
* [High Temporal Resolution Air Quality Observations from Space](https://arset.gsfc.nasa.gov/airquality/webinars/2018-geospatial)
* [Advanced Webinar: Data Analysis Tools for High Resolution Air Quality Satellite Data Sets](https://arset.gsfc.nasa.gov/airquality/webinars/2018-hiresdatasets)

Array of Things (AoT)
* [Mapping Array of Things (AoT) Data with Spatial Statistics](https://geodacenter.github.io/aot-workshop/)

### COMMUNITIES of PRACTICE
[NASA Health and Air Quality Applied Sciences Team](https://haqast.org/) (HAQAST) is a collaborative team that works in partnership with public health and air quality agencies to use NASA data and tools for the public benefit.


