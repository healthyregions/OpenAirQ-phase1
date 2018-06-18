# Sensors DB - Readme

##Overview

* `aot_download_bk` - AoT's sensors raw data, accessed through bulk download (*source :* <https://aot-file-browser.plenar.io/>)
* `aot_api_bk` - AoT's sensors published data, accessed through the Plenar.io API (*source :* <https://dev.plenar.io/api/v2/aot>)
* `aot_node` - AoT's nodes metadata (*source :* <https://aot-file-browser.plenar.io/>)
* `aot_sensor` - AoT's sensors metadata (*source :* <https://aot-file-browser.plenar.io/>)
* `aot_30mn_data` - mean values over 30mn time periods of the cleaned AoT's sensors data (*source :* `aot_download_bk`/`aot_api_bk` processed file)
* `aqs_bk` - AQS's sensors data for criteria pollutants (*source :* <https://aqs.epa.gov/api>)
* `aqs_site` - AQS's sites metadata (*source :* <https://aqs.epa.gov/aqsweb/airdata/download_files.html>)
* `aqs_monitor` - AQS's monitors metadata (*source :* <https://aqs.epa.gov/aqsweb/airdata/download_files.html>)
* `aqs_hourly_data` - hourly mean values of EPA's sensors in a appropriate format (*source :* `aqs_bk` processed file)

##Array of Things (AoT)

### AoT Data Backup



### AoT API Backup
* `node_id` - ID of node which did the measurement.
* `timestamp` - UTC timestamp of when the measurement was done.
* `plugin` - Plugin which did the measurement.
* `sensor` - Sensor that was measured.
* `parameter` - Sensor parameter that was measured.
* `value` - Measured value.

### Nodes metadata
* `node_id` - ID of node.
* `project_id` - ID of project which manages node.
* `vsn` - Public name for node. The VSN is visible on the physical enclosure.
* `address` - Street address of node.
* `lat` - Latitude of node.
* `lon` - Longitude of node.
* `description` - More detailed description of node's build and configuration.
* `type` - `C` if the node is equipped with chemical sensors (Chemsense), `A` if the node is equipped with PM sensors (Alphasense) and `CA` if the node is equipped with both.

### Sensors metadata
* `sid` - Unique identifier for the pair sensor-parameter.
* `sensor_arg` - Sensor name as given in the Argonne metadata file.
* `param_arg` - Sensor parameter as given in the Argonne metadata file.
* `sensor_name` - Sensor name found in additional sources (same as `sensor_arg`for the majority of cases).
* `unit` - Physical units of sensor value.
* `min_val` - Minimum value according to datasheet. Used as lower bound in range filter.
* `max_val` - Maximum value according to datasheet. Used as upper bound in range filter.
* `accy` - Accuracy of the sensor according to datasheet.
* `board`- Waggle sensor board the sensor belongs to (`Metsense`, `Chemsense`, `Lightsense`, `Alphasense`).
* `obj`- Purpose/Application of the sensor (`Weather Conditions`, `Cloud cover/Sunlight intensity`, `Sound intensity`, `Heavy vehicles detection/Shock to street pole`, `Air Quality/Health`).
* `datasheet` - Reference to sensor's datasheet.

### Meaningful sensors
* `sid` - Unique identifier for the pair sensor-parameter.
* `param_name` - Name of the measured parameter.

### Provenance Metadata
The provenance metadata provides additional information about the origin of this
project digest. This file is a CSV with the following fields:

* `data_format_version` - Data format version.
* `project_id` - Project ID.
* `data_start_date` - Minimum possible publishing UTC timestamp.
* `data_end_date` - Maximum possible publishing UTC timestamp. If no explicit date exists, the creation date is used.
* `creation_date` - UTC timestamp this digest was created.
* `url` - URL where this digest was provided.

##Air Quality System (AQS)

### AQS data

* `site` - Twelve-character field based on the three-digit ISO country code plus the nine-digit monitor location code. The nine-digit monitor location code is the AQS site identifier. It consists of a two-digit state code, a three-digit county code, and a four-digit site number without spaces between the individual codes. For example, 123456789123 = 123 (country code) + 456789123 (nine-digit monitor location code). 
* `data_status` - Status of the data. 0 = Preliminary 1 = Final. Data are denoted as final after the agency collecting and reporting the data certifies that they meet quality assurance requirements and are complete and correct in AQS. This is only required for criteria pollutants reported from state agencies. 
* `action_code` - Action code for data ingesting. Not relevant to data obtained from QAD. Always blank. 
* `datetime` - Date and time of the data value, given in the following format without spaces: YYYYMMDDThhmmTZD. Four-digit year (YYYY), two-digit month (MM), two-digit day (DD), capital letter T (time), two-digit hour (hh, in 24-hr time), two-digit minutes (mm), and the time zone designation, TZD. (The date-time field is the ISO 8601 Basic.) TZD is +hhmm/-hhmm from GMT. The time corresponds to the begin time of the sampling period. 
* `parameter` - Five-digit AQS code used to identify the parameter being monitored. 
* `duration` - Measurement (sampling) period in minutes.
* `frequency` - How often the measurement is repeated (minutes). If measurements are taken multiple times per day (i.e., hourly), it is blank; otherwise, minutes equivalent (e.g., every day = 1440, and every other day = 2880). 
* `value` - Data (sampled) value of the specified parameter.
* `unit` - Three-digit AQS code used to describe the units in the measurement of the specified parameter. 
* `qc` - The AIRNow code used to link to the quality control codes that describe the validity, invalidity, or questionable status of the measurement. 
* `poc` - The "parameter occurrence code" (POC). The POC is used to specify if more than one monitor is measuring the same parameter at the same site. For example, if there are two ozone monitors at a site, they would have different POCs. 
* `lat` - Latitude in decimal degrees of the monitor. Latitudes north of the Equator are positive and latitudes south of the Equator are negative. 
* `lon` - Longitude in decimal degrees of the monitor. Longitudes west of the Prime Meridian are negative and longitudes east of the Prime Meridian are positive. 
* `GISDatum` - The datum associated with the latitude and longitude measurements. 
* `elev` - Elevation of the monitor in meters above mean sea level (MSL). 
* `method_code` - Three-digit AQS code that identifies the method used to perform the measurement. 
* `mpc` - Measurement Performance Characteristic (MPC) is a performance measurement for the measurement taken. The only valid value for QAD responses is "MDL" meaning method (lower) detection limit. 
* `mpc_value` - The value for the mpc (MDL) in the same units as the sample. 
* `uncertainty` - Uncertainty needs to be in the same units as the specified parameter and is given using the 95% confidence level. 
* `qualifiers` - AQS qualifier code(s) separated by spaces. Qualifiers indicate whether the data have been flagged by the submitter and the reason the sample was so flagged. 

<https://aqs.epa.gov/aqsweb/documents/gui_query.html>