CREATE database aot;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;

CREATE TABLE       aqs_data     (
       site_id    	TEXT        	NOT NULL,
       dat_status 	TEXT,
       act_code   	TEXT,
       datetime   	TIMESTAMP		NOT NULL,
       param_code 	TEXT        	NOT NULL,
       dur_code   	TEXT,
       freq       	TEXT,
       value      	TEXT,
       unit       	TEXT,
       qc         	TEXT,
       poc        	TEXT,
       lat        	TEXT,
       lon        	TEXT,
       datum      	TEXT,
       elev       	TEXT,
       meth_code  	TEXT,
       mpc        	TEXT,
       mpc_value  	TEXT,
       uncert     	TEXT,
       qualif     	TEXT
);

SELECT create_hypertable('aqs_data', 'datetime', 'site_id');
