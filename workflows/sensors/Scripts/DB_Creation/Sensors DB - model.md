# Sensors DB - model

## Definitions

An index on a file speeds up selections on the search key fields for the index.

* Any subset of the fields of a relation can be the search key for an index on the relation.
* Search key is not the same as key (e.g., doesn’t have to be unique).



Time series data : <https://docs.timescale.com/v0.9/introduction/time-series-data>
TimeScale DB : <https://blog.timescale.com/time-series-data-why-and-how-to-use-a-relational-database-instead-of-nosql-d0cd6975e87c>

Time-series data is data that collectively represents how a system, process, or behavior changes over time.

* Time-centric: Data records always have a timestamp.
* Append-only: Data is almost solely append-only (INSERTs) compared to other data like standard relational "business" data that mostly overwrite (UPDATEs).
* Recent: New data is typically about recent time intervals, and we more rarely make updates or backfill missing data about old intervals.

Environmental monitoring, Monitoring Computer Systems, Financial Trading Systems, etc.

Use of "wide-table" data model for time-serie instead of "narrow-table" used in traditional relational data.
The wide-table data model used in TimeScale DB reflects the inherent structure in the data and looks exactly the same as the initial data stream.
TimescaleDB's data model also has another similarity with relational databases: it supports JOINs. Specifically, one can store additional metadata in a secondary table, and then utilize that data at query time.

TimescaleDB is implemented as an extension on PostgreSQL, which means that a Timescale database runs within an overall PostgreSQL instance. The extension model allows the database to take advantage of many of the attributes of PostgreSQL such as reliability, security, and connectivity to a wide range of third-party tools.

Chunks are created by partitioning the hypertable's data into one or multiple dimensions: All hypertables are partitioned by a time interval, and can additionally be partitioned by a key such as device ID, location, user id, etc. We sometimes refer to this as partitioning across "time and space".

The current open-source release of TimescaleDB only supports single-node deployments. Of note is that the single-node version of TimescaleDB has been benchmarked to over 10-billion-row hypertables on commodity machines without a loss in insert performance.

<https://arrayofthings.github.io/node.html>


## Commands used to create DB (local)

```
CREATE database aot;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;

CREATE TABLE aqs_data (
  site_id		TEXT	NOT NULL,
  dat_status		TEXT,
  act_code		TEXT,
  datetime	TIMESTAMP	NOT NULL,
  param_code		TEXT	NOT NULL,
  dur_code		TEXT,
  freq		TEXT,
  value		TEXT,
  unit		TEXT,
  qc		TEXT,
  poc		TEXT,
  lat		TEXT,
  lon		TEXT,
  datum		TEXT,
  elev		TEXT,
  meth_code		TEXT,
  mpc		TEXT,
  mpc_value		TEXT,
  uncert		TEXT,
  qualif		TEXT,
);

```



