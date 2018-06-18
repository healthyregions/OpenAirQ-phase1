# Postgresql

Documentation : <https://www.postgresql.org/docs/9.1/static/tutorial-accessdb.html>

* `createdb mydb`: create database
* `dropdb mydb`: drop database
* `brew services stop postgresql`: stop postgresql server
* `brew services start postgresql`: start postgresql server
* `brew services restart postgresql`: restart postgresql server
* `psql mydb` : access to a database


`mydb=#` if I am a database superuser (not subject to access control) or `mydb=>`if I am a regular user.

###Internal commands
psql has a number of internal commands, beginning with `\`.

* `\h`: help on the syntax of PostgreSQL SQL commands 
* `\q`: quit psql and return to the command shell (get out of the database)
* `\l+`: list of existing databases
* `\c`: connect to a database (also use to switch to another one)
* `\dt` : list of existing tables for the database
* `\?`: list of internal commands


###SQL commands

* `CREATE database mydb;`: create a database
* `CREATE EXTENSION IF NOT EXISTS mydb CASCADE;`: extend the database with TimescaleDB

###Terminal

* `find /usr/local/ -name "*file_name*"`: find all the files containing "file_name"

###Basic installations (if we start from scratch)

1. Download Homebrew and Xcode
2. `brew install python`
3. `pip install numpy`
4. `brew install postgis`



