
## ---------------------------
## Script name: Use reticulate
##
## Purpose of script: 
## Familiar with reticulate and rdataretriever packages, 
## as well as PostgreSQL and SQLite, write a short code 
## and save as a file named ”r_database.R”, illustrating 
## how to upload the data of Doubs, a built-in dataset 
## of ade4 package into a schema of PostgreSQL or the SQLite.
##
## Author: Weishan Tu 
##
## Date Created: 2024-04-07
##
## Copyright (c) Timothy Farewell, 
## Email: weishan@mail.ustc.edu.cn
## ---------------------------

cat("\014") #clears rhe console
rm(list=ls()) #remove all variales
##install 
# install.packages('reticulate') # Install R package for interacting with Python
# reticulate::install_miniconda() # Install Python
# reticulate::py_install('retriever') # Install the Python retriever package
# install.packages('rdataretriever') # Install the R package for running the retriever
# rdataretriever::get_updates() # Update the available datasets
# install.packages('ade4') # Install the R package for ade4

## load up the packages we will need:
library(reticulate)
library(rdataretriever)
py_config()
###Use reticulate
# Importing Python modules
os <- import("os")
os$listdir(".")
retriever <- import("retriever")
# Installing Python package
py_install("retriever")

# Converting between R and Python
# [convert = TRUE] => convert Python objects to R when appropriate
sys <- import("sys", convert = TRUE)
class(sys$path)
# [convert = FALSE] => always return Python objects
sys <- import("sys", convert = FALSE)
class(sys$path)

###Use rdataretriever
# py_discover_config('retriever')
# List the datasets available via the Retriever
rdataretriever::datasets()
# Install the portal into csv files in your working directory
rdataretriever::install_csv('portal')

# Download the raw portal dataset files without any processing to the
# subdirectory named data
rdataretriever::download('portal', './data/')

# Install and load a dataset as a list
portal = rdataretriever::fetch('portal')
names(portal)
head(portal$species)

# Installing Spatial Datasets
rdataretriever::install_postgres('harvard-forest') # Vector data
rdataretriever::install_postgres('bioclim') # Raster data

# Install only the data of USGS elevation in the given extent
rdataretriever::install_postgres('usgs-elevation', list(-94.98704597353938, 39.027001800158615, -94.3599408119917, 40.69577051867074))

library(DBI)
# Load the RSQLite Library
library(RSQLite)
# load doubs
library(ade4)
data(doubs)
doubs_species <- doubs$species
# Create a connection to our new database, DoubsDB.db
# you can check that the .db file has been created on your working directory
conn <- dbConnect(RSQLite::SQLite(), "DoubsDB.db")

# Write the doubs species table into a table names doubs_species
dbWriteTable(conn, "doubs_species", doubs_species)
# List all the tables available in the database
dbListTables(conn)

####test PostgreSQL
# Load the RPostgreSQL Library
# install.packages('RPostgres')
library(DBI)
library(RPostgreSQL)
library(RPostgres)
doubs_species <- doubs$species
# Connect to the default postgres database
con <- dbConnect(RPostgres::Postgres())
dbListTables(con)
##Save RPostgres database
dbWriteTable(con, "doubs_species", doubs_species)

##Read 
dbListFields(con, "doubs_species")

# Disconnect from the database
dbDisconnect(con)
dbReadTable(con, "doubs_species")
