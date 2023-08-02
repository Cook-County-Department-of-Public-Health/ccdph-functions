# Header -----------------------------------------------------------

# Script name: spatial-database-functions.R
# Purpose: read/write spatial data from/to CCDPH SQL Server inter-spatial database
# Author: C. Scott Smith (christopher.smith@cookcountyhealth.org)
# Created: 5/15/2023 
# Last Updated: 7/17/2023

# Attach R packages -----------------------------------------------------

library(sf)
library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(keyring)
library(tigris)

# Name of CCDPH SQL Server service is encrypted. Set name using keyring's key_set function.
# key_set("ccdph_sql_server")

# To access spatial database functions in external script use the following syntax.
# source("https://raw.githubusercontent.com/Cook-County-Department-of-Public-Health/ccdph-functions/master/spatial-database-functions.R")

# fx_read_spatial_layer_fr_database() parameters: 
# schema_name = name of schema spatial database table is assigned to (defaults to "ref")
# db_table_name = name of spatial table in inter-spatial database
# crs_id = coordinate reference system identification (defaults to 3435)
# Note that schema defaults to "ref" and crs defaults to 3435. 
# All data in inter-spatial are in crs 3435. Sending read function with crs_id=4326 
# automatically transforms the data into 4326 for leaflet mapping

# example:
# counties_illinois_sf <- fx_read_spatial_layer_fr_database(db_table_name = "counties_illinois", crs_id = 4326)

fx_read_spatial_layer_fr_database <- function(schema_name="ref", db_table_name, crs_id=3435){
  
  # connect to inter-spatial
  con_inter_spatial <- dbConnect(odbc::odbc(),
                                 Driver   = "SQL Server",
                                 Server   = key_get("ccdph_sql_server"),
                                 Database = "inter-spatial")
  
  # write data frame to database
  # default schema is ref
  sf_layer_geom <- tbl(
    src = con_inter_spatial,
    from = in_schema(schema_name,
                     db_table_name)) %>%
    mutate(geom = sql("geom.STAsBinary()")) %>%
    collect() %>%
    st_as_sf() %>%
    st_set_crs(3435)
    
  if(crs_id != 3435){
    sf_layer_geom <- st_transform(sf_layer_geom, crs=crs_id)
  }
  
  dbDisconnect(con_inter_spatial)
  
  return(sf_layer_geom)
}

# fx_write_spatial_layer_to_database() parameters: 
# sf_layer_name = simple feature file in R to write to database
# schema_name = name of schema new spatial database table is assigned to (defaults to "ref")
# db_table_name = name of new spatial table in inter-spatial database (default is to overwrite any existing table)
# crs_id = coordinate reference system identification (defaults to 3435)

# Write spatial data example
# Note that schema defaults to "ref" and crs defaults to 3435
# counties_il_db <- fx_write_spatial_layer_to_database(
#   sf_layer_name = counties_il,
#   schema_name = "ref",
#   db_table_name = "counties_illinois",
#   crs_id = 3435)

fx_write_spatial_layer_to_database <- function(sf_layer_name, schema_name="ref", db_table_name, crs_id=3435){
  
  # connect to inter-spatial
  con_inter_spatial <- dbConnect(odbc::odbc(),
                                 Driver   = "SQL Server",
                                 Server   = key_get("ccdph_sql_server"),
                                 Database = "inter-spatial")
  
  sf_layer_df <- as.data.frame(sf_layer_name) # convert to data frame
  sf_layer_geom <- sf_layer_df$geometry # isolate geometry column
  sf_layer_df[,"geom_text"]<-st_as_text(st_transform(sf_layer_geom,crs = crs_id)) # convert geometry format
  
  # write data frame to database
  dbWriteTable(conn = con_inter_spatial, 
               Id(schema=schema_name, 
                  table=db_table_name), 
               overwrite= TRUE,
               sf_layer_df %>% select(-geometry))
  
  # add new geometry column to database table
  dbSendQuery(conn = con_inter_spatial, 
              statement = paste0("ALTER TABLE ",schema_name,".",db_table_name," ADD geom geometry"))
  
  # transform and copy existing text-based geometry into new geometry column
  dbSendQuery(conn = con_inter_spatial,
              statement = paste0("UPDATE ", schema_name,".",db_table_name," Set geom = geometry::STGeomFromText(geom_text,", as.character(crs_id),")"))
  
  # drop text-based geometry column
  dbSendQuery(conn = con_inter_spatial, 
              statement = paste0("ALTER TABLE ", schema_name,".",db_table_name," DROP COLUMN geom_text"))
  
  # read table (geometry only)
  sf_layer_db<-st_read(con_inter_spatial, 
                       query = paste0("SELECT geom.STAsBinary() as geometry FROM ", schema_name,".",db_table_name))
  
  
  dbDisconnect(con_inter_spatial)
  
  return(sf_layer_db)
}