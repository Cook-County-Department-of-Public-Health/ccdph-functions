# Header -----------------------------------------------------------

# Script name: spatial-database-functions.R
# Purpose: write spatial data to CCDPH SQL Server inter-spatial database
# Author: C. Scott Smith (christopher.smith@cookcountyhealth.org)
# Created: 5/15/2023 
# Last Updated: 5/17/2023

# Attach R packages -----------------------------------------------------

library(sf)
library(DBI)
library(odbc)
library(dplyr)
library(tidyverse)
library(spData)
library(keyring)
library(tigris)
library(units)

#Name of CCDPH SQL Server is encrypted. Set name using keyring's key_set function.
#key_set("ccdph_sql_server")

fx_spatial_layer_to_database <- function(sf_layer_name, schema_name, db_table_name, crs_id){
  
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
  
  return(sf_layer_db)
}