#### Required packages ####

library(sf)
library(dplyr)
library(tidyverse)
library(spData)
library(keyring)

# set key for connecting to CCDPH SQL Server instance
#key_set("ccdph_sql_server")

## process, add spatial data to inter-spatial database on CCDPH SQL Server
fx_spatial_data_output <- function(sf_spatialdataset, layer_name, crs_id = 3435){
  
  # connect to inter-spatial
  con_inter_spatial <- dbConnect(odbc::odbc(),
                                 Driver   = "SQL Server",
                                 Server   = key_get("ccdph_sql_server"),
                                 Database = "inter-spatial")
  
  sf_spatialdataset_df <- as.data.frame(sf_spatialdataset) # convert to data frame
  sf_spatialdataset_geom <- sf_spatialdataset_df$geometry # isolate geometry column
  sf_spatialdataset_df[,"geom"]<-st_as_text(st_transform(sf_spatialdataset_geom,crs = crs_id)) # convert geometry format
  
  # write data frame to database
  dbWriteTable(conn = con_inter_spatial, 
               Id(schema="dbo", table=layer_name), 
               overwrite= TRUE,
               sf_spatialdataset_df %>% select(-geometry))
  
  # add new geometry column to database table
  dbSendQuery(conn = con_inter_spatial, 
              statement = paste0("ALTER TABLE dbo.",layer_name," ADD geom2 geometry"))
  
  # transform and copy existing text-based geometry into new geometry column
  dbSendQuery(conn = con_inter_spatial,
              statement = paste0("UPDATE dbo.",layer_name," Set geom2 = geometry::STGeomFromText(geom,", as.character(crs_id),")"))
  
  # drop text-based geometry column
  dbSendQuery(conn = con_inter_spatial, 
              statement = paste0("ALTER TABLE dbo.",layer_name," DROP COLUMN geom"))
  
  # read table
  sf_spatialdataset_db<-st_read(con_inter_spatial, 
                                query = paste0("SELECT NAME, geom2.STAsBinary() AS geom FROM dbo.",layer_name))
  
return(sf_spatialdataset_db)
}


