# -----------------------
# Author(s): Ryan Kinzer and Mike Ackerman
# Purpose: Open connection to the Lower Granite Dam trapping database (LGTrappingDB)
#   and save master table to a .csv
# 
# Created Date: Unknown
#   Last Modified: October 26, 2026
#
# NOTES: due to file size, the Lower Granite trapping database is saved to a .gitignore directory 
#   i.e., the user needs to download their own local copy from https://idahogov.sharepoint.com/sites/IDFG-InSeasonRunDocuments/Share/Forms/AllItems.aspx?id=%2Fsites%2FIDFG%2DInSeasonRunDocuments%2FShare&viewid=e5351068%2Df225%2D4e53%2Da3ad%2D80582d0743ab

# clear environment
rm(list = ls())

# load packages
library(tidyverse)

# requires a copy of the Lower Granite Dam trapping database and the ODBC driver
if(.Platform$OS.type != "unix") {                                       # includes windows machines
  
  trap_filepath = "C:/Users/michelleb/nezperce365/Research Scientists - General/LGTrapping Database/LGTrappingExport.accdb" # set path to LGTrappingDB Access database
  source("scripts/loadLGTrappingDBase.R")                                     # source loadLGTrappingDBase function
  con = loadLGTrappingDBase(trapDB_filepath = trap_filepath)            # connect to database
  trap_dbase = DBI::dbReadTable(con, 'tblLGDMasterCombineExport')       # read master table within LGTrappingDB
  DBI::dbDisconnect(con)                                                # disconnect
}

trap_dbase_filter <- trap_dbase %>%
  filter(SpawnYear %in% c("SY2023", "SY2024", "SY2025"))

# write database to .csv
write_csv(trap_dbase_filter, file = paste0("data_inputs/LGTrappingDB_", Sys.Date(), ".csv"))

# END SCRIPT