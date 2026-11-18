#Lower Granite trapping database


lgr_trap <- read.csv("data_inputs/LGTrappingDB_2026-10-27.csv")

head(lgr_trap)

table(lgr_trap$LGDSpecies)
table(lgr_trap$LGDSex)
table(lgr_trap$LGDRun)
table(lgr_trap$GenPBT_ByHat)
table(lgr_trap$GenPBT_RGroup)


#filter for fall chinook

fchin <- lgr_trap %>%
  filter(LGDSpecies == 1) %>%
  filter(LGDRun == 3)

#collection dates line up with fall chinook trapping
fchin %>%
  group_by(SpawnYear) %>% 
  summarise(min = min(CollectionDate), max = max(CollectionDate))

table(fchin$GenPBT_RGroup)
table(fchin$LGDMarkAD)
table(fchin$LGDTrapID)
table(fchin$BioScaleFinalAge)

#does not contain PBT information or a sampling ID that matches data provided by Bill

#does contain AD clip information, sex, FL, and trap rate
