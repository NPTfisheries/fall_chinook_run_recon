# Fall Chinook run reconstruction

#packages
library(tidyverse)

#read in data

indiv <- read.csv("data_inputs/2023/individual_data_lgr_23.csv")
trap_rate <- read.csv("data_inputs/2023/lgr_trap_rate_23.csv")
pbt_tag_rate <- read.csv("data_inputs/2023/pbt_tag_rate_23.csv")
#age <- read.csv("data_inputs/2025/scale_ages.csv")
#lgr_window <- read.csv("data_inputs/lgr_window_counts.csv")
#release group info stays the same for all years, just added to with new releases
release_group <- read.csv("data_inputs/2023/release_group_info.csv")

run_yr <- 2023

#format data

#individual data
indiv <- indiv %>%
  rename(FL = FL.mm, 
         PopPBT = PopName, 
         RelSite = Final.group,
         RelLGR = rel.abv.blw.LGR) %>%
  mutate(DateSampled = as.Date(DateSampled, format = "%m/%d/%Y")) %>%
  drop_na(DateSampled) %>% #drop NAs from bottom of df
  mutate(RelSite = if_else(is.na(RelSite), "Unassigned", RelSite)) %>%
  mutate(Sex = if_else(GenSex == "U", PhenotypicSex, GenSex)) %>%
  mutate(count = 1)


#LGR trap rate
# trap_rate <- trap_rate %>%
#   rename(TrapRate = Trap.Rate,
#          EffTR = effective.TR, 
#          StartDate = Start.date, 
#          n_geno = X..genotyped, 
#          n_trap = X..trapped, 
#          geno_rate = genotype.rate) %>%
#   mutate(StartDate = as.Date(StartDate, format = "%d-%b")) %>%
#   drop_na(TrapRate)

#adjust trap rate formatting
trap_rate <- trap_rate %>%
  rename(TrapRate = Trap.Rate,
         StartDate = Start.date, 
         geno_rate = genotype.rate) %>%
  mutate(StartDate = as.Date(StartDate, format = "%d-%b")) %>%
  drop_na(TrapRate)

year(trap_rate$StartDate) <- run_yr

#no raw age data for 2023

#scale age
# age <- age %>%
#   rename(FL = Length) %>%
#   mutate(DateSampled = as.Date(DateSampled, format = "%d-%b-%y"))

#lgr_window

# lgr_window <- lgr_window %>%
#   rename(WindowCount = DART.window.counts, 
#          NightExp = night.passage.expansion, 
#          TotChin = total.Chinook.in.sample, 
#          SumChin = X..summer.Chnk.in.sample) %>%
#   mutate(SumChin = if_else(SumChin == "U", NA, SumChin), 
#          SumChin = na_if(SumChin, "")) %>%
#   mutate(SumChin = as.numeric(SumChin))

#pbt tag rate
pbt_tag_rate <- pbt_tag_rate %>%
  rename(PopPBT = Pedigree, 
         PbtTagRate = Tag.Rate)

#fallback and reascension rate
#make sure "Abv" and "Blw" match the indiv dataset
fallback <- data.frame(cbind(jack = rep(c("adult", "jack"), 2), 
                  RelLGR = c(rep("Abv", 2), rep("Blw", 2)), 
                  fallback_rate = c(0.025, 0.047, 0.249, 0.292)))

release_group <- release_group %>%
  rename(tagcode = TAGCODE.as.text, 
         release_year = Release.Year, 
         rearing = Age, 
         brood_year = Brood.Year, 
         RelSite = Release.site, 
         adult_origin = adult.origin) %>%
  mutate(rearing = if_else(rearing == "Yearling", "yearling", rearing)) %>%
  select(tagcode, release_year, brood_year, rearing, RelSite, adult_origin) %>%
  drop_na(tagcode)

SR_hatchery <- c("Lyons Ferry", "Nez Perce")
