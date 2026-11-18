#Fall Chinook run reconstruction

#packages
library(tidyverse)

#from run reconstruction report
run_start <- as.Date("08/18/2025", format = "%m/%d/%Y")
run_end <- as.Date("11/15/2025", format = "%m/%d/%Y")
run_yr <- 2025

#trapping period for each trap rate at LGR
trap_rate <- trap_rate %>%
  arrange(StartDate)

#add LGR effective trap rate and genotyping rate to individual data
indiv <- indiv %>%
  mutate(TrapRate = trap_rate$TrapRate[findInterval(DateSampled, trap_rate$StartDate)]) %>%
  mutate(GenRate = trap_rate$geno_rate[findInterval(DateSampled, trap_rate$StartDate)]) %>%
  left_join(pbt_tag_rate, by = "PopPBT") 

#group by 50 mm FL, expand by the rate of failed genotyping
#this step happens first, because we don't know which group they would have assigned to (or been unassigned)
ggplot(indiv, aes(x = FL, group = Sex)) + 
  geom_histogram() + 
  facet_wrap(~Sex)

#define specific FL bins to include the 570mm cutoff for jacks
FL_seq <- seq(20, max(indiv$FL, na.rm = T) + 50, by = 50)

#check on methodology for length binning and jack determination
indiv <- indiv %>%
  mutate(pool_FL = cut(FL, breaks = FL_seq, right = FALSE)) %>%
  mutate(pool_FL = str_extract(pool_FL, "(?<=,)[^)\\]]+"))
  mutate(pool_FL = substr(pool_FL, 2, 4)) %>%
  mutate(count = 1)

#group by pooled FL, PopPBT, age, 

#expand by LGR trap rate
lgr_expand <- indiv %>%
  mutate(lgr_expand = count/TrapRate)

#number and proportion of fish that didn't genotype by FL  
ng_prop <- lgr_expand %>%
  group_by(pool_FL, Sex, TrapRate, RelSite) %>%
  summarise(count = sum(lgr_expand)) %>%
  #group_by(pool_FL) %>%
  mutate(ng_prop = sum(count[RelSite == "NG"])/sum(count)) %>%
  mutate(gen_prop = 1 - ng_prop) %>%
  ungroup() %>%
  select(pool_FL, Sex, TrapRate, gen_prop) %>%
  distinct()

#expand by failed genotype rate, grouping by FL, Sex and LGR Trap Rate
#makes very small change to dataset
ng_expand <- lgr_expand %>%
  full_join(ng_prop, by = c("pool_FL", "Sex", "TrapRate")) %>%
  mutate(ng_expand = lgr_expand/gen_prop) %>%
  mutate(ng_expand = if_else(RelSite == "NG", 0, ng_expand)) #remove the counts of the NG fish

#expand by PBT tagging rate
#for each fish added to an assigned hatchery group through the tag rate expansion, a fish needs to be removed from an unassigned group
#the subtractions are proportional based on hatchery unassigned (AD) and unassigned (AI) 
pbt_expand_table <- ng_expand %>%
  mutate(PbtTagRate = if_else(PopPBT == "Unassigned", 1, PbtTagRate)) %>%
  mutate(pbt_expand = ng_expand/PbtTagRate) %>%
  group_by(Clip, PopPBT) %>%
  summarise(pbt_expand = sum(pbt_expand), ng_expand = sum(ng_expand), diff = ng_expand - pbt_expand) %>%
  group_by(Clip) %>%
  mutate(exp_sum = sum(diff, na.rm = T), 
         diff = if_else(PopPBT == "Unassigned", exp_sum*-1, diff)) %>%
  select(-exp_sum) %>% 
  ungroup() %>%
  mutate(prop_exp = diff/ng_expand)

pbt_expand <- ng_expand %>%
  left_join(pbt_expand_table %>%
              select(Clip, PopPBT, prop_exp), by = c("PopPBT", "Clip")) %>%
  mutate(pbt_expand = ng_expand - prop_exp)

#expand by the Genotype sample rate
gen_expand <- pbt_expand %>%
  mutate(gen_expand = pbt_expand/GenRate)

#apply fallback and reascension rate
#fallback rates are different for releases above and below LGR, for jacks and adults
#assume that fallback rate is 0 for wild (unassigned, ad intact) fish
fallback_accounting <- gen_expand %>%
  mutate(jack = if_else(pool_FL < 570, "jack", "adult")) %>%
  mutate(origin = if_else(PopPBT == "Unassigned" & Clip == "AI", "wild", 
                          if_else(PopPBT == "Unassigned" & Clip == "AD", "hatchery_unassigned", "hatchery"))) %>%
  left_join(fallback, by = c("jack", "RelLGR")) %>%
  mutate(fallback_rate = as.numeric(fallback_rate)) %>%
  #fallback is only applied to assigned hatchery fish
  #assumed 0 for wild and unassigned hatchery fish
  mutate(fallback_rate = if_else(origin != "hatchery", 0, fallback_rate)) %>%
  mutate(fallback_account = gen_expand*(1-fallback_rate))

fallback_accounting %>% 
  group_by(RelSite, PopPBT) %>%
  summarise(est = sum(fallback_account))

fallback_accounting %>%
  filter(origin == "hatchery") %>%
  group_by(Sex, jack) %>%
  summarise(est = sum(gen_expand, na.rm = T))


#apply ages for wild fish
wild_female_age <- fallback_accounting %>% 
  filter(Sex == "F") %>%
  filter(origin == "wild") %>%
  group_by(pool_FL) %>%
  summarise(sum = sum(fallback_account)) %>%
  left_join(female_key, by = "pool_FL") %>%
  pivot_longer(cols = -c("pool_FL", "sum"), names_to = "scale_age", values_to = "prop") %>%
  mutate(count_by_age = prop * sum) %>%
  select(-sum, -prop) %>%
  mutate(scale_age = if_else(scale_age == "1", "1.0", scale_age)) %>%
  mutate(age = as.numeric(substr(scale_age, 1, 1)) + as.numeric(substr(scale_age, 3, 3)) + 1) %>%
  mutate(rearing = if_else(substr(scale_age, 1, 1) == 0, "subyearling", "yearling")) %>%
  mutate(Sex = if_else(pool_FL < 570, "jack", "F"))
  
wild_male_age <- fallback_accounting %>% 
  filter(Sex == "M") %>%
  filter(origin == "wild") %>%
  group_by(pool_FL) %>%
  summarise(sum = sum(fallback_account)) %>%
  left_join(male_key, by = "pool_FL") %>%
  pivot_longer(cols = -c("pool_FL", "sum"), names_to = "scale_age", values_to = "prop") %>%
  mutate(count_by_age = prop * sum) %>%
  select(-sum, -prop) %>%
  mutate(scale_age = if_else(scale_age == "1", "1.0", scale_age)) %>%
  mutate(age = as.numeric(substr(scale_age, 1, 1)) + as.numeric(substr(scale_age, 3, 3)) + 1) %>%
  mutate(rearing = if_else(substr(scale_age, 1, 1) == 0, "subyearling", "yearling")) %>%
  mutate(Sex = if_else(pool_FL < 570, "jack", "M"))

#apply ages for hatchery unassigned fish
#assume that all hatchery unassigned are subyearling release
hat_unassign_female_age <- fallback_accounting %>%
  filter(Sex == "F") %>%
  filter(origin == "hatchery_unassigned") %>%
  group_by(pool_FL) %>%
  summarise(sum = sum(fallback_account)) %>%
  left_join(female_hat_key, by = "pool_FL") %>%
  pivot_longer(cols = -c("pool_FL", "sum"), names_to = "age", values_to = "prop") %>%
  mutate(count_by_age = prop*sum) %>%
  select(-sum, -prop) %>%
  mutate(rearing = "subyearling") %>%
  mutate(Sex = if_else(pool_FL < 570, "jack", "F"))
  
hat_unassign_male_age <- fallback_accounting %>%
  filter(Sex == "M") %>%
  filter(origin == "hatchery_unassigned") %>%
  group_by(pool_FL) %>%
  summarise(sum = sum(fallback_account)) %>%
  left_join(male_hat_key, by = "pool_FL") %>%
  pivot_longer(cols = -c("pool_FL", "sum"), names_to = "age", values_to = "prop") %>%
  mutate(count_by_age = prop*sum) %>%
  select(-sum, -prop) %>%
  mutate(rearing = "subyearling") %>%
  mutate(Sex = if_else(pool_FL < 570, "jack", "M"))

hatchery_assign <- fallback_accounting %>%
  filter(origin == "hatchery") %>%
  mutate(brood_year = run_yr - agePBT) %>%
  left_join(release_group, by = c("RelSite", "brood_year")) %>%
  mutate(Sex = if_else(jack == "jack", "jack", Sex)) %>%
  mutate(program = substr(PopPBT, 4, 7)) %>%
  group_by(agePBT, brood_year, rearing, RelSite, adult_origin, Sex, program) %>%
  summarise(total = sum(fallback_account)) %>%
  pivot_wider(names_from = Sex, values_from = total, values_fill = 0) %>%
  arrange(brood_year, RelSite) 

#format the stray hatcheries and LFYO which was missing from release group file
 hatchery_assign <- hatchery_assign %>%
   mutate(program = if_else(program == "PBT2", "stray", program)) %>%
   mutate(adult_origin = if_else(program == "stray", "stray", adult_origin)) %>%
   mutate(rearing = if_else(program == "stray", "subyearling", rearing)) %>%
   mutate(rearing = if_else(brood_year == 2023 & RelSite == "LFYO", "Yearling", rearing), 
          adult_origin = if_else(brood_year == 2023 & RelSite == "LFYO", "SR hatchery", adult_origin)) %>%
   rename(age = agePBT)
   
  
 #format hatchery unassigned
hatchery_unassigned <- rbind(hat_unassign_female_age, hat_unassign_male_age) %>%
  group_by(Sex, age, rearing) %>%
  mutate(age = as.numeric(age)) %>%
  summarise(total = sum(count_by_age, na.rm = T)) %>%
  mutate(brood_year = run_yr - age, 
         RelSite = "hatchery_unk", 
         adult_origin = "SR hatchery", 
         program = "unknown") %>%
  pivot_wider(names_from = Sex, values_from = total, values_fill = 0)


wild <- rbind(wild_female_age, wild_male_age) %>%
  group_by(Sex, age, rearing) %>%
  mutate(age = as.numeric(age)) %>%
  summarise(total = sum(count_by_age, na.rm = T)) %>%
  mutate(brood_year = run_yr - age, 
         RelSite = "wild", 
         adult_origin = "wild", 
         program = "natural") %>%
  pivot_wider(names_from = Sex, values_from = total, values_fill = 0) %>%
  arrange(brood_year)
  
  
  