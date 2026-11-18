#Age-length keys
#Sex and origin specific
#To assign ages to Wild fish (age unknown) and to hatchery unassigned fish (PBT age unknown)

table(age$PhenotypicSex, age$Age)

#few fish are 1.x, most are 0.x

#create pooled FL consistent with expansions script

FL_seq <- seq(20, max(indiv$FL, na.rm = T) + 50, by = 50)

#check on methodology for length binning and jack determination
age <- age %>%
  mutate(FLmm = FL*10) %>% #for consistency
  mutate(pool_FL = cut(FLmm, breaks = FL_seq, right = FALSE)) %>%
  mutate(pool_FL = substr(pool_FL, 2, 4))

#all AI fish
table(age$Clip)

male_age <- age %>%
  filter(PhenotypicSex == "M") %>%
  unique(Age)

male_key <- age %>%
  filter(PhenotypicSex == "M") %>%
  group_by(Age, pool_FL) %>%
  count() %>% 
  ungroup() %>%
  complete(Age, pool_FL, fill = list(n = 0)) %>%
  group_by(pool_FL) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  select(Age, pool_FL, prop) %>%
  pivot_wider(names_from = Age, values_from = prop) 
  
female_key <- age %>%
  filter(PhenotypicSex == "F") %>%
  group_by(Age, pool_FL) %>%
  count() %>% 
  ungroup() %>%
  complete(Age, pool_FL, fill = list(n = 0)) %>%
  group_by(pool_FL) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  select(Age, pool_FL, prop) %>%
  pivot_wider(names_from = Age, values_from = prop) 

#Male, ad-clipped, assigned 
male_hat_key <- indiv %>%
  filter(Sex == "M", 
         Clip == "AD", 
         PopPBT != "Unassigned") %>%
  group_by(pool_FL, agePBT) %>%
  count() %>%
  ungroup() %>%
  complete(agePBT, pool_FL, fill = list(n = 0)) %>%
  group_by(pool_FL) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  select(agePBT, pool_FL, prop) %>%
  pivot_wider(names_from = agePBT, values_from = prop)

#female, ad-clipped, assigned
female_hat_key <- indiv %>%
  filter(Sex == "F", 
         Clip == "AD", 
         PopPBT != "Unassigned") %>%
  group_by(pool_FL, agePBT) %>%
  count() %>%
  ungroup() %>%
  complete(agePBT, pool_FL, fill = list(n = 0)) %>%
  group_by(pool_FL) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  select(agePBT, pool_FL, prop) %>%
  pivot_wider(names_from = agePBT, values_from = prop)

  