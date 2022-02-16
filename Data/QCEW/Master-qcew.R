library(tidyverse)



# Prep counties and regions -----------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  rename(mif = `MIF Region`) %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc) ,
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"),
         mif = ifelse(is.na(mif), "TC", mif),
         mif = as.factor(mif),
         mif = fct_relevel(mif, "NW", "NE", "WC", "EC", "SW", "SE", "TC"))

regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique() %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))



# Prep qcew county --------------------------------------------------------

qcew.county <- read_csv("Data/QCEW/qcew-county.csv") %>%
  select(1, area, 12:23) %>%
  gather(key = "key", value = "value", 3:14) %>% 
  drop_na() %>%
  mutate(quarter = str_sub(key, -1, -1),
         key = str_sub(key, 1, -2),
         key = as_factor(key),
         area = str_sub(area, -3, -1),
         year.quarter = paste(periodyear, ".", quarter, sep = ""),
         year.quarter = as.numeric(year.quarter)) %>%
  rename(countyfp = area) %>%
  left_join(counties.regions, by = "countyfp") %>%
  select(-periodyear, -Dem_RUCA)

write_csv(qcew.county, "Data/QCEW/Master-qcew-county.csv")

qcew.ruca <- qcew.county %>%
  group_by(Dem_Desc, year.quarter, key) %>%
  summarize(value = sum(value)) %>%
  ungroup()

write_csv(qcew.ruca, "Data/QCEW/Master-qcew-ruca.csv")

names(qcew.pr)

qcew.pr <- read_csv("Data/QCEW/qcew-pr.csv") %>%
  select(1, areaname, 12:23) %>%
  gather(key = "key", value = "value", 3:14) %>% 
  drop_na() %>%
  mutate(quarter = str_sub(key, -1, -1),
         key = str_sub(key, 1, -2),
         key = as_factor(key),
         year.quarter = paste(periodyear, ".", quarter, sep = ""),
         year.quarter = as.numeric(year.quarter),
         areaname = str_replace(areaname, " Minnesota", ""),
         areaname = str_replace(areaname, ", MN", "")) %>%
  rename(planning.region = areaname)

write_csv(qcew.pr, "Data/QCEW/Master-qcew-pr.csv")

qcew.edr <- read_csv("Data/QCEW/qcew-edr.csv") %>%
  select(1, areaname, 12:23) %>%
  gather(key = "key", value = "value", 3:14) %>% 
  drop_na() %>%
  mutate(quarter = str_sub(key, -1, -1),
         key = str_sub(key, 1, -2),
         key = as_factor(key),
         year.quarter = paste(periodyear, ".", quarter, sep = ""),
         year.quarter = as.numeric(year.quarter),
         areaname = str_replace(areaname, "  ", " ")) %>%
  rename(edr = areaname) %>%
  left_join(regions, by = "edr")

write_csv(qcew.edr, "Data/QCEW/Master-qcew-edr.csv")

