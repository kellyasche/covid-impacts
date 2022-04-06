library(tidyverse)



# Prep counties and regions -----------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  rename(mif = `MIF Region`) %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc),
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"),
         mif = ifelse(is.na(mif), "TC", mif),
         mif = as.factor(mif),
         mif = fct_relevel(mif, "NW", "NE", "WC", "EC", "SW", "SE", "TC"))


# Prep laborforce by gender -----------------------------------------------

lf.gender <- read_csv("Data/Workforce/Gender/labor-force-gender.csv") %>%
  filter(geography != "27",
         sex != 0) %>%
  mutate(geography = str_sub(geography, -3, -1)) %>%
  rename(countyfp = geography) %>%
  group_by(countyfp, sex, year) %>%
  summarize(Emp = round(mean(Emp))) %>%
  ungroup() %>%
  left_join(counties.regions,by = "countyfp")

write_csv(lf.gender, "Data/Workforce/Gender/Master-lf-gender-pr.csv")

names(lf.gender)
