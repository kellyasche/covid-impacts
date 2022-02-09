library(tidyverse)

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


# Prep pop data -----------------------------------------------------------

pop <- read_csv("Data/Cases/Master-covid-cases-counties.csv") %>%
  select(countyfp, pop)

# Prep vaccination data ---------------------------------------------------

master <- read_csv("Data/Vaccinations/People Vaccinated, By County_tcm1148-513635.csv") %>%
  filter(County != "UNKNOWN/MISSING") %>%
  mutate(County = str_to_title(County),
         County = str_replace(County, "Lac Qui Parle", "Lac qui Parle"),
         County = str_replace(County, "Mcleod", "McLeod"),
         County = str_replace(County, "Lake Of The Woods", "Lake of the Woods")) %>%
  left_join(counties.regions, by = c("County" = "Name")) %>%
  left_join(pop, by = "countyfp")

write_csv(master, "Data/Vaccinations/Master-vaccinations-counties.csv")

names(master)
names(pop)
