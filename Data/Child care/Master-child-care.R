library(tidyverse)
library(readxl)


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



# Prep child care data ----------------------------------------------------

est.need <- read_xlsx("Data/Child care/_Child Care Count and Capacity by County 2000-2020.xlsx", sheet = 1) %>%
  rename(county = 1,
         parents.lf.num = 2,
         parents.lf.pct = 3) %>%
  mutate(county = str_sub(county, 1, -18),
         county = trimws(county, "both")) %>%
  left_join(counties.regions, by = c("county" = "Name")) %>%
  select(1,2,6,8:11)

ccc.cap <- read_xlsx("Data/Child care/_Child Care Count and Capacity by County 2000-2020.xlsx", sheet = 4) %>%
  rename(county = 1) %>%
  select(1:22, 25:27) %>%
  gather(key = "year", value = "ccc.capacity", 2:22) %>%
  mutate(year = str_sub(year, - 2, -1),
         year = paste("20", year, sep = ""),
         year = as.numeric(year)) %>%
  left_join(counties.regions[,c(1,2)], by = c("county" = "Name"))

fcc.cap <- read_xlsx("Data/Child care/_Child Care Count and Capacity by County 2000-2020.xlsx", sheet = 6) %>%
  rename(county = 1) %>%
  drop_na(county) %>%
  select(1:22, 25:27) %>%
  filter(county != "Minnesota") %>%
  gather(key = "year", value = "fcc.capacity", 2:22) %>%
  mutate(year = str_sub(year, -2, -1),
         year = paste("20", year, sep = ""),
         year = as.numeric(year),
         county = str_replace(county, "Lac Qui Parle", "Lac qui Parle")) %>%
  left_join(counties.regions[,c(1,2)], by = c("county" = "Name"))


master <- est.need %>%
  left_join(ccc.cap[,c(7, 5, 6)], by = c("countyfp")) %>%
  drop_na(year) %>%
  left_join(fcc.cap[,c(7, 5, 6)], by = c("countyfp", "year"))

write_csv(master, "Data/Child care/Master-child-care.csv")

names(ccc.cap)
names(est.need)
names(fcc.cap)
