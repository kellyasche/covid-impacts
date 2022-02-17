library(tidyverse)
library(janitor)
library(readxl)


# Prep county regions -------------------------------------------------------------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc) ,
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))


# list of counties ----------------------------------------------------------------------------------------------------------

county.list <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 1, skip = 3) %>%
  slice(6:54) %>%
  select(1) %>%
  mutate(County = as.factor(County))

names(county.list)


# Year taxes were imposed ---------------------------------------------------------------------------------------------------

year.tax.imposed <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 1, skip = 3) %>%
  slice(6:54) %>%
  select(1,2,3) 

# Separate data for each county ---------------------------------------------------------------------------------------------

dodge <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 2) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Dodge Co") %>%
  left_join(year.tax.imposed, by = "County")

goodhue <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 3) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Goodhue Co") %>%
  left_join(year.tax.imposed, by = "County") 

sherburne <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 4) %>% 
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Sherburne Co") %>%
  left_join(year.tax.imposed, by = "County")

waseca <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 5) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Waseca Co") %>%
  left_join(year.tax.imposed, by = "County")

redwood <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 6) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Redwood Co") %>%
  left_join(year.tax.imposed, by = "County")

winona <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 7) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Winona Co") %>%
  left_join(year.tax.imposed, by = "County")

pine <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 8) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Pine Co") %>%
  left_join(year.tax.imposed, by = "County")

polk <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 9) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Polk Co") %>%
  left_join(year.tax.imposed, by = "County")

mille.lacs <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 10) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Mille Lacs Co") %>%
  left_join(year.tax.imposed, by = "County")

lake <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 11) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Lake Co") %>%
  left_join(year.tax.imposed, by = "County")

anoka <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 12) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Anoka Co") %>%
  left_join(year.tax.imposed, by = "County")

becker <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 13) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Becker Co") %>%
  left_join(year.tax.imposed, by = "County")

beltrami <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 14) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Beltrami Co") %>%
  left_join(year.tax.imposed, by = "County")

carlton <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 15) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Carlton Co") %>%
  left_join(year.tax.imposed, by = "County")

carver <-read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 16) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Carver Co") %>%
  left_join(year.tax.imposed, by = "County")

dakota <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 18) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Dakota Co") %>%
  left_join(year.tax.imposed, by = "County")

douglas <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 19) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Douglas Co") %>%
  left_join(year.tax.imposed, by = "County")

fillmore <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 20) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Fillmore Co") %>%
  left_join(year.tax.imposed, by = "County")

hubbard <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 22) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Hubbard Co") %>%
  left_join(year.tax.imposed, by = "County")

cook <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 24) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Cook Co") %>%
  left_join(year.tax.imposed, by = "County")

kandiyohi <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 25) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Kandiyohi Co") %>%
  left_join(year.tax.imposed, by = "County")

lyon <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 26) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Lyon Co") %>%
  left_join(year.tax.imposed, by = "County")

morrison <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 27) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Morrison Co") %>%
  left_join(year.tax.imposed, by = "County")

mower <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 28) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Mower Co") %>%
  left_join(year.tax.imposed, by = "County")

nicollet <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 29) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Nicollet Co") %>%
  left_join(year.tax.imposed, by = "County")

olmsted <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 30) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Olmsted Co") %>%
  left_join(year.tax.imposed, by = "County")

ramsey <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 31) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Ramsey Co") %>%
  left_join(year.tax.imposed, by = "County")

rice <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 32) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Rice Co") %>%
  left_join(year.tax.imposed, by = "County")

hennepin <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 33) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Hennepin Co") %>%
  left_join(year.tax.imposed, by = "County")

scott <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 35) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Scott Co") %>%
  left_join(year.tax.imposed, by = "County")

st.louis <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 36) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "St. Louis Co") %>%
  left_join(year.tax.imposed, by = "County")

stearns <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 37) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Stearns Co") %>%
  left_join(year.tax.imposed, by = "County")

steele <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 38) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Steele Co") %>%
  left_join(year.tax.imposed, by = "County")

todd <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 39) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Todd Co") %>%
  left_join(year.tax.imposed, by = "County")

wadena <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 40) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Wadena Co") %>%
  left_join(year.tax.imposed, by = "County")

washington <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 41) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Washington Co") %>%
  left_join(year.tax.imposed, by = "County")

wright <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 42) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Wright Co") %>%
  left_join(year.tax.imposed, by = "County")

freeborn <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 43) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Freeborn Co") %>%
  left_join(year.tax.imposed, by = "County")

blue.earch <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 45) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Blue Earth Co") %>%
  left_join(year.tax.imposed, by = "County")

brown <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 46) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Brown Co") %>%
  left_join(year.tax.imposed, by = "County")

cass <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 47) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Cass Co") %>%
  left_join(year.tax.imposed, by = "County")

chisago <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 48) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Chisago Co") %>%
  left_join(year.tax.imposed, by = "County")

crow.wing <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 49) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Crow Wing Co") %>%
  left_join(year.tax.imposed, by = "County")

otter.tail <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 50) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Otter Tail Co") %>%
  left_join(year.tax.imposed, by = "County")

wabasha <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 51) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Wabasha Co") %>%
  left_join(year.tax.imposed, by = "County")

isanti <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 52) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Isanti Co") %>%
  left_join(year.tax.imposed, by = "County")

benton <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 53) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Benton Co") %>%
  left_join(year.tax.imposed, by = "County")

kanabec <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 54) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "Kanabec Co") %>%
  left_join(year.tax.imposed, by = "County")

mcleod <- read_xlsx("Data/Sales Tax/local_transitSUT_counties.xlsm", sheet = 55) %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  mutate(County = "McLeod Co") %>%
  left_join(year.tax.imposed, by = "County")


# Combine all sheets --------------------------------------------------------------------------------------------------------
levels(county.list$County)
master <- anoka %>%
  rbind(becker, beltrami, benton, blue.earch, brown, carlton, carver, cass, chisago, cook, crow.wing, dakota, dodge, douglas, fillmore, freeborn, goodhue, hennepin, hubbard, isanti, kanabec, kandiyohi, lake, lyon, mcleod, mille.lacs, morrison, mower, nicollet, olmsted, otter.tail, pine, polk, ramsey, redwood, rice, scott, sherburne, st.louis, stearns, steele, todd, wabasha, wadena, waseca, washington, winona, wright) %>%
  mutate(County = as.factor(County),
         County = str_replace(County, " Co", "")) %>%
  gather(key = "year", value = "local.sales.tax.revenue", 2:18) %>%
  left_join(counties.regions, by = c("County" = "Name"))

write_csv(master, "Data/Sales tax/Master-local-sales-tax-revenue-counties-2004-2020.csv")

  names(master)

