library(tidyverse)
library(readxl)
library(janitor)


# Upload regions and zip ----------------------------------------------------------------------------------------------------

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

city.zip <- read_csv("Data/Join docs/city zip codes.csv")




# Looking at data -----------------------------------------------------------------------------------------------------------

cities <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", skip = 3) %>%
  slice(1:47) %>%
  select(1) %>%
  mutate(City = as.factor(City))

albert.lea <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 2) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Albert Lea") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

austin <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 3) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Austin") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

baxter <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 4) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Baxtor") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

bemidji <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 5) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Bemidji") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

brainerd <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 6) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Brainerd") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

clearwater <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 7) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Clearwater") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

cloquet <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 8) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Cloquet") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

detroit.lakes <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 9) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Detroit Lakes") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

duluth <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 10) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Duluth") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

east.grand.forks <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 11) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .1,
         city = "East Grand Forks") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

fairmont <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 12) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Fairmont") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

fergus.falls <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 13) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Fergus Falls") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

hermantown <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 15) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = ".05 - .1",
         city = "Hermantown") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

hutchinson <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 16) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Hutchinson") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

lanesboro <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 17) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Lanesboro") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

mankato <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 18) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Mankato") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

marshall <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 19) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Marshall") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

medford <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 20) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Medford") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

minneapolis <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 21) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Minneapolis") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

moose.lake <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 22) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Moose Lake") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

new.london <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 23) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "New London") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

new.ulm <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 24) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "New Ulm") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

north.mankato <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 25) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "North Mankato") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

owatonna <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 26) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Owatonna") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

proctor <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 27) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = ".05-.1",
         city = "Proctor") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

rochester <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 28) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = ".05-.075",
         city = "Rochester") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

spicer <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 29) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Spicer") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

st.cloud <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 30) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "St. Cloud Area") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

st.paul <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 31) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "St. Paul") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

two.harbors <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 32) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Two Harbors") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

walker <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 33) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .15,
         city = "Walker") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

willmar <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 34) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Willmar") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

worthington <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 35) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Worthington") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

avon <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 36) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Avon") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

blue.earth <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 37) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Blue Earth") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

cambridge <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 38) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Cambridge") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

elk.river <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 39) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Elk River") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

excelsior <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 40) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Excelsior") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

int.falls <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 41) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .1,
         city = "International Falls") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

rogers <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 42) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .025,
         city = "Rogers") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

glenwood <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 43) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Glenwood") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

perham <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 44) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Perham") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

virginia <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 45)  %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .1,
         city = "Virginia") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

west.st.paul <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 46) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "West St. Paul") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

sauk.centre <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 47) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Sauk Centre") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

scanlon <- read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 48) %>%
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Scanlon") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)

read_xlsx("Data/LOST/local_SUT_generals.xlsm", sheet = 49)
  mutate(years.effective = "2004-2020",
         sales.tax.rate = .05,
         city = "Baxtor") %>%
  slice(3:16) %>%
  row_to_names(1) %>%
  rename(years.effective = 19,
         sales.tax.rate = 20,
         city = 21)
  

# BRING ALL TOGETHER --------------------------------------------------------------------------------------------------------
levels(cities$City)
master.data <- albert.lea %>%
  rbind(austin, avon, baxter, bemidji, blue.earth, brainerd, cambridge, clearwater, cloquet, detroit.lakes, duluth, east.grand.forks, elk.river, excelsior, fairmont, fergus.falls, glenwood, hermantown, hutchinson, int.falls, lanesboro, mankato, marshall, medford, minneapolis, moose.lake, new.london, new.ulm, north.mankato, owatonna, perham, proctor, rochester, rogers, sauk.centre, scanlon, spicer, st.cloud, st.paul, two.harbors, virginia, walker, west.st.paul, willmar, worthington) %>%
  gather(key = "year", value = "local.sales.tax.revenue", 2:18) %>%
  left_join(city.zip, by = c("city" = "City")) %>%
  mutate(County = ifelse(city == "West St. Paul", "Dakota", County),
         County = ifelse(city == "St. Paul", "Ramsey", County),
         County = ifelse(city == "St. Cloud Area", "Stearns", County),
         County = ifelse(city == "Scanlon", "Carlton", County),
         County = ifelse(city == "Proctor", "St. Louis", County),
         County = ifelse(city == "North Mankato", "Nicollet", County),
         County = ifelse(city == "Hermantown", "St. Louis", County),
         city = ifelse(city == "Baxtor", "Baxter", city),
         County = ifelse(city == "Baxter", "Crow Wing", County),
         County = ifelse(city == "Clearwater", "Wright", County),
         County = str_replace(County, "Saint Louis", "St. Louis")) %>%
  select(1:6, 8) %>%
  left_join(counties.regions, by = c("County" = "Name")) %>%
  group_by(city, year, local.sales.tax.revenue) %>%
  distinct() %>%
  ungroup()
  

write_csv(master.data, "Data/LOST/Master-local-sales-tax-revenue-cities-2004-2020.csv")
    
names(master.data)

levels(cities$City)

names(albert.lea)
