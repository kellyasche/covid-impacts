library(tidyverse)



# Prep workforce objects ----------------------------------------------------------------------------------------------------
laborforce.pr <- read_csv("Data/Workforce/Mth_LabForce_NSA.csv") %>%
  gather(key = "planning.region", value = "labor.force", 2:7) %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = str_replace(planning.region, " - MN", ""))

write_csv(laborforce.pr, "Data/Workforce/Master-laborforce-pr.csv")

laborforce.annual.pr <- read_csv("Data/Workforce/Ann_LabForce_NSA.csv") %>%
  gather(key = "planning.region", value = "labor.force", 2:7) %>%
  rename(year = 1) %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = str_replace(planning.region, " - MN", ""),
         year = as.integer(str_sub(year, 1, 4)))

write_csv(laborforce.annual.pr, "Data/Workforce/Master-annual-labor-force-pr.csv")

laborforce.annual.edr <- read_csv("Data/Workforce/Ann_LabForce_NSA_EDR.csv") %>%
  gather(key = "EDR", value = "labor.force", 2:14) %>%
  rename(year = 1) %>%
  mutate(EDR = str_replace(EDR, "  ", " "),
         year = as.integer(str_sub(year, 1, 4)))

write_csv(laborforce.annual.edr, "Data/Workforce/Master-annual-labor-force-edr.csv")

names(laborforce.annual.edr)
