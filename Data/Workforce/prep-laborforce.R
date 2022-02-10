library(tidyverse)



# Prep workforce objects ----------------------------------------------------------------------------------------------------
laborforce.pr <- read_csv("Data/Workforce/Mth_LabForce_NSA.csv") %>%
  gather(key = "planning.region", value = "labor.force", 2:7) %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = str_replace(planning.region, " - MN", ""))

write_csv(laborforce.pr, "Data/Workforce/Master-laborforce-pr.csv")
