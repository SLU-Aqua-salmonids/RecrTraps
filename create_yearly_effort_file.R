library(dplyr)
library(readxl)
library(writexl)
library(catchstat) # remotes::install_github("https://github.com/SLU-Aqua-salmonids/catchstat")


## Create a yearly effort file for the salmon and trout fishery.
## Change year in file names below as needed.
##
## This file reads the raw commercial catch data from an Excel file, filters for
## salmon catches by the fyke net fleet, calculates effort and CPUE per
## fishing trip (ansträngning), and writes the processed data to a new Excel file.
## The output file can then be used to analyze of recreational data using
## catch per unit effort (CPUE) from the commercial fishery.
##
#catchfile_root <- "~/Work/Data/HaV/Prelim/"
catchfile_root <- "C:/NoBackup/Work/Data/HaV/Prelim/"

catchfile <- file.path(catchfile_root, "SalmonTroutCatch-2024.xlsx") ## Change year

# Read the data
df <- read_excel(catchfile) %>%
  filter(ACKFLAGG == 1, MAFKOD == "SAL") %>%
  left_join(catchstat::fleet_key, by = join_by(REDSKKOD == gear_code)) %>%
  left_join(catchstat::fishing_area, by = join_by(SUBDIV)) %>%
  filter(metier == "FYK_C") %>%
  mutate(Anstr_id = paste0(LOGGBLNR, "_", ANSTRNR),
         Year = catchstat::DAT2year(LANDDAT),
         Month = catchstat::DAT2month(LANDDAT)) %>%
  mutate(
    Effort = case_when(
      LOGGTYP == "D" & metier == "FYK_C" ~ ANTREDSK * (ANSTRTIM / 24),
      LOGGTYP == "J" & metier == "FYK_C" ~ REDSKINS)) %>%
  mutate(CPUE_anstr = ANTAL / Effort) %>%
  select(Anstr_id, FishingArea, RUTA, Year, Month, Effort, weight_fish = KVANT,
         N_fish = ANTAL, CPUE_anstr)
 
# Write the data
write_xlsx(df, file.path("data", "effort-2024.xlsx")) ## Change year
