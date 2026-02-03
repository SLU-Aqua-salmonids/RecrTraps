library(dplyr)
library(readxl)

areas <- c("27.3.d.30", "27.3.d.31")

d_raw <- read_excel("effort-2022.xlsx") %>%
    mutate(CPUE_anstr = as.numeric(CPUE_anstr)) %>%
    filter(FishingArea %in% areas) %>%
    filter(is.finite(CPUE_anstr))

## Beräkna varianter på CPUE. Terminologi från Pollock et.al. 1997
## R_1a "Ratio of means"
## R_2a "Mean of Ratios"
## SD_2a Standard Deviation of R_2a
## R_2median samma som R_2a men median istf mean
## Antal "loggblad" som ingår i varje resultatrad
CPUE_tab <- d_raw %>%
    group_by(FishingArea, Month) %>%
    summarise(R_1a = sum(N_fish) / sum(Effort),
              R_2a = mean(N_fish / Effort),
              SD_2a = sd(N_fish / Effort),
              R_2median = median(N_fish / Effort),
              N_report = n(),
              .groups = "drop")

print(CPUE_tab)
