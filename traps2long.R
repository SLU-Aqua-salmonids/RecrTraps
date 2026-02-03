##
## Pivot trap data into a "long" dataformat.
## This is an example script showing how to format trap catch data
## into a long format suitable for analysis. Input data is not available
## in the github repo due to GDPR.

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

d  <- read_excel("data/fritidsfällinventeringen_data_2021-2024_vs20250130.xlsx") %>%
    group_by(LÄN) %>%
    mutate(idno = row_number()) %>%
    mutate(trap_id = sprintf("%s_%d", LÄN, idno)) %>%
    ungroup() %>%
    select(Year = ÅR, trap_id, SUBDIV = `ices sd`,
           Area_type = `DELOMRÅDE (TYP)`,
           RUTA = `ices ruta_2`, May = `antal fiskedgr_maj`,
           June = `antal fiskedgr_juni`, July = `antal fiskedgr_juli`,
           August = `antal fiskedgr_augusti`)

d_long <- d %>%
    pivot_longer(cols = c(May, June, July, August),
                 names_to = "Month", values_to = "Days") %>%
    filter(Days > 0)

write_xlsx(d_long, path = "trap_days_long.xlsx")
