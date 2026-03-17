library(dplyr)


ruta2area <- tribble(
  ~RUTA, ~area_nr,
  5062, 1,
  5063, 1,
  5162, 2,
  5262, 2,
  5362, 2,
#  5364, 2,
  5462, 2,
  5463, 2,
  5563, 2,
  5564, 2,
  5665, 3,
  5766, 3,
  5866, 4,
  6067, 5,
  5966, 5,
  5967, 5,
  5968, 5,
  6068, 6,
  6069, 6
)
ruta2area$Area <- factor(ruta2area$area_nr,
                         labels = c(
                           "Gävle",
                           "Sundsvall",
                           "Umeå",
                           "Skellefteå",
                           "Luleå",
                           "Haparanda"
                         ))
Area2subdiv <- tribble(
  ~FishingArea, ~subdiv,
  "27.3.d.29", 29,
  "27.3.d.30", 30,
  "27.3.d.31", 31
)

slurp_effort <- function(file_list) {
  res <- do.call("rbind", file_list) %>%
    filter(Effort > 0) %>%
    filter(N_fish > 0) %>%
    filter(!RUTA == 0) %>%
    mutate(CPUE_anstr = N_fish / Effort) %>%
    ##  filter(CPUE_anstr < 100) %>%
    left_join(Area2subdiv, by = "FishingArea") %>%
    filter(subdiv %in% c(30, 31))
}

calc_CPUE_per_month <- function(anstr, squares) {
  res <- anstr %>%
    filter(RUTA %in% squares) %>%
    group_by(Year, Month) %>%
    summarise(CPUE_summa = sum(N_fish) / sum(Effort),
              CPUE_mean = mean(CPUE_anstr),
              CPUE_25 = quantile(CPUE_anstr, probs = .25),
              CPUE_median = quantile(CPUE_anstr, probs = .50),
              CPUE_75 = quantile(CPUE_anstr, probs = .75),
              SD_cpue = sd(CPUE_anstr),
              N_anstr = n(),
              .groups = "drop")
  return(res)
}

#calc_CPUE_quantiles <-  function(anstr, squares, probs) {
calc_CPUE_quantiles <-  function(anstr, probs) {
  res <- anstr %>%
#    filter(RUTA %in% squares) %>%
    group_by(Year, Area) %>%
    summarise(quantile = scales::percent(probs),
              CPUE_q = quantile(CPUE_anstr, probs=prob),
              .groups = "drop")
  return(res)
}
