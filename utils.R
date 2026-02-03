library(dplyr)


ruta2area <- tribble(
  ~RUTA, ~area,
  5062, "gavle",
  5063, "gavle",
  5162, "sundsvall",
  5262, "sundsvall",
  5362, "sundsvall",
#  5364, "sundsvall",
  5462, "sundsvall",
  5463, "sundsvall",
  5563, "sundsvall",
  5564, "sundsvall",
  5665, "ume",
  5766, "ske",
  5866, "ske",
  6067, "lule",
  5966, "lule",
  5967, "lule",
  5968, "lule",
  6068, "haparanda",
  6069, "haparanda"
)
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
    group_by(Year, area) %>%
    summarise(quantile = scales::percent(probs),
              CPUE_q = quantile(CPUE_anstr, probs=prob),
              .groups = "drop")
  return(res)
}
