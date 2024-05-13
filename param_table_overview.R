source("funs_get_param.R")

highlighted_scheme <- "RGN"

all_activity_params <- jsonlite::fromJSON("secret/all_params.json")$params |> 
  report_params_table_activity_avoidance()

peers_activity_params <- activity_mitigators |> 
  dplyr::select(-value_1, -value_2, -time_profile) |> 
  dplyr::mutate(flag = 1) |> 
  tidyr::pivot_wider(names_from = "peer", values_from = "flag") |> 
  dplyr::mutate(dplyr::across(c(tidyselect::everything(), -activity_type, -strategy, -parameter), 
                ~ifelse(is.na(.x), 0, .x)))


replace_na(x,0)

gt::gt(peers_activity_params) |> 
  gt::data_color(direction = "column",
                 column = c(everything(), - activity_type, -strategy, -parameter),
                 #method = "auto",
                 palette = c("grey20", "gold"),
                 domain = c(0, 1)
                 )

