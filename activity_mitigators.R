library(ggplot2)
# jsons to list ----
json_to_param <- function(x){
  jsonlite::fromJSON(x)$params
}

params <- list.files("secret/jsons/", full.names = T) |> 
  purrr::map(json_to_param)

# source funs ----
source("funs_get_param.R")

# apply fun to data ----
activity_mitigators <- purrr::map(params, purrr::possibly(report_params_table_activity_avoidance)) |> 
  purrr::list_rbind()

#save unique scheme codes ----
all_schemes <- unique(activity_mitigators$peer)
saveRDS(all_schemes, "secret/all_peers.rds")

# create plot_data df ----
plot_data <- activity_mitigators |> 
  dplyr::mutate(midpoint = (value_2 + value_1)/2) |>
  dplyr::rename("lo" = value_1,
                "hi" = value_2) |> 
  tidyr::pivot_longer(cols = c(lo, hi, midpoint), 
                      names_to = "value_type",
                      values_to = "value") |> 
  dplyr::mutate(peer = factor(peer)) |> 
  dplyr::group_by(activity_type,
                  strategy,
                  time_profile,
                  value_type) |> 
  dplyr::mutate(mean_val = mean(value)) |> 
  tidyr::nest(data = c(peer, value_type, value, mean_val)) |>
  dplyr::arrange(activity_type, strategy, time_profile) 

# save data ----

saveRDS(plot_data, "secret/plot_data.RDS")





  


