library(ggplot2)
# jsons to list ----
json_to_param <- function(x){
  jsonlite::fromJSON(x)$params
}

params <- list.files("secret/jsons/", full.names = T) |> 
  purrr::map(json_to_param)

#read in nee values and select key column and hi/lo limits ----

nee <- readRDS("secret/nee_table.rds") |>
  dplyr::select(param_name,
                percentile10,
                percentile90) |> 
  dplyr::mutate(percentile10 = percentile10/100,
                percentile90 = percentile90/100)
  

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
  dplyr::left_join(nee, by = c(strategy = "param_name")) |> 
  dplyr::mutate(Midpoint = (value_2 + value_1)/2) |>
  dplyr::rename("Low" = value_1,
                "High" = value_2) |> 
  tidyr::pivot_longer(cols = c(Low, High, Midpoint), 
                      names_to = "value_type",
                      values_to = "value") |>
  dplyr::mutate(value_type = as.factor(value_type),
                shape_type = dplyr::case_when(value_type %in% c("Low",
                                                                "High") ~ "line",
                                              value_type %in% c("Midpoint") ~ "point"),
                ) |> 
  dplyr::mutate(peer = factor(peer)) |> 
  dplyr::group_by(activity_type,
                  strategy,
                  time_profile,
                  value_type) |> 
  dplyr::mutate(mean_val = mean(value),
                mean_type = paste("Mean", value_type)) |>
  tidyr::nest(data = c(peer, value_type, value, mean_val, mean_type, shape_type)) |>
  dplyr::select(parameter, 
                activity_type, 
                strategy, 
                time_profile, 
                dplyr::everything()) |> 
  dplyr::arrange(activity_type, strategy, time_profile) 


# save data ----

saveRDS(plot_data, "secret/plot_data.RDS")





  


