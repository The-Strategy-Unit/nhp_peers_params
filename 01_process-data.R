# Prepare raw data for use in reports


# Prepare workspace -------------------------------------------------------


purrr::walk(
  list.files("R", pattern = ".R$", full.names = TRUE, recursive = TRUE),
  source
)


# Read data ---------------------------------------------------------------


params <- list.files("data/jsons/", pattern = ".json$", full.names = TRUE) |>
  purrr::map(jsonlite::read_json) |>
  purrr::map(purrr::pluck("params"))

nee <- readRDS("data/nee_table.rds") |>
  dplyr::select(param_name, percentile10, percentile90) |>
  dplyr::mutate(
    percentile10 = percentile10 / 100,
    percentile90 = percentile90 / 100
  )


# Wrangle data ------------------------------------------------------------


activity_mitigators <- params |> 
  purrr::map(purrr::possibly(report_params_table_activity_avoidance)) |>
  purrr::list_rbind()

all_schemes <- unique(activity_mitigators$peer)
saveRDS(all_schemes, "outputs/processed-data/all_peers.rds")

plot_data <- activity_mitigators |>
  dplyr::left_join(nee, by = c(strategy = "param_name")) |>
  dplyr::mutate(Midpoint = (value_2 + value_1) / 2) |>
  dplyr::rename("Low" = value_1, "High" = value_2) |>
  tidyr::pivot_longer(
    cols = c(Low, High, Midpoint),
    names_to = "value_type",
    values_to = "value"
  ) |>
  dplyr::mutate(
    value_type = as.factor(value_type),
    shape_type = dplyr::case_when(
      value_type %in% c("Low", "High") ~ "line",
      value_type == "Midpoint" ~ "point"
    ),
  ) |>
  dplyr::mutate(peer = factor(peer)) |>
  dplyr::group_by(
    activity_type,
    strategy,
    time_profile,
    value_type
  ) |>
  dplyr::mutate(
    mean_val = mean(value),
    mean_type = paste("Mean", value_type)
  ) |>
  tidyr::nest( 
    data = c(peer, value_type, value, mean_val, mean_type, shape_type)
  ) |>
  dplyr::select(
    parameter,
    activity_type,
    strategy,
    time_profile,
    dplyr::everything()
  ) |>
  dplyr::arrange(activity_type, strategy, time_profile)

saveRDS(plot_data, "outputs/processed-data/plot_data.rds")
