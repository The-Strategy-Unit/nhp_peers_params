# Generate mitigator coverage tables (binary and certainty heatmap)


# Prepare workspace -------------------------------------------------------


purrr::walk(
  list.files("R", pattern = ".R$", full.names = TRUE, recursive = TRUE),
  source
)


# Read data ---------------------------------------------------------------


params <- list.files("data/jsons/", pattern = ".json$", full.names = TRUE) |>
  purrr::map(jsonlite::read_json) |>
  purrr::map(purrr::pluck("params"))

selected_activity_mitigators <- params |> 
  purrr::map(purrr::possibly(report_params_table_activity_avoidance)) |>
  purrr::list_rbind() |>
  dplyr::select(-time_profile)

sample_activity_params <- jsonlite::fromJSON("data/all_params.json")$params |>
  report_params_table_activity_avoidance() |>
  dplyr::select(-c(time_profile, value_1, value_2, peer))

activity_mitigators <- sample_activity_params |>
  dplyr::left_join(
    selected_activity_mitigators,
    by = c("activity_type", "strategy", "parameter"))

all_schemes <- readRDS("outputs/processed-data/all_peers.rds")


# Generate tables ---------------------------------------------------------


# Binary (mitigator was selected or not)

for (highlighted_scheme in all_schemes) {
  
  cat("* Creating binary table:", highlighted_scheme, "\n")
  
  filename <- 
    glue::glue("DRAFT_{Sys.Date()}_{highlighted_scheme}_activity-mitigators_binary-table.html")
  
  generate_binary_table(activity_mitigators, highlighted_scheme, all_schemes) |> 
    gt::gtsave(file.path("outputs", "tables", filename))
  
}

# Heatmap (colour indicates certainty, i.e. range of low to high values)

for (highlighted_scheme in all_schemes) {
  
  cat("* Creating certainty heatmap table:", highlighted_scheme, "\n")
  
  filename <- 
    glue::glue("DRAFT_{Sys.Date()}_{highlighted_scheme}_activity-mitigators_certainty-table.html")
  
  generate_certainty_table(activity_mitigators, highlighted_scheme, all_schemes) |> 
    gt::gtsave(file.path("outputs", "tables", filename))
  
}
