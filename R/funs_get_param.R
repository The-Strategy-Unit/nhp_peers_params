
# report_params_table_demographic_adjustment <- function(p) {
#   demographic_adjustment <- p[["demographic_factors"]]
#   
#   shiny::validate(
#     shiny::need(demographic_adjustment, "No parameters provided")
#   )
#   
#   demographic_adjustment |>
#     purrr::pluck("variant_probabilities") |>
#     unlist() |>
#     tibble::enframe("variant", "value") |>
#     dplyr::mutate(parameter = "demographic_factors",
#                   peer = p$dataset,
#                   baseline_year = p$start_year,
#                   horizon_year = p$end_year)
  #gt::gt("variant") |>
  #gt::fmt_percent("value") |>
  #gt_theme()
#}

# report_params_table_baseline_adjustment <- function(p) {
#   baseline_adjustment <- local({
#     x <- p[["baseline_adjustment"]]
#     if (!is.null(x[["aae"]])) {
#       x[["aae"]] <- purrr::map(x[["aae"]], \(.x) list(Other = .x))
#     }
#     return(x)
#   })
#   
#   shiny::validate(
#     shiny::need(baseline_adjustment, "No parameters provided")
#   )
#   
#   baseline_adjustment |>
#     purrr::map_depth(2, tibble::enframe, "specialty") |>
#     purrr::map(dplyr::bind_rows, .id = "pod") |>
#     dplyr::bind_rows(.id = "activity_type") |>
#     ##report_params_fix_data() |>
#     dplyr::relocate("value", .after = tidyselect::everything()) |>
#     tidyr::unnest("value") |>
#     dplyr::mutate(parameter = "baseline_adjustment",
#                   peer = p$dataset)
#   #gt::gt("specialty_name", c("activity_type_name", "pod")) |>
#   #gt_theme()
# }
# 
# report_params_table_covid_adjustment <- function(p) {
#   covid_adjustment <- p[["covid_adjustment"]]
#   
#   shiny::validate(
#     shiny::need(covid_adjustment, "No parameters provided")
#   )
#   
#   covid_adjustment |>
#     purrr::map(tibble::enframe, "pod") |>
#     dplyr::bind_rows(.id = "activity_type") |>
#     tidyr::unnest_wider("value", names_sep = "_") |>
#     dplyr::mutate(parameter = "covid_adjustment",
#                   peer = p$dataset)
#   #report_params_fix_data() |>
#   #gt::gt("pod", "activity_type_name") |>
#   #gt_theme()
# }
# 
# report_params_table_non_demographic_adjustment <- function(p) {
#   non_demographic_adjustment <- p[["non-demographic_adjustment"]]
#   
#   shiny::validate(
#     shiny::need(non_demographic_adjustment, "No parameters provided")
#   )
#   
#   non_demographic_adjustment |>
#     purrr::map(tibble::enframe, "pod") |>
#     dplyr::bind_rows(.id = "activity_type") |>
#     #report_params_fix_data() |>
#     tidyr::unnest_wider("value", names_sep = "_") |>
#     dplyr::mutate(parameter = "non-demographic_adjustment",
#                   peer = p$dataset)
#   #gt::gt("pod", "activity_type_name") |>
#   #gt_theme()
# }
# 
report_params_table_activity_avoidance <- function(p) {
  actitvity_avoidance <- p[["activity_avoidance"]]

  shiny::validate(
    shiny::need(actitvity_avoidance, "No parameters provided")
  )

  time_profiles <- p[["time_profile_mappings"]][["activity_avoidance"]] |>
    purrr::map(unlist) |>
    purrr::map(tibble::enframe, "strategy", "time_profile") |>
    data.table::rbindlist(idcol = "activity_type") |>
    dplyr::tibble()

  #dplyr::bind_rows(.id = "activity_type")

  actitvity_avoidance |>
    purrr::map_depth(2, "interval") |>
    purrr::map(tibble::enframe, "strategy") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "_") |>
    dplyr::left_join(
      time_profiles,
      by = dplyr::join_by("activity_type", "strategy")
    ) |>
    #report_params_fix_data() |>
    dplyr::arrange("activity_type_name", "mitigator_name") |>
    dplyr::mutate(parameter = "activity_avoidance",
                  peer = p$dataset,
                  baseline_year = p$start_year,
                  horizon_year = p$end_year)
  #gt::gt("mitigator_name", "activity_type_name") |>
  #gt_theme()
}
# 
report_params_table_efficiencies <- function(p) {
  efficiencies <- p[["efficiencies"]]

#   shiny::validate(
#     shiny::need(efficiencies, "No parameters provided")
#   )
#   
  time_profiles <- p[["time_profile_mappings"]][["efficiencies"]] |>
    purrr::map(unlist) |>
    purrr::map(tibble::enframe, "strategy", "time_profile") |>
    #dplyr::bind_rows(.id = "activity_type")
    data.table::rbindlist(idcol = "activity_type") |>
    dplyr::tibble()

  efficiencies |>
    purrr::map_depth(2, "interval") |>
    purrr::map(tibble::enframe, "strategy") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "_") |>
    dplyr::left_join(
      time_profiles,
      by = dplyr::join_by("activity_type", "strategy")
    ) |>
#     #report_params_fix_data() |>
    dplyr::arrange("activity_type_name", "mitigator_name") |>
    dplyr::mutate(parameter = "efficiencies",
                  peer = p$dataset,
                  baseline_year = p$start_year,
                  horizon_year = p$end_year) |>
    dplyr::filter(!stringr::str_detect(strategy, "bads"))
#   #gt::gt("mitigator_name", "activity_type_name") |>
#   #gt_theme()
 }
# 
# report_params_table_bed_occupancy_specialty_mapping <- function(p) {
#   p[["bed_occupancy"]][["specialty_mapping"]] |>
#     shiny::req() |>
#     purrr::map(unlist) |>
#     purrr::map(tibble::enframe, "specialty", "ward_group") |>
#     dplyr::bind_rows(.id = "specialty_group") |>
#     dplyr::mutate(parameter = "bed_occupancy_specialty_mapping",
#                   peer = p$dataset)
# }
# 
# report_params_table_waiting_list_adjustment <- function(p) {
#   waiting_list_adjustment <- p[["waiting_list_adjustment"]]
#   
#   shiny::validate(
#     shiny::need(waiting_list_adjustment, "No parameters provided")
#   )
#   
#   waiting_list_adjustment |>
#     purrr::map(tibble::enframe, "specialty", "value") |>
#     dplyr::bind_rows(.id = "activity_type") |>
#     tidyr::unnest("value") |>
#     dplyr::mutate(parameter = "waiting_list_adjustment",
#                   peer = p$dataset)
# }
# 
# report_params_table_bed_occupancy_day_night <- function(p) {
#   p[["bed_occupancy"]][["day+night"]] |>
#     # list() |> 
#     #  data.table::rbindlist() |> 
#     shiny::req() |>
#     dplyr::bind_rows(.id = "ward_group") |>
#     #potential fix?
#     tidyr::pivot_longer(cols = dplyr::everything(), names_to = "ward_group") |> 
#     dplyr::group_by(ward_group) |> 
#     dplyr::mutate(value_1 = min(value),
#                   value_2 = max(value)) |> 
#     dplyr::distinct(ward_group, value_1, value_2) |>
#     dplyr::mutate(parameter = "bed_occupancy_day+night",
#                   peer = p$dataset)
# }

