
# load in JSON table to join on to mitigators

mitigators_names <- jsonlite::fromJSON("mitigators.json")

mitigators_names <- tibble::enframe(unlist(mitigators_names)) |>
  purrr::set_names(c("parameter_name", "name"))

make_params <- function(params_list){

  params_df <- purrr::map_dfr(params_list, function(x){

    data.frame("low" = x[["interval"]][1], "high" = x[["interval"]][2])
  })

  params_df$parameter_name <- names(params_list)

  params_df <- params_df |>
    dplyr::select(parameter_name, low, high)

  params_df <- params_df |>
    dplyr::mutate(low = low * 100, high = high * 100)

  dplyr::left_join(params_df, mitigators_names, by = "parameter_name") |>
    dplyr::select(name, low, high)

}

show_table <- function(df1, df2){

  dplyr::full_join(df1, df2, by = "name",
                   suffix = c("_a_trust", "_b_trust")) |>
    gt::gt()
}

load_and_tabulate <- function(type, activ_eff){

  a_trust <- make_params(param_1$params[[activ_eff]][[type]])

  b_trust <- make_params(param_2$params[[activ_eff]][[type]])

  show_table(a_trust, b_trust)
}
