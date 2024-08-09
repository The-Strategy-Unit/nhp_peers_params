generate_binary_table_all <- function(activity_mitigators, all_schemes) {
  
  binary_data <- 
    prepare_binary_table_all(activity_mitigators, all_schemes) |>
    dplyr::select(-parameter)
  
  binary_data |> 
    gt::gt(groupname_col = "activity_type") |>
    gt::fmt_markdown(columns = tidyselect::any_of(all_schemes)) |>
    gt::tab_style_body(
      style = list(gt::cell_fill(color = "gold")),
      values = "&#10003;"  # tick
    ) |>
    gt::tab_style_body(
      style = gt::cell_fill(color = "grey"),
      values = "&#10005;"  # cross
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::any_of(all_schemes)) |>
    gt::grand_summary_rows(
      columns = -c(activity_type, strategy, perc_peers_picked),
      fns = perc_params_picked ~ scales::label_percent()(
        sum(stringr::str_detect("&#10003;", .x)) / length(.x)
      ),
      side = "top",
    ) |>
    gt::cols_hide(1)
  
}

prepare_binary_table_all <- function(activity_mitigators, all_schemes) {
  
  activity_mitigators |>
    dplyr::select(
      -c(
        value_1, 
        value_2,
        baseline_year,
        horizon_year,
        peer,
        scenario,
        run_stage
      )
    ) |>
    dplyr::mutate(flag = 1) |>
    tidyr::pivot_wider(names_from = "peer_year", values_from = "flag") |>
    dplyr::mutate(
      perc_peers_picked = scales::label_percent()(
        rowSums(
          dplyr::across(
            c(
              tidyselect::everything(),
              -activity_type,
              -strategy,
              -parameter
            )
          ),
          na.rm = TRUE
        ) /
          length(
            dplyr::across(
              c(
                tidyselect::everything(),
                -activity_type,
                -strategy,
                -parameter
              )
            )
          )
      )
    ) |>
    dplyr::mutate(dplyr::across(
      c(
        tidyselect::everything(),
        -activity_type,
        -strategy,
        -parameter,
        -perc_peers_picked
      ),
      ~ ifelse(
        is.na(.x),
        htmltools::HTML("&#10005;"), # cross
        htmltools::HTML("&#10003;") # tick
      )
    )) |>
    dplyr::select(
      activity_type,
      strategy,
      parameter,
      #highlighted_scheme,
      tidyselect::everything()
    )
  
}

generate_midpoint_table_all <- function(activity_mitigators, all_schemes) {
  
  midpoint_data <- 
    prepare_midpoint_table_all(activity_mitigators, all_schemes) |> 
    dplyr::select(-parameter)
  
  midpoint_data |> 
    gt::gt(groupname_col = "activity_type") |>
    gt::data_color(
      columns = tidyselect::any_of(all_schemes),
      fn = midpoint_pal_fun
    ) |> 
    gt::fmt_markdown() |> 
    gt::sub_missing(missing_text = htmltools::HTML("&#10005;")) |>
    gt::tab_style_body(
      style = gt::cell_text(color = "grey80"),
      fn = function(x) is.na(x)
    ) |>
    gt::tab_style_body(
      columns = tidyselect::any_of(all_schemes),
      pattern = "(Point Estimate)", 
      style = gt::cell_fill(color = "grey80")) |> 
    gt::cols_align(align = "center", columns = tidyselect::any_of(all_schemes)) |>
    gt::tab_stubhead(label = "activity_type") 
  
}

prepare_midpoint_table_all <- function(activity_mitigators, all_schemes) {
  
  activity_mitigators |>
    dplyr::mutate(
      range_val = value_2 - value_1,
      range_label_tmp = dplyr::if_else(
        range_val == 0,
        "Point Estimate",
        paste0(value_1, " - ", value_2)
      ),
      midpoint = value_1 + ((value_2 - value_1) / 2),
      value_label = paste0(
        '<font size=+1>',
        midpoint, 
        " ",
        "</font>","<br>",
        '<font size=-1>',
        "(",
        range_label_tmp,
        ")",
        "</font>"
      )
    ) |>
    dplyr::select(
      -c(
        value_1, 
        value_2,
        baseline_year,
        horizon_year,
        peer,
        range_val,
        scenario,
        run_stage,
        midpoint,
        range_label_tmp
      )
    ) |> 
    tidyr::pivot_wider(names_from = "peer_year", values_from = "value_label") |>
    dplyr::select(
      activity_type,
      strategy,
      parameter,
      tidyselect::everything()
    )
  
  
}

midpoint_pal_fun <- function(x) {
  
  lo <- as.numeric(stringr::str_extract(x, "(?<=\\().*(?= - )"))
  hi <- as.numeric(stringr::str_extract(x, "(?<= - ).*(?=\\))"))
  range <- hi - lo
  
  pal <- scales::col_numeric(
    "Blues",
    domain = c(0, 1),
    reverse = TRUE,
    na.color = "white"
  )
  
  pal(range)
  
}