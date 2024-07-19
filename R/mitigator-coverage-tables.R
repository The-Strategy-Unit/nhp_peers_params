generate_binary_table <- function(
    activity_mitigators,
    highlighted_scheme,
    all_schemes
) {
  
  prepare_binary_table(activity_mitigators, highlighted_scheme, all_schemes) |>
    dplyr::rename("Your Scheme" = highlighted_scheme) |>
    dplyr::select(-parameter) |> 
    gt::gt(groupname_col = "activity_type") |>
    gt::fmt_markdown(columns = tidyselect::contains("Scheme")) |>
    gt::tab_style_body(
      style = gt::cell_fill(color = "gold"),
      values = "&#10003;"
    ) |>
    gt::tab_style_body(
      style = gt::cell_fill(color = "grey"),
      values = "&#10005;"
    ) |>
    gt::cols_align(align = "center", columns = tidyselect::contains("Scheme")) |>
    gt::grand_summary_rows(
      columns = -c(activity_type, strategy, perc_peers_picked),
      fns = perc_params_picked ~ scales::label_percent()
      (sum(stringr::str_detect("&#10003;", .x)) / length(.x)),
      side = "top",
    ) |>
    gt::cols_hide(1)
  
}

prepare_binary_table <- function(
    activity_mitigators,
    highlighted_scheme,
    all_schemes
) {
  
  activity_mitigators |>
    dplyr::select(-value_1, -value_2) |>
    dplyr::mutate(flag = 1) |>
    tidyr::pivot_wider(names_from = "peer", values_from = "flag") |>
    dplyr::mutate(perc_peers_picked = scales::label_percent()(
      rowSums(
        dplyr::across(c(
          tidyselect::everything(),
          -activity_type,
          -strategy,
          -parameter
        )),
        na.rm = TRUE
      ) /
        length(dplyr::across(
          c(
            tidyselect::everything(),
            -activity_type,
            -strategy,
            -parameter
          )
        ))
    )) |>
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
    dplyr::rename_with(
      ~ paste("Scheme", c(1:(length(all_schemes) - 1))),
      (c(
        tidyselect::everything(),
        -activity_type,
        -strategy,
        -parameter,
        -perc_peers_picked,
        -highlighted_scheme
      ))
    ) |>
    dplyr::select(
      activity_type,
      strategy,
      parameter,
      highlighted_scheme,
      tidyselect::everything()
    )
  
}

generate_certainty_table <- function(
    activity_mitigators,
    highlighted_scheme,
    all_schemes
) {
  
  certainty_data <- 
    prepare_certainty_table(activity_mitigators, highlighted_scheme, all_schemes) |> 
    dplyr::rename("Your Scheme" = highlighted_scheme) |>
    dplyr::select(-parameter)
  
  lowest_certainty <- certainty_data |> 
    dplyr::select(tidyselect::contains("Scheme")) |> 
    max(na.rm = TRUE) 
  
  certainty_data |> 
    gt::gt(groupname_col = "activity_type") |>
    gt::data_color(
      columns = tidyselect::contains("Scheme"),
      palette = c("grey90","red"),
      na_color = "white",
      domain = c(0, lowest_certainty)
    ) |> 
    gt::sub_missing(missing_text = htmltools::HTML("&#10005;")) |>
    gt::tab_style_body(
      style = gt::cell_text(color = "grey90"),
      fn = function(x) is.na(x)
    ) |>
    gt::cols_align(align = "center", columns = tidyselect::contains("Scheme")) |>
    gt::tab_stubhead(label = "activity_type")
  
}

prepare_certainty_table <- function(
    activity_mitigators,
    highlighted_scheme,
    all_schemes
) {
  
  activity_mitigators |>
    dplyr::mutate(range_val = value_2 - value_1) |>
    dplyr::select(-c(value_1, value_2)) |> 
    tidyr::pivot_wider(names_from = "peer", values_from = "range_val") |>
    dplyr::rename_with(
      ~ paste("Scheme", c(1:(length(all_schemes) - 1))),
      (c(
        tidyselect::everything(),
        -activity_type,
        -strategy,
        -parameter,
        -highlighted_scheme
      ))
    ) |>
    dplyr::select(
      activity_type,
      strategy,
      parameter,
      highlighted_scheme,
      tidyselect::everything()
    )
  
}

generate_binary_table_all <- function(
    activity_mitigators,
    #highlighted_scheme,
    all_schemes
) {
  
  prepare_binary_table_all(activity_mitigators, all_schemes) |>
    #dplyr::rename("Your Scheme" = highlighted_scheme) |>
    dplyr::select(-parameter) |> 
    gt::gt(groupname_col = "activity_type") |>
    gt::fmt_markdown(columns = tidyselect::any_of(all_schemes)) |>
    gt::tab_style_body(
      style = list(
        gt::cell_fill(color = "gold")#,
        #gt::cell_text(color = "gold")
        ),
      values = "&#10003;"
    ) |>
    gt::tab_style_body(
      style = gt::cell_fill(color = "grey"),
      values = "&#10005;"
    ) |>
    gt::cols_align(align = "center", columns = tidyselect::any_of(all_schemes)) |>
    gt::grand_summary_rows(
      columns = -c(activity_type, strategy, perc_peers_picked),
      fns = perc_params_picked ~ scales::label_percent()
      (sum(stringr::str_detect("&#10003;", .x)) / length(.x)),
      side = "top",
    ) |>
    gt::cols_hide(1)
  
}

prepare_binary_table_all <- function(
    activity_mitigators,
    #highlighted_scheme,
    all_schemes
) {
  
  activity_mitigators |>
    dplyr::select(-c(value_1, 
                     value_2,
                     baseline_year,
                     horizon_year,
                     peer,
                     scenario,
                     run_stage
                     )) |>
    dplyr::mutate(flag = 1) |>
    tidyr::pivot_wider(names_from = "peer_year", values_from = "flag") |>
    dplyr::mutate(perc_peers_picked = scales::label_percent()(
      rowSums(
        dplyr::across(c(
          tidyselect::everything(),
          -activity_type,
          -strategy,
          -parameter
        )),
        na.rm = TRUE
      ) /
        length(dplyr::across(
          c(
            tidyselect::everything(),
            -activity_type,
            -strategy,
            -parameter
          )
        ))
    )) |>
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
    # dplyr::rename_with(
    #   ~ paste("Scheme", c(1:(length(all_schemes) - 1))),
    #   (c(
    #     tidyselect::everything(),
    #     -activity_type,
    #     -strategy,
    #     -parameter,
    #     -perc_peers_picked,
    #     #-highlighted_scheme
    #   ))
    # ) |>
    dplyr::select(
      activity_type,
      strategy,
      parameter,
      #highlighted_scheme,
      tidyselect::everything()
    )
  
}

midpoint_pal_fun <- function(x){
  library(scales)
  lo <- as.numeric(stringr::str_extract(x, "(?<=\\().*(?= - )"))
  hi <- as.numeric(stringr::str_extract(x, "(?<= - ).*(?=\\))"))
  
  range <- hi - lo
  pal <- scales::col_numeric("Blues",
                             domain = c(0,1),
                             reverse = T,
                             na.color = "white")
  
  pal(range)
}
transform_fun <- function(x){
  #string <- stringr::str_split(x, "\\s(?=\\()")
  #midpoint <- string[1]
  #range <- string[2]
  
  paste0('<font size="+1">',
         stringr::str_split(x, "\\s(?=\\()")[1],
         "</font>",
         "<br>",
         '<font size="-1">',
         "(",
         stringr::str_split(x, "\\s(?=\\()")[2],
         ")",
         "</font>")}

generate_midpoint_table_all <- function(
    activity_mitigators,
    all_schemes
) {
  
  midpoint_data <- 
    prepare_midpoint_table_all(activity_mitigators, all_schemes) |> 
    dplyr::select(-parameter)
  
  # lowest_certainty <- certainty_data |> 
  #   dplyr::select(tidyselect::contains("Scheme")) |> 
  #   max(na.rm = TRUE) 
  
  midpoint_data |> 
    gt::gt(groupname_col = "activity_type") |>
    gt::data_color(
      columns = tidyselect::any_of(all_schemes),
      #palette = c("black","yellow"),
      fn = midpoint_pal_fun,
      #na_color = "white",
      #domain = c(0, 1)
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
    #) |> 
    #gt::tab_style_body(columns = tidyselect::any_of(all_schemes)) |> 
    gt::cols_align(align = "center", columns = tidyselect::any_of(all_schemes)) |>
    gt::tab_stubhead(label = "activity_type") #|>
    #gt::text_transform(fn = transform_fun,
     #                locations =
      #                gt::cells_body(columns = tidyselect::any_of(all_schemes))) |>
    #  })
   # gt::as_raw_html()
  
}

prepare_midpoint_table_all <- function(
    activity_mitigators,
    all_schemes
) {
  
  activity_mitigators |>
    dplyr::mutate(range_val = value_2 - value_1,
                  range_label_tmp = ifelse(range_val == 0,
                                       "Point Estimate",
                                       paste0(value_1, 
                                              " - ",
                                              value_2)),
                  midpoint = value_1 + ((value_2-value_1)/2),
                  value_label = paste0('<font size=+1>',
                                       midpoint, 
                                       " ",
                                       "</font>","<br>",
                                       '<font size=-1>',
                                       "(",
                                       range_label_tmp,
                                       ")",
                                       "</font>"
                                       )) |>
    dplyr::select(-c(value_1, 
                     value_2,
                     baseline_year,
                     horizon_year,
                     peer,
                     range_val,
                     scenario,
                     run_stage,
                     midpoint,
                     range_label_tmp)) |> 
    tidyr::pivot_wider(names_from = "peer_year", values_from = "value_label") |>
    dplyr::select(
      activity_type,
      strategy,
      parameter,
      tidyselect::everything()
    )
   
  
}

