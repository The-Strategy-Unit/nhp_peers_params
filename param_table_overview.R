source("funs_get_param.R")

highlighted_scheme <- "RAS"

json_to_param <- function(x){
  jsonlite::fromJSON(x)$params
}

params <- list.files("secret/jsons/", full.names = T) |> 
  purrr::map(json_to_param)

selected_activity_mitigators <- purrr::map(params, purrr::possibly(report_params_table_activity_avoidance)) |> 
  purrr::list_rbind()|> 
  dplyr::select(-time_profile)

sample_activity_params <- jsonlite::fromJSON("secret/all_params.json")$params |> 
  report_params_table_activity_avoidance() |> 
  dplyr::select(-time_profile,
                -value_1,
                -value_2,
                -peer)

activity_mitigators <- sample_activity_params |> 
  dplyr::left_join(selected_activity_mitigators,
                   by = c("activity_type",
                          "strategy",
                          "parameter"))

make_param_gt <- function(highlighted_scheme){

all_schemes <- readRDS("secret/all_peers.rds")

peers_activity_params_yesno <- activity_mitigators |> 
  dplyr::select(-value_1, -value_2) |> 
  dplyr::mutate(flag = 1) |> 
  tidyr::pivot_wider(names_from = "peer", values_from = "flag") |> 
  dplyr::mutate(perc_peers_picked = scales::label_percent()(
    rowSums(dplyr::across(c(
    tidyselect::everything(), 
    -activity_type, 
    -strategy, 
    -parameter)),
    na.rm = T) /
      length(dplyr::across(
        c(
          tidyselect::everything(), 
          -activity_type, 
          -strategy, 
          -parameter)
      ))
  )
) |> 
  dplyr::mutate(dplyr::across(c(tidyselect::everything(), 
                                -activity_type,
                                -strategy,
                                -parameter,
                                -perc_peers_picked), 
                ~ifelse(is.na(.x), 
                        htmltools::HTML("&#10005;"), #cross
                        htmltools::HTML("&#10003;") #tick 
                        ))) |> 
  dplyr::rename_with(
    ~paste("Scheme", c(1:(length(all_schemes)-1))), 
    (c(tidyselect::everything(), 
               -activity_type,
               -strategy,
               -parameter,
               -perc_peers_picked,
               -highlighted_scheme))) |> 
      dplyr::select(activity_type,
                    strategy,
                    parameter,
                    highlighted_scheme,
                    tidyselect::everything())
  
# heatmap style ----

gt::gt(peers_activity_params_yesno) |> 
  gt::fmt_markdown(columns = c(tidyselect::everything(), 
                               -activity_type,
                               -strategy,
                               -parameter,
                               -perc_peers_picked)
                   ) |> 
  gt::tab_style_body(
    style = gt::cell_fill(color = "gold"),
    values = "&#10003;"
  ) |> 
  gt::tab_style_body(
    style = gt::cell_fill(color = "grey80"),
    values = "&#10005;"
  ) |> 
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = c("left", "right"),
        color = "red",
        weight = gt::px(3)
      )
      ),
      locations = list(
        gt::cells_body(
          columns = highlighted_scheme
        ),
        gt::cells_column_labels(
          columns = highlighted_scheme
        )
      )
    ) |> 
  gt::cols_align(align = "center") |> 
  gt::grand_summary_rows(columns = c(tidyselect::everything(), 
                               -activity_type,
                               -strategy,
                               -parameter,
                               -perc_peers_picked
                               ),
                         fns = perc_params_picked ~ scales::label_percent() 
                         (sum(stringr::str_detect("&#10003;", .x))/ 
                           length(.x))
  )
}

make_param_gt("RCX")

# simple style ----

gt::gt(peers_activity_params |> 
         dplyr::rename("Your Scheme" = highlighted_scheme) |> 
         dplyr::select(-parameter),
       #groupname_col = "activity_type"
       ) |> 
  gt::fmt_markdown(columns = c(tidyselect::everything(), 
                               -activity_type,
                               -strategy,
                              # -parameter,
                               -perc_peers_picked)
  ) |> 
  gt::tab_style_body(
    style = gt::cell_fill(color = "gold"),
    values = "&#10003;"
  ) |>
  gt::tab_style_body(
    style = gt::cell_text(color = "gold"),
    values = "&#10003;"
  ) |>
  gt::tab_style_body(
    style = gt::cell_fill(color = "grey20"),
    values = "&#10005;"
  ) |> 
  gt::tab_style_body(
    style = gt::cell_text(color = "grey20"),
    values = "&#10005;"
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = c("left", "right"),
        color = "grey50",
        weight = gt::px(5)
      )
    ),
    locations = list(
      gt::cells_body(
        columns = "Your Scheme"
      ),
      gt::cells_column_labels(
        columns = "Your Scheme"
      )
    )
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = c("left", "right"),
        color = "grey50",
        weight = gt::px(1.5)
      )
    ),
    locations = gt::cells_body(
      #columns = "Scheme 1"
    )
  ) |>
  gt::cols_align(align = "center") |> 
  gt::grand_summary_rows(columns = c(tidyselect::everything(),
                                     -activity_type,
                                     -strategy,
                                     #-parameter,
                                     -perc_peers_picked
  ),
  fns = perc_params_picked ~ scales::label_percent()
  (sum(stringr::str_detect("&#10003;", .x))/
      length(.x)),
  side = "top") |> 
  gt::tab_header(title = unique(peers_activity_params$parameter)) |> 
  gt::tab_stubhead(label = activity_type)
  
# range heat map ----

param_certainty_data <- activity_mitigators |> 
  dplyr::mutate(range_val = value_2-value_1,
                range_text = paste(value_1, value_2, sep = " - ")) |> 
  dplyr::select(-value_1,
                -value_2) |>
  dplyr::arrange(range_val)
  dplyr::mutate(factor_tmp = factor(range_text,
                                    levels = unique(range_val)))
  tidyr::pivot_wider(names_from = "peer", values_from = c("range_val", "range_text")) 
  





