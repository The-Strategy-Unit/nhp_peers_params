source("funs_get_param.R")

highlighted_scheme <- "RAS"

make_param_gt <- function(highlighted_scheme){
all_activity_params <- jsonlite::fromJSON("secret/all_params.json")$params |> 
  report_params_table_activity_avoidance()

all_schemes <- readRDS("secret/all_peers.rds")

peers_activity_params <- activity_mitigators |> 
  dplyr::select(-value_1, -value_2, -time_profile) |> 
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
  dplyr::rename_with(#dplyr::across(c(tidyselect::everything(), 
                      #          -activity_type,
                        #        -strategy,
                       #         -parameter,
                         #       -perc_peers_picked)))
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
  

gt::gt(peers_activity_params) |> 
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

