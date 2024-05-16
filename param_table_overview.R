source("funs_get_param.R")

highlighted_scheme <- "RGN"

all_activity_params <- jsonlite::fromJSON("secret/all_params.json")$params |> 
  report_params_table_activity_avoidance()

peers_activity_params <- activity_mitigators |> 
  dplyr::select(-value_1, -value_2, -time_profile) |> 
  dplyr::mutate(flag = 1) |> 
  tidyr::pivot_wider(names_from = "peer", values_from = "flag") |> 
  dplyr::mutate(dplyr::across(c(tidyselect::everything(), -activity_type, -strategy, -parameter), 
                ~ifelse(is.na(.x), 
                        htmltools::HTML("&#10005;"), #cross
                        htmltools::HTML("&#10003;") #tick 
                        )))
                        #htmltools::HTML("&#10004;"))))


#replace_na(x,0)

gt::gt(peers_activity_params) |> 
  gt::fmt_markdown(columns = c(tidyselect::everything(), 
                               -activity_type,
                               -strategy,
                               -parameter)
                   ) |> 
  #gt::data_color(direction = "column",
   #              column = c(tidyselect::everything(), - activity_type, -strategy, -parameter),
    #             #method = "auto",
     #            palette = c("grey20", "gold"),
      #           domain = c(fontawesome::fa("times"),
        #                    htmltools::HTML("&#10003;")
       #                     )
         #        )
  # gt::tab_style(
  #   style = list(
  #     gt::cell_fill(color = "gold")
  #     ),
  #   location = gt::cells_body(
  #     columns = c(tidyselect::everything(), - activity_type, -strategy, -parameter),
  #     #rows = peers_activity_params$activity_type[peers_activity_params$activity_type == "&#10003;"]
  #     )
  #   )
  gt::tab_style_body(
    style = gt::cell_fill(color = "gold"),
    values = "&#10003;"
  ) |> 
  gt::tab_style_body(
    style = gt::cell_fill(color = "grey80"),
    values = "&#10005;"
  ) |> 
  gt::cols_align(align = "center")

