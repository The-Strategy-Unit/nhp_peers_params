# 0.0 Setup ----

#load ggplot
library(ggplot2)

# source functions
# purrr::walk(
#   list.files("R", pattern = ".R$", full.names = TRUE, recursive = TRUE),
#   source
# )

purrr::walk(
  c("R/funs_get_param.R",
  "R/mitigator-coverage-tables.R"),
  source)

## 0.1 read data ----

params <- list.files("data/jsons/", pattern = ".json$", full.names = TRUE) |>
  purrr::map(jsonlite::read_json) |>
  purrr::map(purrr::pluck("params"))


## 0.2 read mitigator name look up ----

mitigator_lookup <- readr::read_csv("data/mitigator_name_lookup.csv") |> 
  dplyr::select("Strategy variable",
                "Mitigator code") 

## 0.3 read nee ----

nee <- readRDS("data/nee_table.rds") |>
  dplyr::select(param_name, percentile10, percentile90) |>
  dplyr::mutate(
    percentile10 = percentile10 / 100,
    percentile90 = percentile90 / 100
  ) |> 
  dplyr::filter(!stringr::str_detect(param_name, "bads"))

## 0.4 read mitigator groupings ----

mitigator_groups <- readxl::read_excel("data/mitigator groupings.xlsx") |>  
  dplyr::mutate(Grouping = stringr::str_replace_all(Grouping, 
                                                    "ameanable",
                                                    "amenable"))

## 0.5 make df ----

extracted_params <- params |> 
  purrr::map(purrr::possibly(report_params_table_activity_avoidance)) |>
  purrr::list_rbind() |> 
  dplyr::bind_rows(
    params |> 
      purrr::map(purrr::possibly(report_params_table_efficiencies)) |> 
      purrr::list_rbind()
  ) |> 
  dplyr::mutate(peer_year = paste0(peer,
                                   "_",
                                   stringr::str_sub(
                                     baseline_year, 3, 4),
                                   "_",
                                   stringr::str_sub(
                                     horizon_year, 3, 4)
  )) 

skeleton_df <- tidyr::expand_grid("strategy" = unique(extracted_params$strategy), 
                                  "peer_year" = unique(extracted_params$peer_year)) |> 
  dplyr::left_join(extracted_params |> 
                     dplyr::group_by(
                       activity_type,
                       strategy,
                       parameter
                     ) |> 
                     dplyr::summarise(tmp = dplyr::n()) |> 
                     dplyr::select(-tmp))

df <- skeleton_df |> 
  dplyr::left_join(extracted_params) |>  
  dplyr::left_join(mitigator_lookup,
                   by = c(strategy = "Strategy variable"))|>
  dplyr::left_join(
    nee |> 
      #tidyr::expand_grid(peer = unique(extracted_params$peer)) |> 
      dplyr::rename("strategy"  = param_name) #|> 
      #dplyr::filter(strategy %in% unique(extracted_params$strategy)
      ) |>
  dplyr::filter((value_1 <= 1 &
                  value_2 <= 1)|
                  is.na(value_1) &
                  is.na(value_2)) |> 
  dplyr::mutate(midpoint = value_1 + (value_2-value_1)/2) |> 
  dplyr::left_join(mitigator_groups |> 
                     dplyr::select(`Mitigator code`,
                                   Grouping))|>
  dplyr::mutate(percentile50 = percentile10 - (percentile10-percentile90)/2,
                point_or_range = ifelse((value_2 - value_1) == 0,
                                        "point",
                                        "range")
  ) |> 
  dplyr::arrange(`Mitigator code`)
  

## 0.6 plot funs ----
### 0.6.1 original plots ----
#### 0.6.1.1 POD ----

plot_pod_original <- function(pod){
  print(
  df |> 
    dplyr::filter(activity_type == pod) |> 
    ggplot(aes(x = midpoint, y = `Mitigator code`, colour = peer_year))+
    geom_crossbar(aes(x = percentile50, 
                      xmin = percentile90,
                      xmax = percentile10),
                  fill = "lightgrey",
                  colour = "grey85",
                  alpha = 0.2,
                  width = 0.3)+
    geom_pointrange(#data = ~ .x |> dplyr::filter(point_or_range == "range"),
                    aes(x = midpoint, 
                        xmin = value_1,
                        xmax = value_2),
                    size = 0.2,
                    linewidth = 1
    )+
    # geom_point(data = ~ .x |> dplyr::filter(point_or_range == "point"),
    #            shape = 4,
    #            size = 1,
    #            stroke = 1) +
    facet_wrap(~peer_year,
               nrow = 1,
    )+
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       limits = c(0,1),
                       labels = scales::label_percent(suffix = ""),
                       name = "% Reduction (assumption)"
                       #,guide = guide_axis(n.dodge=1,angle = 45)
                       
    )+
    ylab("Mitigator Code")+
    theme_bw() + 
    theme(panel.grid.major.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.spacing = unit(0.5, "lines"),
          panel.background = element_rect(fill = NA, color = "grey20")
    )+
    ggtitle(paste(toupper(pod), "Overview"))
  )
}

#### 0.6.1.2 Mitigator groupings ----

plot_mitigator_group_original <- function(group){
  print(
  df |> 
    dplyr::filter(Grouping == group) |>
    ggplot(aes(x = midpoint, y = `Mitigator code`, colour = peer_year))+
    geom_crossbar(aes(x = percentile50, 
                      xmin = percentile90,
                      xmax = percentile10),
                  fill = "lightgrey",
                  colour = "grey85",
                  alpha = 0.2,
                  width = 0.3)+
    geom_pointrange(#data = ~ .x |> dplyr::filter(point_or_range == "range"),
                    aes(x = midpoint, 
                        xmin = value_1,
                        xmax = value_2),
                    size = 0.2,
                    linewidth = 1
    )+
    # geom_point(data = ~ .x |> dplyr::filter(point_or_range == "point"),
    #            shape = 4,
    #            size = 1,
    #            stroke = 1) +
    facet_wrap(~peer_year,
               nrow = 2,
    )+
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       limits = c(0,1),
                       labels = scales::label_percent(suffix = ""),
                       name = "% Reduction (assumption)")+
    ylab("Mitigator code")+
    theme_bw() + 
    theme(panel.grid.major.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.spacing = unit(1, "lines"),
          panel.background = element_rect(fill = NA, color = "grey20")
    )+
    ggtitle(group)
  )
}


### 0.6.2 original plots ----

#### 0.6.2.1 Activity POD ----

plot_pod_transposed <- function(pod){
  
  cat(#':::{.content-visible when-format="html"}', "\n",
      "###", pod, "{.hide}", "\n")
  
  if(pod == "ip"){
    facet_cols = 11
    wrap_width = 17
    facet_font = 6
  }else{
    facet_cols = 8
    wrap_width = 20
    facet_font = 7
  }
  
  
  
  colours <- tibble::tibble(
    peer_year = unique(df$peer_year[df$parameter == "activity_avoidance"]),
    colour = scales::hue_pal()(length(unique(df$peer_year[df$parameter == "activity_avoidance"])))) |> 
    dplyr::mutate(new_peer_year = paste("<span style = 'color: ",
                                          colour,
                                          ";'>",
                                          "\u25a0",
                                          "</span>",
                                          " ",
                                          peer_year,
                                          sep = ""))
  
  print(
  df |> 
    dplyr::filter(activity_type == pod & 
                    parameter == "activity_avoidance") |> 
    dplyr::mutate(strategy = stringr::str_replace_all(strategy,
                                                      "_",
                                                      " ")) |>
    dplyr::left_join(colours) |>
    dplyr::mutate(peer_year = new_peer_year) |> 
    dplyr::select(-new_peer_year) |> 
    ggplot(aes(x = midpoint, y = peer_year, colour = colour))+
    geom_crossbar(aes(x = percentile50, 
                      xmin = percentile90,
                      xmax = percentile10),
                  fill = "lightgrey",
                  colour = "grey85",
                  alpha = 0.2,
                  width = 0.4)+
    geom_pointrange(#data = ~ .x |> dplyr::filter(point_or_range == "range"),
                    aes(x = midpoint, 
                        xmin = value_1,
                        xmax = value_2),
                    size = 0.3,
                    linewidth = 1.2
    )+
    scale_colour_identity()+
    # geom_point(data = ~ .x |> dplyr::filter(point_or_range == "point"),
    #            shape = 4,
    #            size = 1,
    #            stroke = 1) +
    facet_wrap(~ strategy,
                 #`Mitigator code`,
               #nrow = 2,
               labeller = label_wrap_gen(width = wrap_width),
               ncol = facet_cols
    )+
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       labels = c(0, 0.5, 1),
                       limits = c(0,1),
                       #labels = scales::label_percent(suffix = ""),
                       name = "Assumption input (0-1)"
                       #,guide = guide_axis(n.dodge=1,angle = 45)
                       
    )+
    ylab("Trust Code")+
    theme_bw() + 
    theme(panel.grid.major.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = ggtext::element_markdown(),
          legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          panel.background = element_rect(fill = NA, color = "grey20"),
          strip.text.x.top = element_text(size = facet_font)
    ) +
    ggtitle(paste(toupper(pod), "Overview: Activity Avoidance"))
  )
}

#### 0.6.2.2 Activity Mitigator groupings ----

plot_mitigator_group_transposed <- function(group, facet_cols){
  cat("###", group, "{.hide}", "\n")
  
  colours <- tibble::tibble(
    peer_year = unique(df$peer_year[df$parameter == "activity_avoidance"]),
    colour = scales::hue_pal()(length(unique(df$peer_year[df$parameter == "activity_avoidance"])))) |> 
    dplyr::mutate(new_peer_year = paste("<span style = 'color: ",
                                        colour,
                                        ";'>",
                                        "\u25a0",
                                        "</span>",
                                        " ",
                                        peer_year,
                                        sep = ""))
  
  print(
    df |> 
      dplyr::mutate(strategy = stringr::str_replace_all(strategy,
                                                        "_",
                                                        " ")) |> 
    dplyr::filter(Grouping == group & 
                    parameter == "activity_avoidance") |>
      dplyr::left_join(colours) |>
      dplyr::mutate(peer_year = new_peer_year) |> 
      dplyr::select(-new_peer_year) |> 
    ggplot(aes(x = midpoint, y = peer_year, colour = colour))+
    geom_crossbar(aes(x = percentile50, 
                      xmin = percentile90,
                      xmax = percentile10),
                  fill = "lightgrey",
                  colour = "grey85",
                  alpha = 0.2,
                  width = 0.4)+
    geom_pointrange(aes(x = midpoint, 
                        xmin = value_1,
                        xmax = value_2),
                    size = 0.3,
                    linewidth = 1.2
    )+
      scale_colour_identity()+
    facetious::facet_wrap_strict(~strategy,
                                 
                                 ncol = facet_cols,
                                 nrow = 2,
                                 labeller = label_wrap_gen(width = 20)
    )+
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       labels = c(0, 0.5, 1),
                       limits = c(0,1),
                       name = "Assumption input (0-1)")+
    ylab("Trust Code")+
    theme_bw() + 
    theme(panel.grid.major.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = ggtext::element_markdown(),
          legend.position = "none",
          panel.spacing = unit(0.2, "lines"),
          panel.background = element_rect(fill = NA, color = "grey20"),
          strip.text.x.top = element_text(size = 7)
    )+
    ggtitle(group)
  )
}



#### 0.6.3.1 Efficiencies POD----
plot_efficiencies_pod_transposed <- function(pod){
  
  cat("\n")
  cat("###", pod, "{.hide}", "\n")
  
  if(pod == "ip"){
    facet_cols = 11
    wrap_width = 17
    facet_font = 6
  }else{
    facet_cols = 8
    wrap_width = 20
    facet_font = 7
  }
  
  
  
  colours <- tibble::tibble(
    peer_year = unique(df$peer_year[df$parameter == "efficiencies"]),
    colour = scales::hue_pal()(length(unique(df$peer_year[df$parameter == "efficiencies"])))) |> 
    dplyr::mutate(new_peer_year = paste("<span style = 'color: ",
                                        colour,
                                        ";'>",
                                        "\u25a0",
                                        "</span>",
                                        " ",
                                        peer_year,
                                        sep = ""))
  
  print(
    df |> 
      dplyr::filter(activity_type == pod & 
                      parameter == "efficiencies") |> 
      dplyr::mutate(strategy = stringr::str_replace_all(strategy,
                                                        "_",
                                                        " ")) |>
      dplyr::left_join(colours) |>
      dplyr::mutate(peer_year = new_peer_year) |> 
      dplyr::select(-new_peer_year) |> 
      ggplot(aes(x = midpoint, y = peer_year, colour = colour))+
      geom_crossbar(aes(x = percentile50, 
                        xmin = percentile90,
                        xmax = percentile10),
                    fill = "lightgrey",
                    colour = "grey85",
                    alpha = 0.2,
                    width = 0.4)+
      geom_pointrange(
        aes(x = midpoint, 
            xmin = value_1,
            xmax = value_2),
        size = 0.3,
        linewidth = 1.2
      )+
      scale_colour_identity()+
      facet_wrap(~ strategy,
                 
                 #nrow = 2,
                 labeller = label_wrap_gen(width = wrap_width),
                 #ncol = facet_cols
      )+
      scale_x_continuous(breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1),
                         limits = c(0,1),
                         name = "Assumption input (0-1)"
                         
                         
      )+
      ylab("Trust Code")+
      theme_bw() + 
      theme(panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = ggtext::element_markdown(),
            legend.position = "none",
            panel.spacing = unit(0.2, "lines"),
            panel.background = element_rect(fill = NA, color = "grey20"),
            strip.text.x.top = element_text(size = facet_font)
      ) +
      ggtitle(paste(toupper(pod), "Overview: Efficiencies"))
  )
  
}
#### 0.6.3.2 Efficiencies Mitigtor groupings ----

plot_efficiencies_mitigator_group <- function(group, facet_cols){
  cat("###", group, "{.hide}", "\n")
  
  colours <- tibble::tibble(
    peer_year = unique(df$peer_year[df$parameter == "efficiencies"]),
    colour = scales::hue_pal()(length(unique(df$peer_year[df$parameter == "efficiencies"])))) |> 
    dplyr::mutate(new_peer_year = paste("<span style = 'color: ",
                                        colour,
                                        ";'>",
                                        "\u25a0",
                                        "</span>",
                                        " ",
                                        peer_year,
                                        sep = ""))
  
  print(
    df |> 
      dplyr::mutate(strategy = stringr::str_replace_all(strategy,
                                                        "_",
                                                        " ")) |> 
      dplyr::filter(Grouping == group & 
                      parameter == "efficiencies") |>
      dplyr::left_join(colours) |>
      dplyr::mutate(peer_year = new_peer_year) |> 
      dplyr::select(-new_peer_year) |> 
      ggplot(aes(x = midpoint, y = peer_year, colour = colour))+
      geom_crossbar(aes(x = percentile50, 
                        xmin = percentile90,
                        xmax = percentile10),
                    fill = "lightgrey",
                    colour = "grey85",
                    alpha = 0.2,
                    width = 0.4)+
      geom_pointrange(aes(x = midpoint, 
                          xmin = value_1,
                          xmax = value_2),
                      size = 0.3,
                      linewidth = 1.2
      )+
      scale_colour_identity()+
      facetious::facet_wrap_strict(~strategy,
                                   ncol = facet_cols,
                                   nrow = 2,
                                   labeller = label_wrap_gen(width = 20)
      )+
      scale_x_continuous(breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1),
                         limits = c(0,1),
                         name = "Assumption input (0-1)")+
      ylab("Trust Code")+
      theme_bw() + 
      theme(panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = ggtext::element_markdown(),
            legend.position = "none",
            panel.spacing = unit(0.2, "lines"),
            panel.background = element_rect(fill = NA, color = "grey20"),
            strip.text.x.top = element_text(size = 7)
      )+
      ggtitle(group)
  )
}

####
## 0.7 Mitigator look up DT----

make_mitigator_lookup <- function(){
  mitigator_groups |>
    dplyr::filter(!`Mitigator type` == "Efficiencies") |> 
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                factor)) |> 
    #dplyr::select(-`Strategy variable`) |> 
    DT::datatable(options = list(dom = 'ft',
                                 pageLength = nrow(mitigator_groups)),
                  filter = "top")
}

## 0.8 raw data DT ----

make_df_dt <- function(){
  df |> 
    dplyr::mutate(dplyr::across(c(activity_type,
                                  strategy,
                                  time_profile,
                                  parameter,
                                  peer,
                                  peer_year,
                                  `Mitigator code`,
                                  Grouping,
                                  point_or_range),
                                factor)) |> 
    DT::datatable(extensions = 'Buttons',
                  options = list(dom = 'Bft',
                                 pageLength = nrow(df),
                                 buttons = list( 
                                   list(extend = "csv",   
                                        filename = "nhp_inputs_raw_data",
                                        text = "Download Raw Data (.csv)")
                                   )
                                 ),
                  filter = "top",
                  rownames = F)
}

## 0.9 make binary table ----

selected_activity_mitigators <- extracted_params |> 
  dplyr::select(-time_profile)

sample_activity_params <- jsonlite::fromJSON("data/all_params.json")$params |>
  report_params_table_activity_avoidance() |>
  dplyr::select(-c(time_profile, value_1, value_2, peer,
                   baseline_year, horizon_year))

activity_mitigators <- sample_activity_params |>
  dplyr::left_join(
    selected_activity_mitigators,
    by = c("activity_type", "strategy", "parameter"))

all_schemes <- unique(activity_mitigators$peer_year)



generate_binary_table_all(activity_mitigators, 
                          all_schemes)

# 0.10 hosp code lookup ---- 

trust_code_lookup <- readxl::read_excel("data/NHP_trust_code_lookup.xlsx")

make_trust_code_lookup <- function(){
  
 df |> 
    dplyr::select(peer,
                  peer_year,
                  baseline_year,
                  horizon_year) |> 
   dplyr::filter(!is.na(peer)) |> 
    dplyr::group_by(
      dplyr::pick(tidyselect::everything())) |> 
    dplyr::summarise(tmp = dplyr::n()) |> 
    dplyr::select(-tmp) |> 
    dplyr::left_join(trust_code_lookup |> 
                       dplyr::select(`Name of Trust`,
                                     `Trust ODS Code`,
                                     `Name of Hospital site`),
                     by = c(peer = "Trust ODS Code")) |> 
    dplyr::select(peer, `Name of Trust`, tidyselect::everything()) |> 
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                factor)) |>
      DT::datatable(options = list(dom = 'ft',
                                   pageLength = nrow(mitigator_groups)),
                    filter = "top")
      
}

#//--SCRAP--\\ ----

# 1.0 original plots ----




## 1.1 pod overview ----

# 1.2 mitigator grouping ----



# 2.0 transposed plots ----

## 2.1 pod overview ----

## 2.2 mitigator grouping ----

