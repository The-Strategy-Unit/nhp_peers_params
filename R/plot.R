plot_pod_transposed <- function(dat, pod) {
  
  cat("\n\n###", convert_pod_name(pod), "\n")
  
  if (pod == "ip") {
    facet_cols <- 11
    wrap_width <- 17
    facet_font <- 6
  }
  
  if (pod %in% c("op", "aae")) {
    facet_cols <- 8
    wrap_width <- 20
    facet_font <- 7
  }
  
  colours <- build_peer_year_spans(dat, "activity_avoidance")
  
  dat_prepared <- dat |> 
    dplyr::filter(activity_type == pod & parameter == "activity_avoidance") |> 
    dplyr::mutate(
      strategy = stringr::str_replace_all(strategy, "_", " ")
    ) |>
    dplyr::left_join(colours, by = dplyr::join_by(peer_year)) |>
    dplyr::mutate(peer_year = new_peer_year) |> 
    dplyr::select(-new_peer_year) 
  
  p <- dat_prepared |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = midpoint,
        y = peer_year,
        colour = colour
      )
    ) +
    ggplot2:: geom_crossbar(
      ggplot2::aes(
        x = mean, 
        xmin = percentile90,
        xmax = percentile10
      ),
      fill = "lightgrey",
      colour = "grey85",
      alpha = 0.2,
      width = 0.4
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        x = midpoint, 
        xmin = value_1,
        xmax = value_2
      ),
      size = 0.3,
      linewidth = 1.2
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::facet_wrap(
      ~strategy,
      labeller = ggplot2::label_wrap_gen(width = wrap_width),
      ncol = facet_cols
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0,1),
      name = "80% prediction interval (0 to 1)"
      
    ) +
    ggplot2::ylab("Trust Code")+
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(),
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.2, "lines"),
      panel.background = ggplot2::element_rect(fill = NA, color = "grey20"),
      strip.text.x.top = ggplot2::element_text(size = facet_font)
    ) +
    ggplot2::ggtitle(paste(toupper(pod), "Overview: Activity Avoidance"))
  
  print(p)
  
}

plot_mitigator_group_transposed <- function(dat, group, facet_cols){
  
  cat("\n\n###", group, "\n")
  
  colours <- build_peer_year_spans(dat, "activity_avoidance")
  
  dat_prepared <- dat |> 
    dplyr::mutate(strategy = stringr::str_replace_all(strategy, "_", " ")) |> 
    dplyr::filter(Grouping == group & parameter == "activity_avoidance") |>
    dplyr::left_join(colours, by = dplyr::join_by(peer_year)) |>
    dplyr::mutate(peer_year = new_peer_year) |> 
    dplyr::select(-new_peer_year)
  
  p <- dat_prepared |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = midpoint, 
        y = peer_year,
        colour = colour
      )
    ) +
    ggplot2::geom_crossbar(
      ggplot2::aes(
        x = mean, 
        xmin = percentile90,
        xmax = percentile10
      ),
      fill = "lightgrey",
      colour = "grey85",
      alpha = 0.2,
      width = 0.4
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(x = midpoint, 
                   xmin = value_1,
                   xmax = value_2),
      size = 0.3,
      linewidth = 1.2
    )+
    ggplot2::scale_colour_identity() +
    facetious::facet_wrap_strict(
      ~strategy,
      ncol = facet_cols,
      nrow = 2,
      labeller = ggplot2::label_wrap_gen(width = 20)
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0,1),
      name = "80% prediction interval (0 to 1)"
    ) +
    ggplot2::ylab("Trust Code")+
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(),
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.2, "lines"),
      panel.background = ggplot2::element_rect(fill = NA, color = "grey20"),
      strip.text.x.top = ggplot2::element_text(size = 7)
    ) +
    ggplot2::ggtitle(group)
  
  print(p)
  
  
}

plot_efficiencies_pod_transposed <- function(dat, pod) {
  
  cat("\n\n###", convert_pod_name(pod), "\n")
  
  if (pod == "ip") {
    facet_cols <- 9
    wrap_width <- 17
    facet_font <- 6
  }
  
  if (pod %in% c("op", "aae")) {
    facet_cols <- 8
    wrap_width <- 20
    facet_font <- 7
  }
  
  colours <- build_peer_year_spans(dat, "efficiencies")
  
  dat_prepared <- dat |> 
    dplyr::filter(activity_type == pod & parameter == "efficiencies") |> 
    dplyr::mutate(strategy = stringr::str_replace_all(strategy, "_", " ")) |>
    dplyr::left_join(colours, by = dplyr::join_by(peer_year)) |>
    dplyr::mutate(peer_year = new_peer_year) |> 
    dplyr::select(-new_peer_year) 
  
  p <- dat_prepared |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = midpoint,
        y = peer_year,
        colour = colour
      )
    ) +
    ggplot2::geom_crossbar(
      ggplot2::aes(
        x = percentile50, 
        xmin = percentile90,
        xmax = percentile10
      ),
      fill = "lightgrey",
      colour = "grey85",
      alpha = 0.2,
      width = 0.4
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        x = midpoint, 
        xmin = value_1,
        xmax = value_2
      ),
      size = 0.3,
      linewidth = 1.2
    ) +
    ggplot2::scale_colour_identity()+
    ggplot2::facet_wrap(
      ~strategy,
      labeller = ggplot2::label_wrap_gen(width = wrap_width),
      ncol = facet_cols
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0,1),
      name = "80% prediction interval (0 to 1)"
    ) +
    ggplot2::ylab("Trust Code")+
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(),
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.2, "lines"),
      panel.background = ggplot2::element_rect(fill = NA, color = "grey20"),
      strip.text.x.top = ggplot2::element_text(size = facet_font)
    ) +
    ggplot2::ggtitle(paste(toupper(pod), "Overview: Efficiencies"))
  
  print(p)
  
  
}

plot_efficiencies_mitigator_group <- function(dat, group, facet_cols) {
  
  cat("\n\n###", group, "\n")
  
  colours <- build_peer_year_spans(dat, "efficiencies")
  
  dat_prepared <- dat |> 
    dplyr::mutate(strategy = stringr::str_replace_all(strategy, "_", " ")) |> 
    dplyr::filter(Grouping == group & parameter == "efficiencies") |>
    dplyr::left_join(colours, by = dplyr::join_by(peer_year)) |>
    dplyr::mutate(peer_year = new_peer_year) |> 
    dplyr::select(-new_peer_year) 
  
  p <- dat_prepared |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = midpoint,
        y = peer_year,
        colour = colour
      )
    ) +
    ggplot2::geom_crossbar(
      ggplot2::aes(
        x = percentile50, 
        xmin = percentile90,
        xmax = percentile10
      ),
      fill = "lightgrey",
      colour = "grey85",
      alpha = 0.2,
      width = 0.4
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        x = midpoint, 
        xmin = value_1,
        xmax = value_2
      ),
      size = 0.3,
      linewidth = 1.2
    ) +
    ggplot2::scale_colour_identity() +
    facetious::facet_wrap_strict(
      ~strategy,
      ncol = facet_cols,
      nrow = 2,
      labeller = ggplot2::label_wrap_gen(width = 20)
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0,1),
      name = "80% prediction interval (0 to 1)"
    ) +
    ggplot2::ylab("Trust Code") +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(),
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.2, "lines"),
      panel.background = ggplot2::element_rect(fill = NA, color = "grey20"),
      strip.text.x.top = ggplot2::element_text(size = 7)
    ) +
    ggplot2::ggtitle(group)
  
  print(p)
  
}

convert_pod_name <- function(pod) {
  switch(pod, "aae" = "A&E", "ip" = "Inpatients", "op" = "Outpatients", NULL)
}

build_peer_year_spans <- function(
    dat,
    parameter = c("activity_avoidance", "efficiencies")
) {
  
  peer_years <- dat |> 
    dplyr::filter(parameter == parameter) |> 
    dplyr::distinct(peer_year) |> 
    dplyr::pull()
  
  tibble::tibble(
    peer_year = peer_years,
    colour = scales::hue_pal()(length(peer_years))
  ) |> 
    dplyr::mutate(
      new_peer_year = glue::glue(
        "<span style='color:{colour};'>\u25a0</span> {peer_year}"
      )
    )
  
}
