make_activity_mitigator_graphs <- function(
    activity_type,
    strategy,
    time_profile,
    parameter,
    percentile10,
    percentile90,
    data,
    highlighted_scheme,
    all_schemes
) {
  
  all_schemes <- unlist(all_schemes)
  scheme <- highlighted_scheme
  scheme_val <- data$value[data$peer == scheme & data$value_type == "Midpoint"][1]
  
  graph_title <- paste(
    activity_type,
    strategy,
    time_profile,
    sep = ": "
  )
  
  no_value_schemes <- all_schemes[!all_schemes %in% data$peer]
  
  # data <- data |>
  #   dplyr::ungroup() |>
  #   dplyr::arrange(desc(value_type),
  #                  value,
  #                  peer) |>
  #   dplyr::mutate(order = ifelse(value_type == "Midpoint",
  #                                dplyr::row_number(),
  #                                               NA),
  #                                peer = forcats::fct_reorder(peer, order, .na_rm = F)) |>
  #   dplyr::add_row(peer = scheme,
  #                  value_type = "Highlighted Scheme",
  #                  value = scheme_val,
  #                  shape_type = "point")
  #
  
  d <- data |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::desc(value_type),
      value,
      peer
    ) |>
    dplyr::add_row(
      peer = scheme,
      value_type = "Highlighted Scheme",
      value = scheme_val,
      shape_type = "point"
    ) |>
    dplyr::add_row(peer = no_value_schemes) |>
    dplyr::mutate(
      order = ifelse(
        value_type %in% c("Midpoint"),
        dplyr::row_number(),
        NA
      ),
      peer = forcats::fct_rev(
        forcats::fct_reorder(
          peer,
          rev(order),
          .na_rm = FALSE
        )
      )
    ) 
  
  p <- d |>
    ggplot2::ggplot(ggplot2::aes(
      peer,
      value,
      colour = value_type,
      shape = shape_type
    )) +
    ggplot2::geom_rect(
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = percentile90,
      colour = "grey50",
      fill = "grey50"
    ) +
    ggplot2::geom_rect(
      xmin = -Inf,
      xmax = Inf,
      ymin = percentile10,
      ymax = Inf,
      colour = "grey50",
      fill = "grey50"
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = percentile10),
      colour = "black"
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = percentile90),
      colour = "black"
    ) +
    ggplot2::geom_point(
      data = ~ dplyr::filter(.x, value_type %in% c("Low", "High")),
      ggplot2::aes(colour = value_type),
      size = 5,
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(
        yintercept = mean_val,
        colour = value_type
      ),
      show.legend = F,
      alpha = 0.5,
      size = 2,
    ) +
    ggplot2::geom_point(
      data = ~ dplyr::filter(
        .x,
        value_type == "Midpoint" &
          peer != scheme
      ),
      # colour = "black",
      size = 1.5
    ) +
    ggplot2::geom_point(
      data = ~ dplyr::filter(.x, value_type == "Highlighted Scheme"),
      colour = "red",
      size = 1.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = peer),
      alpha = 0.5,
      colour = "black"
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        ~.,
        breaks = c(
          percentile10,
          percentile90,
          d$mean_val
        ),
        labels = c(
          "NEE percentile10",
          "NEE percentile90",
          d$mean_type
        )
      )
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_shape_manual(
      values = c(16, 95),
      breaks = c("point", "line"),
      labels = NULL,
      guide = "none",
      drop = FALSE
    ) +
    ggplot2::scale_colour_manual(
      name = "Value",
      values = c(
        scales::hue_pal()(3)[1],
        scales::hue_pal()(3)[3],
        scales::hue_pal()(3)[2],
        "red"
      ),
      breaks = c(
        "High",
        "Midpoint",
        "Low",
        "Highlighted Scheme"
      ),
      drop = FALSE
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(95, 16, 95, 16),
          size = c(5, 1.5, 5, 1.5)
        )
      )
    )
  
  # geom_text(aes(0, percentile10, label = "percentile10"))+
  
  ggplot2::ggtitle(graph_title)
  
  print(p)
  
}
