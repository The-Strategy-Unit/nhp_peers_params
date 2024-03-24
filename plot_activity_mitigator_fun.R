library(ggplot2)
# plotting function ----

make_activity_mitigator_graphs <- function(activity_type,
                                           strategy,
                                           time_profile,
                                           parameter,
                                           percentile10,
                                           percentile90,
                                           data,
                                           highlighted_scheme){
  
  scheme <- highlighted_scheme
  graph_title <- paste(activity_type,
                      strategy,
                      time_profile,
                      sep = ": ")
 p <-  data |> 
    dplyr::ungroup() |> 
    dplyr::arrange(desc(value_type), 
                  value) |> 
    dplyr::mutate(order = ifelse(value_type == "midpoint", 
                                 dplyr::row_number(),
                                 NA),
                  peer = forcats::fct_reorder(peer, order, .na_rm = T)) |> 
      
    ggplot(aes(peer, value, colour = value_type))+
    
    geom_rect(xmin = -Inf, 
              xmax = Inf,
              ymin = -Inf,
              ymax = percentile90,
              colour = "grey50",
              fill = "grey50")+
    
    geom_rect(xmin = -Inf, 
              xmax = Inf,
              ymin = percentile10,
              ymax = Inf,
              colour = "grey50",
              fill = "grey50")+
    
     geom_hline(aes(yintercept = percentile10),
               colour = "black"
    )+
    geom_hline(aes(yintercept = percentile90),
               colour = "black"
    )+
    
   
    
    geom_point(data = ~dplyr::filter(.x, value_type %in% c("lo", "hi")),
               shape = "-",
               size = 5
    )+
    geom_hline(aes(yintercept = mean_val,
                   colour = value_type),
               alpha = 0.5
    )+
    
    geom_point(data = ~dplyr::filter(.x, value_type == "midpoint" &
                                       peer != scheme),
               colour = "black")+
    geom_point(data = ~dplyr::filter(.x, value_type == "midpoint" &
                                       peer == scheme),
               colour = "red")+
    
    geom_line(aes(group = peer),
              alpha = 0.5,
              colour = "black") +
    scale_x_discrete(drop = F)+
    scale_y_continuous(sec.axis = sec_axis(~.,
                                           breaks = c(percentile10,
                                                      percentile90),
                                           labels = c("NEE percentile10",
                                                      "NEE percentile90")))+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    
    #geom_text(aes(0, percentile10, label = "percentile10"))+

    ggtitle(graph_title)
  
 print(p)
 
}
