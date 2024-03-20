library(ggplot2)
# plotting function ----

make_activity_mitigator_graphs <- function(activity_type,
                                           strategy,
                                           time_profile,
                                           parameter,
                                           data,
                                           highlighted_scheme){
  
  scheme <- highlighted_scheme
  graph_title <- paste(activity_type,
                      strategy,
                      time_profile,
                      sep = ": ")
  data |> 
    ggplot(aes(peer, value, colour = value_type))+
    
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
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    scale_x_discrete(drop = F)+
    ggtitle(graph_title)
  
}