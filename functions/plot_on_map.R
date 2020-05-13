

#' Plot all of the catch data provided
#'
#' @param pyyk_ruutudes 
#' @param map_data 
#'
#' @return
#' @export
#'
#' @examples
plot_pyyk <- function(pyyk_ruutudes,
                      map_data,
                      estonia_map,
                      algus_kuup,
                      lopp_kuup,
                      text_size = 3,
                      fill_unit = "t",
                      fill_title = "Catch"){
  
  
  pyygiruudud_3301 <- map_data$pyygiruudud_3301
  
  if(fill_unit =="t"){
    divider <- 1000
  }
  else {
    divider <- 1
  }

  
  ruudud <- pyygiruudud_3301[pyygiruudud_3301$pr_id %in% pyyk_ruutudes$pr_id,]
  ruudud <- sp::merge(ruudud, pyyk_ruutudes)
  
  estonia_map + 
    ggspatial::geom_sf(data = ruudud, ggplot2::aes(fill = total/divider) ) +
    ggplot2::geom_sf_text(ggplot2::aes(label=pr_id), data = ruudud, size = text_size  ) +
    ggplot2::labs(subtitle = paste0(algus_kuup, " kuni ", lopp_kuup),
         title =    paste0(unique(pyyk_ruutudes$species), collapse = ", "),
         fill = paste0(fill_title, " (", fill_unit, ")"))
  
}


#' Create a ggplot base map 
#'
#' @param pyygiruudud_3301 
#' @param county 
#' @param cropbox 
#'
#' @return
#' @export
#'
#' @examples
make_base_map <- function(pyygiruudud_3301,
                          county,
                          add_catch_area_outline = T,
                          add_catch_area_nr = T,
                          text_size = 2){
  
  if(text_size == 0){
    add_catch_area_nr = FALSE
    add_catch_area_outline = FALSE
  }
  
  # plot the layer
  estonia_map <-ggplot2::ggplot()+
    ggplot2::scale_fill_gradientn(colours = c("palegreen", "lightyellow", "orange", "red")) +
    ggspatial::geom_sf(data=county, color = "gray")
  
  if(add_catch_area_outline){
    estonia_map <- estonia_map +
      ggspatial::geom_sf(data = pyygiruudud_3301, colour="gray", fill=ggplot2::alpha("lightblue",0.0)) 
  }
  
  if(add_catch_area_nr){
    estonia_map <- estonia_map +
      ggplot2::geom_sf_text(ggplot2::aes(label=pr_id), data = pyygiruudud_3301, size =text_size  )
  }
  
  estonia_map <- estonia_map +
    ggspatial::annotation_scale(location = "bl", style = "ticks") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue"),
          panel.grid = ggplot2::element_line(color = "lightblue")
    ) +
    ggplot2::labs(x=ggplot2::element_blank(),y=ggplot2::element_blank())
  return(estonia_map)
}
