#' Save fish maps to plots
#'
#' @param piirangud_all_plots list of values as comes from function
#'  "pyyk_vahemike_kaupa"
#' @param dir directory
#' @param prefix  filename
#' @param ext 
#'
#' @return
#' @export
#'
#' @examples
save_fish_plots_to_files<- function(piirangud_all_plots,
                               dir = "imgs",
                               prefix = "piirang-",
                               ext = ".png"){
  #check if main dir exists an create if not
  dir.create(file.path("./results", dir), showWarnings = FALSE)
  #save the plots to files
    kalad <- names(piirangud_all_plots)
    for(kala in kalad){
      dir.create(file.path(paste0("./results/",dir), kala), showWarnings = FALSE)
      
      for(i in 1:length(piirangud_all_plots[[kala]])){
        fname <- paste0("./results/",
                        dir,"/",
                        kala, "/",
                        prefix,
                        i,
                        ext)
        m1 <- piirangud_all_plots[[kala]][[i]][["plot"]]
        
        tryCatch({
          ggplot2::ggsave(fname, m1,
                          width = 14, height = 7)
        }, error=function(err){
          warning(paste("Can't save as image ",
                        kala, " at ", 
                        names(piirangud_all_plots[[kala]][[i]]),"\n"))
        })
      }
      
    }
}





#' Save map and boxplot combined plots
#'
#' @param piirangud_all_plots list of values as comes from function
#'  "join_restriction_plots"
#' @param dir directory
#' @param prefix  filename
#' @param ext 
#'
#' @return
#' @export
#'
#' @examples
save_plots_to_files<- function(piirangud_all_plots,
                              dir = "imgs",
                              prefix = "piirang-",
                              ext = ".png"){
  #check if main dir exists an create if not
  dir.create(file.path("./results", dir))
  #save the plots to files
  for(i in 1:length(piirangud_all_plots)){
    kalad <- names(piirangud_all_plots[[i]])
    for(kala in kalad){
      dir.create(file.path(paste0("./results/",dir), kala), showWarnings = FALSE)
      fname <- paste0("./results/",
                      dir,"/",
                      kala, "/",
                      prefix, 
                      names(piirangud_all_plots[i]), ext)
      m1 <- piirangud_all_plots[[i]][[kala]][["map"]]
      s1 <- piirangud_all_plots[[i]][[kala]][["box"]]
      
      tryCatch({
        ggplot2::ggsave(fname, gridExtra::grid.arrange(m1, s1 , ncol=2),
                       width = 14, height = 7)
      }, error=function(err){
        warning(paste("Can't save as image ",
                      kala, " at ", 
                      names(piirangud_all_plots[i]),"(restriction)\n"))
      })
      
    }
  }
}


#' Save map and boxplot combined plots
#'
#' @param piirangud_all_plots list of values as comes from function
#'  "join_restriction_plots"
#' @param dir directory
#' @param prefix  filename
#' @param plotType string map or box for the selected plot to save
#' @param ext
#' @param fig.width 
#' @param  fig.height 
#'
#' @return
#' @export
#'
#' @examples
save_selected_plots_to_files<- function(piirangud_all_plots,
                                      plotType = "map",
                              dir = "./results/imgs/",
                              prefix = "piirang-",
                              ext = ".png",
                              fig.width = 7,
                              fig.height = 7){
  #save the plots to files
  for(i in 1:length(piirangud_all_plots)){
    kalad <- names(piirangud_all_plots[[i]])
    for(kala in kalad){
      fname <- paste0(dir,
                      kala, "/",
                      plotType, "-",
                      prefix, 
                      names(piirangud_all_plots[i]), ext)
      plot <- piirangud_all_plots[[i]][[kala]][[plotType]]
      tryCatch({
        ggplot2::ggsave(fname, plot, width = fig.width, height = fig.height)
      }, error=function(err){
        warning(paste("Can't save as image ",
                      kala, " at ", 
                      names(piirangud_all_plots[i]),"(restriction)\n"))
      })
      
    }
  }
}

