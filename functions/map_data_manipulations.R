#' Find neighboring areas within sf data frame 
#'
#' @param pyygiruudud_3301 an sf data frame
#'
#' @return area naighbors data frame
#' @export
#'
#' @examples
adjaent_areas<-function(pyygiruudud_3301){
  kylgnevad <-sf::st_intersects(pyygiruudud_3301,pyygiruudud_3301)
  n<-nrow(pyygiruudud_3301)
  pr_id<-character(n)
  neighbors<-vector("list", length=n)
  for(i in 1:n){
    pr_id[i] <- (pyygiruudud_3301[i,]$pr_id)
    neighbors[[i]] <-purrr::map(kylgnevad[[i]], function(x) pyygiruudud_3301$pr_id[row.names(pyygiruudud_3301)==x])
    
  }
  ruudumap<-data.frame(pr_id=pr_id)
  ruudumap$neighbors<-neighbors
  return(ruudumap)
}


#' Add neighboring areas to sf data frame
#'
#' @param pyygiruudud 
#'
#' @return
#' @export
#'
#' @examples
lisa_kylgnevad_ruudud<- function(pyygiruudud){
  pyygiruudud$kylgnevad_ruudud <- NA
  for(i in 1:nrow(pyygiruudud)){
    ruudud <- sf::st_intersection(pyygiruudud[i,],pyygiruudud)$PR_NR_INT
    if(length(ruudud)>0){
      pyygiruudud[i,]$kylgnevad_ruudud<-list(ruudud)
    }
    
  }
  return(pyygiruudud)
}