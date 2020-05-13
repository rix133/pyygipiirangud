
#' Compare species catch between areas
#'
#' @param piirangu_pyyk_alades data
#' @param arvesta_pindala  take the area of the location also into account
#'
#' @return a summary of the statistics
#' @export
#'
#' @examples
pyygi_erinevuse_stat <- function (piirangu_pyyk_alades, arvesta_pindala = F){
  
  if(length(unique(piirangu_pyyk_alades$species))>1)stop("More than one species in data!")
  
  piirangu_pyyk_alades$landing_kg_per_area<-piirangu_pyyk_alades$landing_kg / piirangu_pyyk_alades$Shape_Area 
  
  if(arvesta_pindala){ 
    valem <- as.formula("landing_kg_per_area ~ pr_id")
    }
  
  else{valem <- as.formula("landing_kg ~ pr_id")}
  
  piirangu_pyyk_alades$pr_id <- as.factor(piirangu_pyyk_alades$pr_id)
  pyygi_stat <- aov(valem, data = piirangu_pyyk_alades) 
  summary(pyygi_stat)
}

#' Compare species catch between areas for multiple species
#'
#' @param piirangu_pyyk_alades data
#' @param arvesta_pindala  take the area of the location also into account
#'
#' @return a summary of the statistics
#' @export
#'
#' @examples
pyygi_erinevuse_stat_by_species <- function (piirangu_pyyk_alades, arvesta_pindala = F){
  res <- list()
  fishes<-unique(piirangu_pyyk_alades$species)
  for(fish in fishes){
    df <- piirangu_pyyk_alades[piirangu_pyyk_alades$species == fish,]
    if(length(unique(df$pr_id))>1){
      res[[fish]] <- pyygi_erinevuse_stat(df, arvesta_pindala)
    }
    
  }
  return(res)
  
}
