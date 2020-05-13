#' Restriction Catch Evaluation 
#'
#' @param piirangunr  number of the restriction as in KalaDB
#' @param kalaDB  df of restrictions
#' @param catchdata df of all catch data 
#' @param ruudu_naabrid df specifing the adjuent areas of each catch square
#' @param map_data list of values required for map plotting
#' @param filter_catchment should the catch data include only the restricted
#'  "pyygivahendid"
#' @param arvesta_pindala take the area into account when calcuating landings
#' @param crop_map 
#' @param monthOnly use only monthly data (his has more data in it 
#' but gives catch with month accuaracy)
#' @param pmax  the p value to sort out the significantly diffrenta areas
#'
#' @return a list of plots and statistics for the restriction area
#' @export
#'
#' @examples
restriction_catch_by_species<-function(piirangunr,
                                       kalaDB,
                                       catchdata,
                                       ruudu_naabrid,
                                       map_data,
                                       filter_catchment = T,
                                       arvesta_pindala = F,
                                       crop_map = T,
                                       monthOnly = F,
                                       pmax = 0.05){
  
  alapiirang<-kalaDB[kalaDB$piirangu_id == piirangunr, ]
  
  if(filter_catchment){
    catchdata<-filter_catch_by_catchment(kalaDB, catchdata, piirangunr)
  }
  
  alainfo_piirang <- extract_catch_differences_by_species(alapiirang,
                                                          catchdata,
                                                          month_only = monthOnly,
                                                          piirangu_lahistel = TRUE,
                                                          ruudu_naabrid = ruudu_naabrid,
                                                          pmax = pmax,
                                                          arvesta_pindala = arvesta_pindala)
  piirangukala_pyyk <- pyyk_piirangu_ajal(alapiirang, catchdata,
                                          month_only = monthOnly,
                                          sum_pyyk = F)
  
  piirangukala_pyyk <- piirangu_pyyk_korvalruutudes(alapiirang,
                                                    piirangukala_pyyk,
                                                    ruudu_naabrid)
  
  maps<-list()
  maps[[as.character(piirangunr)]] <-  
    get_piirangu_pyyk_by_fish_plots(alapiirang, alainfo_piirang,
                                    map_data, crop = crop_map)
  
  boxes<-list()
  boxes[[as.character(piirangunr)]] <- 
    get_piirangu_pyyk_by_fish_stats_plots(alapiirang, piirangukala_pyyk)
  
  
  maps_and_boxes <- join_restriction_plots(maps,boxes)
  stats<-list()
  stats[[as.character(piirangunr)]]<- 
    pyygi_erinevuse_stat_by_species(piirangukala_pyyk)
  
  
  return(list(plots=maps_and_boxes, stats = stats))
}