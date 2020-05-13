#source("./functions/general_containment_cheking.R")

#' Filter a data frame by date
#'
#' filter out 
#'
#' @param df a data frame
#' @param startdate_filedname
#' @param enddate_fieldname 
#' @param fish list of strings to keep
#' @param startdate periond start as string (ymd)
#' @param enddate  period end as string (ymd)
#'
#' @return
#' @export
#'
#' @examples
fish_dates_restrictions <- function(df, startDateFieldName, endDateFieldName, fish = list(),
                                    startdate = "2018-01-01",
                                    enddate = "2018-12-31",
                                    filterType = "intersect"
){
  #TODO deal with diffrent date formats
  #accepts lubridate now
  sd<-lubridate::ymd(startdate)
  ed<-lubridate::ymd(enddate)
  period <- lubridate::interval(sd,ed)
  piirang<- lubridate::interval(df[, c(startDateFieldName)], df[,c(endDateFieldName)])
 
  if(filterType=="intersect"){
    keep <- !is.na(lubridate::intersect(piirang, period))
    res<-df[keep,]
  }
  if(filterType=="within"){
    res<-df[lubridate::`%within%`(piirang,period),]
  }
  #print(nrow(res))
  if(nrow(res)>0){
    res <- res[sapply(res$keelu_kalad, detectfish, fish),]
  }
  res
}

getCatchmentClass <- function(catchment, showWarnings = T){
  lokspyynised<- c("ääremõrd suu kõrgusega kuni 1 m",
                   "ääremõrd suu kõrgusega 1-3 m",
                   "ääremõrd suu kõrgusega 1–3 m",
                   "avaveemõrd",
                   "kastmõrd",
                   "rivimõrd", 
                   "mõrd",
                   "mõrrajada",
                   "mõrd mõrrajadas",
                   "lõkspüünis",
                   "ääremõrd",
                   "rivimõrd ehk angerjarüsa",
                   "kastmõrd ehk seisevnoot ehk kakuam",
                   "kastmõrd" , "seisevnoot" , "kakuam",
                   "juhtaiata mõrd", "jõemõrd" 
                   )
  kurnpyynised <- c("veonoot", "põhjanoot", "kurnpüünis", "pöörinoot")
  traalpyynised <- c("agariku tragi", "traalnoot", 
                     "pelaagiline traalnoot", "põhjatraalnoot")
  ongpyynised <- c("õngejada",
                   "õngejada (> 100 konksu)",
                   "õngejada (kuni 100 konksu)",
                   "õngpüünis")
  vorkpyynised <- c("nakke- või raamvõrk",
                    "nakkevõrk",
                    "raamvõrk",
                    "seisevvõrk",
                    "võrgujada",
                    "ankurdatud ujuvvõrk",
                    "triivvõrk",
                    "võrkpüünis",
                    'nakke- või raamvõrk, silm 70–89 mm',
                    'nakke- või raamvõrk, silm 90–149 mm',
                    'nakke- või raamvõrk, silm kuni 69 mm',
                    'nakke- või raamvõrk, silm vähemalt 150 mm'
                    )
  res<- ifelse(catchment %in% lokspyynised, "lõkspüünis", NA)
  res<- ifelse(catchment %in% kurnpyynised, "kurnpüünis", res)
  res<- ifelse(catchment %in% vorkpyynised, "võrkpüünis", res)
  res<- ifelse(catchment %in%  ongpyynised, "õngpüünis", res)
  res<- ifelse(catchment %in%  traalpyynised, "traalpüünis", res)
  
  if(any(is.na(res)) & showWarnings){
    warning("\nCan't determine class for: ")
    warning(unique(catchment[is.na(res)]))
  }
  
  return(res)
}























