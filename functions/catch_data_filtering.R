

#source("./functions/plot_on_map.R") # for plotting

#' Filter a data frame 
#' 
#' The function filters out rows from the data frame that intersect with the period searched for
#' if the month_only flag is true all catches whose net in date falls within the period 
#' 
#' @param df a data frame that has columns c("net_in_date", "net_out_date", "month", "species")
#' @param fish 
#' @param startdate 
#' @param enddate 
#' @param month_only filter only acoording to month field on a data frame
#'
#' @return
#' @export
#'
#' @examples
fish_dates_catch <- function(df,  fish = NULL,
                             startdate = "2018-01-01",
                             enddate = "2018-12-31",
                             month_only = TRUE,
                             nights = TRUE
){
  #filter by fish
  if(!is.null(fish)){
    df <- df[df$species %in% c(tolower(fish)),]
    if(nrow(df)==0){
      return(df)
    }
  }
 
  
  #remove NA rows
  if(!month_only & (any(is.na(df$net_in_date)) | any(is.na(df$net_out_date)))){
    naRows <- nrow(df[is.na(df$net_in_date) | is.na(df$net_out_date),])
    warning(paste("Removing", naRows,"NA rows from data for: ", fish, "\n"))
    
    df <- df[!(is.na(df$net_in_date) | is.na(df$net_out_date)),]
    
  }  
  
  sd<-lubridate::ymd(startdate)
  ed<-lubridate::ymd(enddate)
  #check if startdate is before enddate if not make two periods
  #this is useful for using restrictions that start at eg autumn and end at spring
  if(ed-sd < 0){
    fullYear<- makeFullYearDates(lubridate::year(sd))
    period1<-fish_dates_catch(df, fish = fish, startdate = fullYear$startDate,
                              enddate = sd, month_only)
    period2<-fish_dates_catch(df, fish = fish, startdate = ed, 
                              enddate = fullYear$endDate, month_only)
    return(rbind(period1,period2))
  }
  
  edMO <- ed
  
  if(nights){
    pyyk<- lubridate::interval(df$net_in_date, df$net_out_date)
    df$catchTime <- df$catchNights
  }
  else{
    ed <- ed + lubridate::days(1)
    newOut <- df$net_out_date
    newOut <- newOut + lubridate::days(1)
    pyyk<- lubridate::interval(df$net_in_date, newOut)
    df$catchTime <- df$catchNights + 1
  }
  
  
  #this is a special case in where now night day difference is made
  #TODO reduce DRY
  if(month_only){
    #set all net in and out dates to be full month
    df$net_in_date <- lubridate::ymd(paste0(df$year,"-",df$month,"-1"))
    df$net_out_date <- df$net_in_date
    lubridate::day(df$net_out_date) <- lubridate::days_in_month(df$net_out_date)
    
    pyyk<- lubridate::interval(df$net_in_date, df$net_out_date)
    period <- lubridate::interval(sd,edMO)
    
    df$intersections <- lubridate::intersect(pyyk, period)
    
    x<-df[!is.na(df$intersections),]
    x$proportionWithin<- 1
    return(x)
  }
  
  
  period <- lubridate::interval(sd,ed)
  #check if there is any intersection
  df$intersections <- lubridate::intersect(pyyk, period)
  x<-df[!is.na(df$intersections),]
  
  x$proportionWithin<-lubridate::int_length(x$intersections) / 
      (as.numeric(x$catchTime)*3600*24)
  res<-x[x$proportionWithin > 0, ] 

  
  return(res)
}


makeFullYearDates<- function(year){
  list(startDate = paste0(year,"-01","-01"),
       endDate = paste0(year,"-",12,"-",31))
}


#' Kala püük  kuupäevavahemikus
#' 
#' Funktsioon võtab sisse liigi, ja püügiandmed ja tagastab kui palju on nendest aladest sellel
#' ajavahemikul kalu püütud. Samas kui määrata mõni teine väli siis annab selle summa.
#'
#' @param kalaliik kala nimetus kelle kohta soovid püügiandmeid
#' @param catchdata 
#' @param s_date 
#' @param e_date 
#' @param map_data
#' @param sumBy -millist tulpa kasutada kaardil sumeerimiseks (vaikimisi landing_kg) 
#' @param ... parametter saadedake edasi plot_pyyk funktsiooni 
#'
#' @return püügiandmed antud ajal piirangu all olevates alades valitud kalaliigil 
#' @export
#'
#' @examples
pyyk_vahemikus <- function(catchdata, 
                           kalaliik,
                           sumBy = "landing_kg",
                           month_only = F, 
                           ...){
  
  if(!is.data.frame(catchdata)){stop("missing catch data input data frame!")}
    x <- fish_dates_catch(catchdata, fish = kalaliik, month_only = month_only, ...)
    x[[sumBy]] <- as.numeric(x[[sumBy]])
    if(sumBy == "gearCount"){
      x[[sumBy]] <- x[[sumBy]] * x$catchTime
    }
    pyyk_alades<- aggregate(x[sumBy] * x$proportionWithin, by=x[,c("pr_id", "species")], sum)
    pyyk_alades$total <- pyyk_alades[[sumBy]]
    
    return(pyyk_alades)
    
 
}


