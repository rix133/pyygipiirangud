# only functions that return a vector


#' Replace year in a date vector
#'
#' @param date_var a date
#' @param year new year, default 2018 
#'
#' @return
#' @export
#'
#' @examples
bring_to_one_year <- function(date_var,year = "2018" ){
  
  for(i in 1990:2020){
    date_var<- as.Date(gsub(paste0(i,"-"),"2018-",as.character(date_var)))
  }
  return(date_var)
}


unlist_string_column<-function(col, sep = ", "){
  lapply(col, strsplit, sep)
  
}
