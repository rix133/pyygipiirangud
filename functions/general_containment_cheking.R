# only functions that return one boolean value


#' Does a list contain an exact string match 
#'
#' @param kalalist list of string to check 
#' @param kala  string to test 
#'
#' @return
#' @export
#'
#' @examples
detectfish<-function(kalalist, kala){
  b <- paste0("^",kala,"$")
  c <-grep(b, unlist(kalalist))
  if(length(c)>0){d<-TRUE}else{d<-FALSE}
  d
  
}

#' Is Any Smaller
#'
#' @param vect numeric vector of values 
#' @param x max value
#'
#' @return logical
#' @export
#'
#' @examples
any_smaller_than_x <- function(vect, x){
  vect <- as.numeric(vect)
  if(any(vect < x, na.rm = T)) return(TRUE)
  return(FALSE)
}


#' Check if statistics contain significant diffrences
#'
#' @param summary_obj 
#' @param pmax 
#'
#' @return
#' @export
#'
#' @examples
has_significant_differences <- function(summary_obj, pmax){
  
  if("summary.aov" %in% class(summary_obj)){
    coef_ps <-summary_obj[[1]]$`Pr(>F)`
    return(any_smaller_than_x(coef_ps, pmax))
  }
  if("summary.lm" %in% class(summary_obj)){
    coef_ps <-summary_obj$coefficients[2:nrow(summary_obj$coefficients),4]
    return(any_smaller_than_x(coef_ps, pmax))
  }
  
  stop(paste(class(summary_obj), "not implemented! see: general_containment_cheking.R"))
}