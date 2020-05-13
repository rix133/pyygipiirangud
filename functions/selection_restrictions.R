getMinMaxDates <- function(input){
  res<-list(min="2018-01-01", max="2019-12-31")
  if(input$columns2Use == "gearCount"){
    res$max <- "2018-12-31" 
  }
  return(res)
}