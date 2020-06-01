#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#functions
source("./functions/plot_on_map.R") # for plotting
source("./functions/catch_data_filtering.R")
source("./functions/map_data_manipulations.R")

source("./functions/general_filtering.R")
source("./functions/general_containment_cheking.R") 
source("./functions/general_replacements.R")



#load requred data and maps 
map_data<-readRDS("./data/map_data.rds")
miniEst <- make_base_map(map_data$pyygiruudud_3301, map_data$estonia_map, F, F)
#load
catchdata<-readRDS("./data/catchdata1819.rds")
piirangud_sf <- readRDS("./data/piirangud1819.rds")
piirangud <- sf::st_drop_geometry(piirangud_sf) 

piirangudFilterNames <- c("valitud perioodiga lõikuvad piirangud"="intersect",
                          "valitud perioodi sisse jäävad piirangud" = "within",
                          "kõik piirangud" = "all")

species <- unique(catchdata$species)
columns2Use <- c("püügikogus" = "landing_kg", 
                 "püügipäevad/ööd" = "catchTime", 
                 "püügivahendid" = "gearCount")
pvs <- c("kõik", unique(catchdata$pvClass))

lng <- list(
    search="Otsi",
    paginate=list(first="Algus", previous="Eelmine", 'next'="Järgmine", last="Viimane"),
    processing="Palun oodake, koostan kuvamiseks nimekirja!",
    lengthMenu="Näita piiranguid _MENU_ kaupa",
    zeroRecords=  "Otsingu vastet ei leitud.",
    info=      "Kuvatud: _TOTAL_ piirangut (_START_-_END_)",
    infoEmpty= "Otsinguvasteid ei leitud ",
    infoFiltered= " - filteeritud _MAX_ piirangu seast.",
    infoPostFix=  "Kõik kuvatud piirangud põhinevad reaalsetel tulemustel.")

dtOptions <- list(
    language = lng,
    pageLength = 10
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kalapüük 2018-2019 aastal"),
    #tags$head(tags$style(HTML('#minimap {width: 400px;}'))),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(id = "sidebar",
            selectInput("liik", "Kalaliik:", c(species)),
            selectInput("col", "Andmed:", c(columns2Use)),
            textOutput("infoText"),
            checkboxInput("nights", "Ainult üleöö püük", value = F),
            selectInput("pv", "Püügivahend:", c(pvs)),
            
            dateRangeInput("dates",
                           "Ajavahemik",
                           start = "2019-01-01",
                           end = "2019-01-31",
                           language = "et",
                           min ="2018-01-01",
                           max = "2019-12-31",
                           separator = " kuni "),
            fluidRow(div(style="float: left;", actionButton("previousMonth",textOutput("toMonthBack"))),
                     div(style="float: right;", actionButton("nextMonth", textOutput("toMonthForward")))),
            checkboxInput("exactDates", "Kuupäeva täpsusega", value = T),
            helpText("Valides kuupäeva täpsusega andemed filtreeritakse välja need read
                     millel puudub püügi algus ja lõppaeg. Pikkade perjoodie summasid
                     on seega täpsem vaadata kuu täpsusega."),
            br(),
            helpText("Sellel pisikaardil saab suurel kaardil valitud ala suurendada. 
                     Selleks joonista hiirega väiksel kaardil soovitud ala.
                     Ala asukoha muutmiseks lohista seda pisikaardil."),
            plotOutput("miniMap", height = 300, brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE)),
            br(),
            
            helpText("Suure kaardi all on tabel, mis kuvab valitud perioodi 
            jäävaid piiranguid. Näha on ainult valitud liigi ja püügivahendi kohta 
            käivaid piiranguid. Tabeli ridadele klõpastes joonistuvad valitud piirang
            või piirangud (valides mitu) kaardile."),
            selectInput("piiranguFilter", "Kuva", piirangudFilterNames, ),
            checkboxInput("aastaringsed", "Kuva ka aastaringseid piiranguid", value = F),
            br(),
            sliderInput("areaSlider", label ="Kuva piirangualasid pindalaga (ha, logaritmitud skaala):",
                        min = 0, max = round(log10(as.numeric(max(piirangud$area))),0), 
                        value = c(0, 7)),
            br(),
            
          sliderInput("text_size", "Püügiruudu numbri suurus kaardil:",
                        min = 0, max = 6, value = 3
            ),
            checkboxInput("showData", "Kuva andmetabel", value = F),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("estMap", height = "800px"),
           tableOutput("dataFrame"),
           DT::dataTableOutput("piirangud"),
           width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ranges <- reactiveValues(x = NULL, y = NULL)
    dates <- reactiveValues(data = NULL)
    baseMap <- reactive({
        estonia <- make_base_map(map_data$pyygiruudud_3301,
                                 map_data$estonia_map,
                                 text_size = input$text_size)
        return(estonia)
    })
    
    pyykVahemikusData <- reactive({
            if((lubridate::ymd(input$dates[2]) - lubridate::ymd(input$dates[1]))<0){
              stop("Valitud perioodi lõppaeg on enne algusaega!")
            }
      
            df <- catchdata
            if(input$pv != "kõik"){
                df <- df[df$pvClass == input$pv, ]
            }
        
            data<-pyyk_vahemikus(df,
                                 input$liik,
                                 startdate = input$dates[1],
                                 enddate = input$dates[2],
                                 nights = input$nights,
                                 month_only = !(input$exactDates),
                                 sumBy = input$col)
            return(data)
                                  
    })
    
    mapInput <- reactive({
        estonia <- baseMap()
        inputData <- pyykVahemikusData()
        if(!is.null(inputData)){
            if(nrow(inputData) > 0){
                if(input$col == "landing_kg"){
                    fill_unit = "kg"
                    fill_title = "Püük"
                }
                if(input$col == "catchTime"){
                  
                  fill_unit = "päevi"
                  fill_title = "Püügil"
                  if(input$nights){fill_unit = "öid"}
                  
                }   
                  
                if(input$col =="gearCount"){
                  fill_unit <- "vahend*päevi"
                  fill_title = "Püügivahendeid"
                  if(input$nights){fill_unit <- "vahend*öid"}
                }
                
                pStart <- input$dates[1]
                pEnd <- input$dates[2]
                if(!input$exactDates){
                  lubridate::day(pStart) <- 1
                  lubridate::day(pEnd) <- lubridate::days_in_month(pEnd)
                }
                
                return(plot_pyyk(inputData, map_data, estonia,
                                 pStart, pEnd,
                                 fill_unit =  fill_unit,
                                 fill_title =  fill_title, 
                                 text_size = input$text_size))
            }
            else{
                return(estonia)
            }
        }
    })
    
    piirangualaSuurus <- reactive({
        s <- units::as_units(10^input$areaSlider, "ha")
        #s<- input$areaSlider
        df<-piirangud[piirangud$area > s[1] & piirangud$area < s[2], ]
        return(df)
        })
    
    aastaringsedPiirangud<-reactive({
        dfP <- piirangualaSuurus()
        if(!input$aastaringsed){
          df <- dfP[!(dfP$piirangu_algus_kuup =="2019-01-01" &
                      dfP$piirangu_lopp_kuup == "2019-12-31"),]
          
          df <- df[!(df$piirangu_algus_kuup =="2018-01-01" &
                        df$piirangu_lopp_kuup == "2018-12-31"),] 
        }
        else{
            df <- dfP
        }
        return(df)
    })
    
    piiragudByFishDate <- reactive({
        if(input$piiranguFilter != "all"){
            df<-fish_dates_restrictions(aastaringsedPiirangud(),
                                        "piirangu_algus_kuup",
                                        "piirangu_lopp_kuup" ,
                                        startdate = input$dates[1],
                                        enddate = input$dates[2], 
                                        fish = input$liik,
                                        filterType = input$piiranguFilter)  
        }
        else{
            df<-aastaringsedPiirangud()
            hasFish <- sapply(df$keelu_kalad, function(x){input$liik %in% x})
            df<-df[hasFish, ]
        }
        
        return(df)
    })
    
    piiranguteTabel <- reactive({
        df<- piiragudByFishDate()
        if(input$pv != "kõik"){
            df <- df[input$pv %in% df$keeluPvClasses,]
        }
        return(df)
    })
    
    piiranguteDF <- reactive({
        
        df<- piiranguteTabel()
        
        outDF <- data.frame(
            Nimi = df[,"piirangu_nimi"],
            Algus = format(df[,"piirangu_algus_kuup"], "%m-%d"),
            Lõpp = format(df[,"piirangu_lopp_kuup"], "%m-%d"),
            Kirjeldus = df[,"piirangu_kirjeldus_pikk"],
            'Pindala(ha)' = signif(df$area, digits=3)
        )
      
        return(outDF)
    })
    
    kaardiPiirangud<-reactive({
        s <- input$piirangud_rows_selected
        if(length(s)>0){
            ids <- piiranguteTabel()[s, "piirangu_id"]
            psf<-piirangud_sf[piirangud_sf$piirangu_id %in% ids,]
            mapLayer <-ggspatial::geom_sf(data = psf, colour="brown", fill=ggplot2::alpha("lightblue",0.0),
                               size=input$text_size/4) 
            return(mapLayer)
        }
        
        return(ggplot2::geom_blank())
        
    })
    

    output$estMap <- renderPlot({
        req(kaardiPiirangud(), mapInput())
        p<-mapInput() +
          kaardiPiirangud() +
          ggspatial::coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE) 
        
        
        return(p)
    })
    
    
    output$dataFrame <-renderTable({
      if(input$showData){
        df<- pyykVahemikusData()
        df<-df[order(df$total, decreasing = T), c(1:3)]
        return(df)
      }
    })
    
    output$miniMap <- renderPlot({
        miniEst
    })
    
    output$piirangud <- DT::renderDataTable({
        df<-piiranguteDF()
        return(df)
        
        
    }, options = dtOptions)
    
    
    output$infoText <- renderText({
        s <- input$col
        if (s =="gearCount") {
            return("NB! püügivahendite arvu andmed on vaid 2018 aasta kohta!")
        }
        })
    
    ##############-----Month change buttons ----------#############
    newDates <- function(amount, dates){
      dates[1] <- lubridate::add_with_rollback(dates[1], months(amount), roll_to_first = T)
      lubridate::day(dates[1]) <- 1
      dates[2] <- lubridate::add_with_rollback(dates[2], months(amount))
      lubridate::day(dates[2]) <- lubridate::days_in_month(dates[2])
      return(dates)
    }
    
    
    monthDisplay <- function(amount, input){
      dates <- newDates(amount, input$dates)
      start <- lubridate::month(dates[1], label = T, locale = "et_EE.utf8")
      end <- lubridate::month(dates[2], label = T, locale = "et_EE.utf8")
      if(amount>0){
        if(start == end) return(paste0(start, " >"))
        return(paste0(start, " - ",end, " >"))
        }
      if(amount<0){
        if(start == end) return(paste0("< ", start))
        return(paste0("< ", start, " - ",end))
        }
      return("0")
    }
    
    
    pMonthDisplay <- reactive({
     return(monthDisplay(-1, input))
    })
    
    nMonthDisplay <- reactive({
      return(monthDisplay(1, input))
    })
    
    
    output$toMonthBack <- renderText({ pMonthDisplay()  })
    output$toMonthForward <- renderText({ nMonthDisplay()  })
    
    observeEvent(input$previousMonth, {
      dates <- newDates(-1, input$dates)
      updateDateRangeInput(session, "dates", start = dates[1], end = dates[2] )
    })
    
    observeEvent(input$nextMonth, {
      dates <- newDates(1, input$dates)
      updateDateRangeInput(session, "dates", start = dates[1], end = dates[2] )
    })  
    
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
        brush <- input$plot2_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
