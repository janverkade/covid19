#
# Jan Verkade, jan@forecastverification.com
# Delft, March 21, 2020
#

library(shiny)
library(ggplot2)
library(RCurl)
library(dplyr)
library(lubridate)
library(markdown)
library(scales)
library(sf)
library(leaflet)
library(shinyBS)
library(mapview)

##### Read file from Dropbox
url <- "https://dl.dropboxusercontent.com/s/6q2vfc0d52wt72k/covid19_data.rds?dl=1"
remoteBinFile <- getBinaryURL(url = url)
tmp = tempfile()
writeBin(object=remoteBinFile, con = tmp)
cases <- readRDS(tmp)
unlink(tmp)

wbpop18 <- readRDS("wb_pop18.rds")
worldmap <- st_read("TM_WORLD_BORDERS_SIMPL-0.3.shp",stringsAsFactors = FALSE, quiet = TRUE) %>% select(ISO3)
mapCases <- left_join(cases, worldmap) %>%  st_as_sf() 

myTitle <- "Reported COVID19 cases, deaths and recoveries"

ui <- navbarPage("covid19 visualization tool, by Jan Verkade",
                 tabsetPanel(id="covid19_panel",
                             tabPanel("Timeseries plots",
                                      tags$head(includeHTML(("google-analytics.html"))),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("myCountries","Country/Countries","",multiple=T),
                                          selectInput("myVariables","Variable(s)","",multiple=T),
                                          selectInput("myDataType","Data type","",multiple=F),
                                          selectInput("myVerticalScale","Vertical scale","",multiple=F),
                                          h3("Relative numbers"),
                                          checkboxInput("useRelativeData","Plot data relative to population size",value=F),
                                          h3("Relative dates"),
                                          checkboxInput("useRelativeDate",HTML("Plot using relative dates"),value=F),
                                          conditionalPanel(condition="input.useRelativeDate",
                                                           h5("Relative date condition:"),
                                                           selectInput("myThresholdType",label=NULL,"",multiple=F),
                                                           selectInput("myThresholdTypeAbsRel",label=NULL,"",multiple=F),
                                                           numericInput("myThresholdValue",label=NULL,100,min=1),
                                                           sliderInput("myRelativeDatesXLims",label="Relative date axis limits:",min=-120,max=120,value=c(-15,90))
                                          ), #conditionalPanel
                                        ), #sidebarPanel
                                        mainPanel(
                                          plotOutput("coronaPlot"),
                                          p(),
                                          actionLink("link_to_tabpanel_downloads", "You can download your plot (and the underlying data) from the 'downloads' tab.")
                                        ) #mainPanel
                                      ) #sidebarLayout
                             ), #tabpanel
                             tabPanel("Maps",
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("myMapVariables","Variable(s)","",multiple=F),
                                          selectInput("myMapDataType","Data type","",multiple=F),
                                          sliderInput("myMapDate",label="Date",min=min(cases$date),max=max(cases$date),value=max(cases$date),animate=T),
                                        ), #sidebarPanel
                                        mainPanel(
                                          h4("Experimental maps; not yet documented; not yet fully functional"),
                                          leafletOutput("myMap")
                                        ) #mainPanel
                                      ) #sidebarLayout
                             ), #tabPanel
                             tabPanel("Downloads",
                                      fluidPage(
                                        fluidRow(
                                          h4("Download current timeseries plot"),
                                          checkboxInput("changeTimeseriesPlotDownloadSettings",HTML("Change timeseries plot download settings"),value=F),
                                          conditionalPanel(condition="input.changeTimeseriesPlotDownloadSettings",
                                                           column(2,
                                                                  h5("PNG download settings"),
                                                                  numericInput("myPNGwidth","Width (px)",2400,min=1,width=150),
                                                                  numericInput("myPNGheight","Height (px)",1350,min=1,width=150),
                                                                  numericInput("myPNGres","Resolution (ppi)",300,min=100,width=150),
                                                           ),
                                                           column(2,
                                                                  h5("PDF download settings"),
                                                                  numericInput("myPDFwidth","Width (i)",8,min=1,width=150),
                                                                  numericInput("myPDFheight","Height (i)",4.5,min=1,width=150),
                                                           )
                                          ), #conditionalPanel
                                        ), #fluidRow
                                        fluidRow(
                                          downloadButton("downloadPNGplot", "PNG plot download"),
                                          downloadButton("downloadPDFplot", "PDF plot download"),
                                          p(),
                                        ), #fluidRow
                                        fluidRow(
                                          h4("Download current map"),
                                          p("Note that map downloads take a few seconds to render"),
                                          downloadButton("downloadPNGmap", "PNG map download"),
                                          downloadButton("downloadPDFmap", "PDF map download"),
                                          p(),
                                        ), #fluidRow
                                        fluidRow(
                                          h4("Download data"),
                                          selectInput("data2download","Choose a dataset:",choices = c("All data","Data from current plot","Population data")),
                                          downloadButton("downloadCSV","CSV download")
                                        ) #fluidRow
                                      ) #fluidPage
                             ), #tabPanel
                             
                             tabPanel("About", fluidRow( column(12, includeMarkdown("about.md")) ) )
                 )) #tabsetPanel   #navbarPage 

# Server logic
server <- function(input, output, session) {
  
  observe({
    
    myDataTypeChoices <- list("Cumulative"="cum","Daily"="daily")
    updateSelectInput(session=session,inputId="myDataType",label="Data type",
                      choices=myDataTypeChoices,selected=myDataTypeChoices[1])
    
    myVerticalScaleChoices <- list("Linear"="identity","Logaritmic (base e)"="log","Logaritmic (base 2)"="log2","Logaritmic (base 10)"="log10")
    updateSelectInput(session=session,inputId="myVerticalScale",label="Vertical scale",
                      choices=myVerticalScaleChoices,selected =myVerticalScaleChoices[1])
    
    myCountriesChoices <- sort(unique(cases$country))
    updateSelectInput(session=session,inputId="myCountries",label="Country/-ies",
                      choices=myCountriesChoices,selected="Netherlands")
    
    updateSelectInput(session=session,inputId="myVariables",
                      choices=unique(cases$variable),selected=unique(cases$variable)[1])
    
    myThresholdType <- sort(unique(cases$variable))
    updateSelectInput(session=session,inputId="myThresholdType",label=NULL,
                      choices=myThresholdType,selected=myThresholdType[1])
    
    myThresholdTypeAbsRel <- list("Absolute"="abs","Relative to the population size (fraction!)"="rel")
    updateSelectInput(session=session,inputId="myThresholdTypeAbsRel",label=NULL,
                      choices=myThresholdTypeAbsRel,selected=myThresholdTypeAbsRel[1])
    
    updateSelectInput(session=session,inputId="myMapVariables",
                      choices=unique(cases$variable),selected=unique(cases$variable)[1])
    
    updateSelectInput(session=session,inputId="myMapDataType",label="Data type",
                      choices=myDataTypeChoices,selected=myDataTypeChoices[1])
    
  })
  
  observeEvent(input$link_to_tabpanel_downloads, {
    updateTabsetPanel(session, inputId = "covid19_panel", selected="Downloads")
  })
  
  observeEvent(input$myThresholdTypeAbsRel,{
    if(input$myThresholdTypeAbsRel=="abs") {
      updateNumericInput(session,"myThresholdValue",value=100,min=1)
    } else {
      updateNumericInput(session,"myThresholdValue",value=1e-4,min=1e-5,max=1,step=1e-4)
    }
  })
  
  #myDateLabel <-function(x) { paste(substr(format(x, "%a"),1,3),format(x, "%b %d"),sep="\n") }
  myDateLabel <-function(x) { paste(format(x, "%b %d"),format(x, "%Y"),sep="\n") }
  
  createSubset <- function(){
    myCases <- subset(cases, country %in% input$myCountries)
    myCases <- merge(x=myCases,y=wbpop18)
    if(input$myThresholdTypeAbsRel=="rel") {
      myCases$rel_value <- myCases$value/myCases$pop18
      myCases <- myCases %>% group_by(country,variable) %>% mutate(t0 = min(date[datatype == "cum" & variable==input$myThresholdType & rel_value >= input$myThresholdValue]))
    } else {
      myCases$rel_value <- myCases$value
      myCases <- myCases %>% group_by(country,variable) %>% mutate(t0 = min(date[datatype == "cum" & variable==input$myThresholdType & value >= input$myThresholdValue]))
    }
    
    
    myCases <- myCases %>% group_by(country,date) %>% mutate(t0 = t0[is.finite(t0)][1])
    myCases <- myCases %>% group_by(country,variable) %>% mutate(rel_date = date - t0)
    myCases$t0 <- NULL
    myCases <- subset(myCases, datatype == input$myDataType)
    
  }
  
  createCoronaPlot <- function(){
    myCases <- createSubset()
    myCaption <- "Plotted by @janverkade based on data from Johns Hopkins University (https://systems.jhu.edu/research/public-health/ncov/)."
    
    if(input$myDataType=="cum" | input$myVerticalScale != "identity") {
      m2 <- geom_point(size=3,alpha=.8)
      m3 <- geom_line()
    } else {
      m2 <- geom_col(position="dodge",alpha=0.4)
      m3 <- NULL
    }
    
    
    if(input$useRelativeData) {
      myCases$rel_value <- myCases$value/myCases$pop18
      if(input$myVerticalScale == "identity") {myAccuracy=1e-2} else {myAccuracy=1e-4}
      myLabels <- scales::percent_format(accuracy = myAccuracy)
      myYLab <- "Reported number / population [%]"
    } else {
      myCases$rel_value <- myCases$value
      myLabels <- scales::comma_format(accuracy = 1)
      myYLab <- "Reported number [-]"
    }
    
    if(input$useRelativeDate) {
      m1 <- ggplot(subset(myCases,variable %in% input$myVariables),aes(x=rel_date,y=rel_value,col=country,shape=variable,linetype=variable,group=interaction(country,variable,datatype)))
      m4 <- scale_x_continuous(breaks = seq(-1400,1400,7),minor_breaks=NULL,limits=input$myRelativeDatesXLims)
      m5 <- if(input$useRelativeData & input$myThresholdTypeAbsRel=="abs") {NULL} else {geom_hline(yintercept=input$myThresholdValue,color="grey")}
      m6 <- geom_vline(xintercept=0,color="grey")
    } else {
      m1 <- ggplot(subset(myCases,variable %in% input$myVariables),aes(x=date,y=rel_value,col=country,fill=country,shape=variable,linetype=variable,group=interaction(country,variable,datatype)))
      m4 <- scale_x_date(date_breaks = "1 month",labels=myDateLabel,minor_breaks=NULL)
      m5 <- NULL
      m6 <- NULL
    }
    
    if (!exists(input$myVerticalScale)) { input$myVerticalScale <- "identity"}
    m1 + m5 + m6 + m2 + m3  +
      scale_y_continuous(trans=input$myVerticalScale,labels = myLabels) +
      xlab(NULL) + ylab(myYLab) +
      labs(caption=myCaption) +
      m4 +
      theme( legend.position="bottom",
             legend.margin=margin(t=0,r=0,b=0,l=0,unit="pt"),
             legend.key.height=unit(0,"cm"),
             legend.box="horizontal",
             legend.title=element_text(size=8))
  }
  
  output$coronaPlot <- renderPlot({
    print(createCoronaPlot())
  })
  
  createMyMapCases <- function(){
    myMapCases <- subset(mapCases, variable==input$myMapVariables & datatype==input$myMapDataType & date==input$myMapDate)
  }
  
  createMyLeafletMap <- function(){
    myMapCases <- createMyMapCases()
    leaflet(myMapCases) %>%  addTiles() %>%
      addPolygons(layerId = myMapCases$value,smoothFactor=0.5,weight=0.01,color="grey",
                  fillOpacity=0.8,fillColor = ~colorQuantile("Blues", unique(value),n=9)(value),
                  label=paste(myMapCases$country,paste0("(",myMapCases$ISO3,")"),"\n",
                              paste0(myMapCases$variable," (",myMapCases$datatype,"):"),scales::comma(myMapCases$value,accuracy = 1),
                              "on",myMapCases$date),
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))
  }
  
  output$myMap <- renderLeaflet({
    createMyLeafletMap()
  })
  
  datasetInput <- reactive({
    myCases <- createSubset()
    myCases <- subset(myCases, select = -c(rel_value))
    switch(input$data2download,"All data" = cases,"Data from current plot" = myCases,"Population data" = wbpop18)
  })
  
  output$downloadPNGplot <- downloadHandler(
    filename=function() {paste(Sys.Date(),"-covid19-plotted-by-janverkade.png",sep="")},
    content=function(filename) {
      png(filename,width=input$myPNGwidth,height=input$myPNGheight,res=input$myPNGres)
      print(createCoronaPlot()+labs(title=myTitle))
      dev.off()
    }
  )
  
  
  
  output$downloadPDFplot <- downloadHandler(
    filename=function() {paste(Sys.Date(),"-covid19-plotted-by-janverkade.pdf",sep="")},
    content=function(filename) {
      pdf(filename,width=input$myPDFwidth,height=input$myPDFheight,colormodel="cmyk",title="Produced by forecastverification.com")
      print(createCoronaPlot()+labs(title=myTitle))
      dev.off()
    }
  )
  
  output$downloadPNGmap <- downloadHandler(
    filename=function() {paste(Sys.Date(),"-covid19map-plotted-by-janverkade.png",sep="")},
    content=function(filename) {
      mapshot(createMyLeafletMap(),file=filename)
    }
  )
  
  output$downloadPDFmap <- downloadHandler(
    filename=function() {paste(Sys.Date(),"-covid19map-plotted-by-janverkade.pdf",sep="")},
    content=function(filename) {
      mapshot(createMyLeafletMap(),file=filename)
    }
  )
  
  
  output$downloadCSV <- downloadHandler(
    filename = function() { 'covid19-data-compiled-by-janverkade.csv' },
    content = function(file) {
      write.csv(datasetInput(),file,row.names=F,quote=F)
    }
  )

} #server

# Run the application 
shinyApp(ui = ui, server = server)
