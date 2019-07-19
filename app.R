#' Colorado Demographic Profiles Comparison Tool
#' @author  Adam Bickford, Colorado State Demography Office, March 2019 -November 2019
#' Release Version 0.5 5/21/2019

#setwd("J:/Community Profiles/Shiny Demos/Comparisons")

rm(list = ls())
library(tidyverse, quietly=TRUE)
library(readr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(codemogLib)
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL)
library(rmarkdown)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(eulerr)
library(rgdal)
library(jsonlite)
library(geojsonio)
library(units)
library(grid)
library(gridExtra)
library(ggthemes)
library(maptools)
library(officer)
library(flextable)
library(ggplotify)
library(ggrepel)  # These are the new packages
library(leaflet)
library(htmltools)
library(mapview)
library(DT)
library(WeightedCluster)
library(data.tree)
library(circlepackeR)
library(htmlwidgets)
library(plotly)



# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')

source("R/CountyName.R")
source("R/CountyRank.R")
source("R/MunicipalRank.R")
source("R/ageForecastPRO.R")
source("R/agePlotPRO.R")
source("R/baseIndustries.R")
source("R/boxContent.R")
source("R/captionSrc.R")
source("R/chkID.R")
source("R/cocPlot.R")
source("R/codemog_cdp.r")
source("R/dashboardMAP.R")
source("R/downloadObj.R")
source("R/downloadObjUI.R")
source("R/educPRO.R")
source("R/GenerateVenn.R")
source("R/houseEstPRO.R")
source("R/housePRO.R")
source("R/HouseVal.R")
source("R/incomePRO.R")
source("R/incomeSrc.R")
source("R/jobMigration.R")
source("R/jobsByIndustry.R")
source("R/jobsPlot.R")
source("R/jobsPopForecast.R")
source("R/listTofips.R")
source("R/medianAgeTab.R")
source("R/migbyagePRO.R")
source("R/HousingUnits.R")
source("R/percent.R")
source("R/pop_timeseries.R")
source("R/popForecast.R")
source("R/popPlace.R")
source("R/popPlotly.R")
source("R/raceTab1.R")
source("R/raceTab2.R")
source("R/residentialLF.R")
source("R/roundUpNice.R")
source("R/setAxis.R")
source("R/setYrRange.R")
source("R/simpleCap.R")
source("R/statsTable1.R")

source("R/submitPush.R")
source("R/submitReport.R")
source("R/tabList.R")
source("R/tabTitle.R")
source("R/TempFil.R")
source("R/weeklyWages.R")
source("R/unemployment.R")


# The GLOBAL Variables  Add Additional lists items as sections get defined
#File Locations ALSO LOOK AT LINE IN THE PDF OUTPUT CODE  LINE 1229
# Local/Development
# tPath <- "J:/Community Profiles/Shiny Demos/TempDir"  #Development

#Production
 tPath <- "/tmp"  

# Locations for Google Analtyics Java Script Files
# Local/ Development

# initJS <- "J:/Community Profiles/Shiny Demos/comparisons/www/dL_init.js"
# tagManJS <- "J:/Community Profiles/Shiny Demos/Comparisons/www/tag_manager.js"

#Production
# initJS <- "/srv/shiny-server/ProfileDashboard2/www/dL_init.js"
# tagManJS <- "/srv/shiny-server/ProfileDashboard2/www/tag_manager.js"

# Current ACS database
curACS <- "acs1317"
curYr <- 2017
fipslist <<- ""

# Set up database pool 1/23/19

config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)



dbGetInfo(DOLAPool)


onStop(function(){
  poolClose(DOLAPool)
})


fixPath <- function(inPath){
  outPath <- gsub("ABICKF~1","ABickford",inPath)
  outPath <-gsub("\\\\","/",outPath)
  return(outPath)
}



#CountyRanling
ctyRank.list <<- list()

#Basic Statistics
stats.list <<- list()

# Population Change
popa1 <<- list()
popa2 <<- list()
popa3 <<- list()
popa4 <<- list()
popa.list <<- list()

#Population Forecast
popf1 <<- list()
popf2 <<- list()
popf3 <<- list()
popf4 <<- list()
popf.list <<- list()


#Population Characteristics
popc1 <<- list()
popc2 <<- list()
popc3 <<- list()
popc4 <<- list()
popc.list <<- list()

#Housing and Household Characteristics
poph1 <<- list()
poph2 <<- list()
poph3 <<- list()
poph4 <<- list()
poph5 <<- list() # Housing Values
poph.list <<- list()

#Commuting (Transit)
popt1 <<- list()
popt2 <<- list()  # This is for the jobs and Migration chart
popt.list <<- list()

#Employment by Industry
popei1 <<- list()
popei2 <<- list()
popei3 <<- list()
popei.list <<- list()

#Employment and Demographic Forecast
popem1 <<- list()
popem2 <<- list()
popem3 <<- list()
popem4 <<- list()
popem.list <<- list()


# Structure of user Interface
ui <- 
  dashboardPage( 
                 skin="green", 
                 title= "Colorado Demographic Profiles Comparisons",
                 dashboardHeader(title = span(img(src="ShieldOnly_LRG.png", height = 70, align = "top"),"Colorado Demographic Profiles Comparisons"), titleWidth=550), #dashboardHeader
                 dashboardSidebar( width = 300,  useShinyjs(), 
                                   # data level Drop down
                                   selectInput("level", "Select Comparison Type" ,
                                               choices=c("Select a Comparison Type","County Ranking","Municipal Ranking","Regional Summary","Region to County","County to County","Municipality to Municipality")  #Enabled in V1; Need to add 'Municipal Ranking'
                                   ),
                                   selectInput("base", "Select Reference Location" ,choices="", multiple=FALSE),
                                   selectInput("comp","Select Comparison Location(s)" ,choices="Select Items",multiple=TRUE),
                                   checkboxGroupInput("outChk", label=NULL,
                                                      choices = NULL),
                                   #Action Button
                                   actionButton("profile","View Profile"),
                                   actionButton("contact","Contact SDO",onclick ="window.open('https://goo.gl/forms/xvyxzq6DGD46rMo42', '_blank')")
                                   
                                   
                                   
                                   
                 ), #dashboardSidebar
                 dashboardBody(  tags$head( 
                   tags$meta(name="keywords", content="Colorado, demographic, county, community, municiplaity, city, population, housing, household, age, median income, jobs, wages"),
                 #  includeScript(initJS),
                 #  includeScript(tagManJS), #writes GTM connection
                   tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),  #Link to CSS...
                   tags$title("Colorado Demographic Profiles Comparisons") #,
                   # includeScript("www/dataL.js") # This is the linkage to the dataLayer Output code
                 ),
                 tags$body(includeHTML("www/tag_body.js")),  # for non-JS instances
                 tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {
                                 color:#fffff;
                                 background:#C9C6C5
                                 }
                                 .box.box-solid.box-primary{
                                 color: #ffffff;
                                 border-bottom-color:#C9C6C5;
                                 border-left-color:#C9C6C5;
                                 border-right-color:#C9C6C5;
                                 border-top-color:#C9C6C5;
                                 }   ")),
                fluidRow(uiOutput("ui")
                 )
                
                 ) #dashboardBody
                
                
                 ) # dashboardPage/ui


# Server Management Function
server <- function(input, output, session) {

  infoSrc <- matrix(" ",nrow=8,ncol=2)
  infoSrc[1,1] <- "<b>Basic Statistics</b>"
  infoSrc[1,2] <- "Summary Table and Map"
  infoSrc[2,1] <- "<b>Population Trends</b>"
  infoSrc[2,2] <- "Population Estimates and Forecasts"
  infoSrc[3,1] <- "<b>Population Characteristics: Age</b>"
  infoSrc[3,2] <- "Population Estimates and Migration by Age"
  infoSrc[4,1] <- "<b>Population Characteristics: Income, Education and Race</b>"
  infoSrc[4,2] <- "Population Estimates by Income, Income Source, Educational Attainment and Race"
  infoSrc[5,1] <- "<b>Housing and Households</b>"
  infoSrc[5,2] <- "Housing Units, Costs and Unit Characteristics"
  infoSrc[6,1] <- "<b>Commuting and Job Growth</b>"
  infoSrc[6,2] <- "Commuting Patterns and Job Growth and Migration"
  infoSrc[7,1] <- "<b>Employment by Industry</b>"
  infoSrc[7,2] <- "Employment Data by Industry"
  infoSrc[8,1] <- "<b>Employment Forecast and Wage Information</b>"
  infoSrc[8,2] <- "Employment Forecasts, Wage and Number of Firms"
  
  infoTab <-  kable(infoSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed", full_width = F) %>%
    column_spec(1, width = "4in") 
  infoTab <- gsub("&lt;","<",infoTab)
  infoTab <- gsub("&gt;",">",infoTab)
  
  #Creating data Source Links Table
  linkSrc <- matrix(" ", nrow=6, ncol=5)
  linkSrc[1,1]  <- "<b>Data Dashboard</b>"
  linkSrc[2,1]  <- "<a href='https://gis.dola.colorado.gov/apps/demographic_dashboard/' target='_blank'>Demographic Dashboard</a>"
  linkSrc[3,1]  <- "<a href='https://gis.dola.colorado.gov/apps/netmigration_dashboard/' target='_blank'>Net Migration Dashboard</a>"
  
  linkSrc[4,1] <- "<b>Data Lookup Pages</b>"
  linkSrc[5,1] <- "<a href='https://demography.dola.colorado.gov/population/data/profile-county/' target='_blank'>County Data Lookup</a>"
  linkSrc[6,1] <- "<a href='https://demography.dola.colorado.gov/population/data/profile-regions/' target='_blank'>Regional Data Lookup</a>"
  
  linkSrc[1,2]  <- "<b>Maps and GIS data</b>" 
  linkSrc[2,2]  <- "<a href='https://demography.dola.colorado.gov/gis/map-gallery/' target='_blank'>Interactive Map Gallery</a>"
  linkSrc[3,2]  <- "<a href='https://demography.dola.colorado.gov/gis/thematic-maps/#thematic-maps' target='_blank'>Thematic Maps</a>"
  linkSrc[4,2]  <- "<a href='https://demography.dola.colorado.gov/demography/region-reports-2014/#colorado-planning-region-reports' target='_blank'>Region Reports</a>"
  linkSrc[5,2] <- "<a href='https://demography.dola.colorado.gov/gis/gis-data/#gis-data' target='_blank'>GIS Data Downloads</a>"
  linkSrc[6,2] <- "<a href='https://demography.dola.colorado.gov/gis/gis-data/#gis-data' target='_blank'>Links to GIS Data and DOLA Grants</a>"
  
  
  linkSrc[1,3]  <- "<b>Population Data</b>"
  linkSrc[2,3]  <- "<a href='https://demography.dola.colorado.gov/population/' target='_blank'>Population Estimates and Forecasts</a>"
  linkSrc[3,3]  <- "<a href='https://demography.dola.colorado.gov/births-deaths-migration/' target='_blank'>Births Deaths and Migration</a>"
  linkSrc[4,3]  <- "<a href='https://demography.dola.colorado.gov/economy-labor-force/' target='_blank'>Economy and Labor Force</a>"
  linkSrc[5,3]  <- "<a href='https://demography.dola.colorado.gov/housing-and-households/' target='_blank','>Housing and Households</a>"
  
  linkSrc[1,4] <- "<b>Census and ACS Data</b>"
  linkSrc[2,4] <- "<a href='https://demography.dola.colorado.gov/data/#census-data-tools' target='_blank'>Census Data Tools</a>"
  linkSrc[3,4] <- "<a href='https://demography.dola.colorado.gov/census-acs/' target='_blank'>Census Data Page</a>"
  linkSrc[1,5]  <- "<b>Publications</b>"
  linkSrc[2,5]  <- "<a href='https://demography.dola.colorado.gov/demography/publications-and-presentations/#publications-and-presentations' target='_blank'>Publications and Reports</a>"
  linkSrc[3,5]  <- "<a href='https://demography.dola.colorado.gov/crosstabs/' target='_blank'>Crosstabs</a>"
  linkSrc[4,5]  <- "<a href='https://demography.dola.colorado.gov/demography/publications-and-presentations/#annual-demography-summit-2017' target='_blank'>Annual Summit</a>"
  
  
  linkTab <-  kable(linkSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed") %>%
    column_spec(1, width = "2.25in") %>%
    column_spec(2, width = "2.25in") %>%
    column_spec(3, width = "2.25in") %>%
    column_spec(4, width = "2.25in") %>%
    column_spec(5, width = "2.25in")
  
  linkTab <- gsub("&lt;","<",linkTab)
  linkTab <- gsub("&gt;",">",linkTab)
  
  frontPgBox1 <- box(width=11,tags$div(tags$b("Welcome to the State Demography Office (SDO) Colorado Demographic Profiles Comparisons Website"), tags$br(),
                                       "This tool provides summary plots and data describing Counties and Incorporated Municipalities in Colorado.", tags$br(),
                                       tags$em("Profile Contents:"),
                                       HTML(infoTab),
                                       "To create a profile:",tags$br(),
                                       tags$ul(
                                         tags$li("Select a Data Level and Location using the dropdown boxes."),
                                         tags$li("Select specific Data Elements to display using the checkboxes."),
                                         tags$li("Click on the 'View Profile' button to display the selected profile.")
                                       ), 
                                       "You can download the plots and underlying data for each display by selecting the 'Sources and Downloads' 
                                       panel of each display box.", tags$br(),
                                       tags$em(tags$b("Notes:")), tags$br(), 
                                       tags$ul(
                                         tags$li("Profiles are available for Counties and Incorporated Municipalites.  
                                                 Please contact SDO for information on other geographies and places."),
                                         tags$li("Producing the requested outputs may take up to 3 minutes, depending on your request and your connection speed."),
                                         tags$li("Downloading any report, plot or data object will open a new browser window while the 
                                                 object is being processed and downloaded.  This window will close once the object processing is completed."),
                                         tags$li("Downloaded objects will be saved in the 'Download' location supported by your browser.")
                                         )))
  frontPgBox2 <-  box(width=11, tags$div(
    tags$b("Links to other SDO Data Sources:"),
    HTML(linkTab)))
  
  frontPg <- list(frontPgBox1,frontPgBox2)
  
 # output$ui <- renderUI(frontPg)

  # updates Dropdown boxes and selects data level and unit
  LocList <- popPlace(DOLAPool,curYr)
  CountyList <- LocList$Counties
  PlaceList <- LocList$Munis
  RegionList <- LocList$Region
  
  observeEvent(input$level, ({
    
    if(input$level == "Select Comparison Type") { #the initial state of the dropdowns
      outBase <- ""
      outComp <- ""
    }
    if(input$level == "County Ranking") {  # Added 9/18
      shinyjs::hide("base")
      shinyjs::hide("comp")
      updateCheckboxGroupInput(session,"outChk", label="Select the Data Elements for table:",
                               choices = c("Total Population" = "totpop",
                                           "Average Annual Population Growth Rate" ="popgr",
                                           "Percentage of Population Age 25 to 64" = "pop2564",
                                           "Percentage of Population Age 65 and Older" = "pop65",
                                           "Percentage of Persons of Color" = "pctNW",
                                           "Percentage of Persons with a Bachelor's Degree or Higher" = "educ",
                                           "Total Estimated Jobs"= "jobs",
                                           "Median Household Income"="medinc",
                                           "Percent of Persons Below the Poverty Line" = "poverty"),
                               selected =  c("totpop","popgr","pop2564","pop65",
                                             "pctNW","educ","jobs","medinc","poverty"))
    }
    if(input$level == "Municipal Ranking") {  # Added 9/18
      outBase <-  RegionList
      updateSelectInput(session, "base", choices = outBase)
      shinyjs::hide("base")
      shinyjs::hide("comp")
      updateCheckboxGroupInput(session,"outChk", label="Select the Data Elements to display:",
                               choices = c("Total Population" = "totpop",
                                           "Average Annual Population Growth Rate" ="popgr",
                                           "Percentage of Population Age 25 to 64" = "pop2564",
                                           "Percentage of Population Age 65 and Older" = "pop65",
                                           "Percentage of Persons of Color" = "pctNW",
                                           "Percentage of Persons with a Bachelor's Degree or Higher" = "educ",
                                           "Total Estimated Jobs"= "jobs",
                                           "Median Household Income"="medinc",
                                           "Percent of Persons Below the Poverty Line" = "poverty"),
                               selected =  c("totpop","popgr","pop2564","pop65",
                                             "pctNW","educ","jobs","medinc","poverty"))
    }
    
    if(input$level == "Regional Summary") {  # Added 9/18
       outBase <-  RegionList
       updateSelectInput(session, "base", choices = outBase)
       shinyjs::show("base")
       shinyjs::hide("comp")
       updateCheckboxGroupInput(session,"outChk", label="Select the Data Elements to display:",
                                choices = c("Basic Statistics" = "stats",
                                           "Population Estimates and Forecasts" = "popf",
                                            "Housing and Households" = "housing",
                                            "Base Industries, Firms and Jobs"="emplind",
                                            "Labor Force Participation and Wage Information"="emply"),
                          selected =  c("stats", "popf","housing","emplind","emply"))
    }
    if(input$level == "Region to County") {
      shinyjs::show("base")
      shinyjs::show("comp")
      outBase <- RegionList
      outComp <- unique(as.list(CountyList[,3]))
      updateSelectInput(session, "base", choices = outBase)
      updateSelectizeInput(session, "comp", choices = outComp)
      updateCheckboxGroupInput(session,"outChk", label="Select the Data Elements to display:",
                                choices = c("Basic Statistics" = "stats",
                                           "Population Estimates and Forecasts" = "popf",
                                            "Housing and Households" = "housing",
                                            "Base Industries, Firms and Jobs"="emplind",
                                            "Labor Force Participation and Wage Information"="emply"),
                          selected =  c("stats", "popf","housing","emplind","emply"))
    }
    if(input$level == "County to County") {
      shinyjs::show("base")
      shinyjs::show("comp")
      outBase <- unique(as.list(CountyList[,3]))
      outComp <- unique(as.list(CountyList[,3]))
      updateSelectInput(session, "base", choices = outBase)
      updateSelectizeInput(session, "comp", choices = outComp)
      updateCheckboxGroupInput(session,"outChk", label="Select the Data Elements to display:",
                                choices = c("Basic Statistics" = "stats",
                                           "Population Estimates and Forecasts" = "popf",
                                            "Housing and Households" = "housing",
                                            "Base Industries, Firms and Jobs"="emplind",
                                            "Labor Force Participation and Wage Information"="emply"),
                          selected =  c("stats", "popf","housing","emplind","emply"))
    }
    if(input$level == "Municipality to Municipality") {
      shinyjs::show("base")
      shinyjs::show("comp")
      outBase <- unique(as.list(PlaceList[,3]))
      outComp <- unique(as.list(PlaceList[,3]))
      updateSelectInput(session, "base", choices = outBase)
      updateSelectizeInput(session, "comp", choices = outComp)
      updateCheckboxGroupInput(session,"outChk", label="Select the Data Elements to display:",
                                choices = c("Basic Statistics" = "stats",
                                            "Housing and Households" = "housing"),
                          selected =  c("stats", "housing"))
    }
  }))  #observeEvent input$level
  
  # Event for Comparison selection
  observeEvent(input$comp, {
    
  }) #observeEvent input$comp
  
  # Event for click on profile button
  observeEvent(input$profile,  {
    

 #   dLout <- submitPush(input$level,input$unit,input$outChk)  # Generate dataLayer Command
 #   session$sendCustomMessage("handler1",dLout)  #Sends dataLayer command to dataL.js script
   
    outputList <<- list()
    output$ui <- renderUI(outputList)
  
    #creating the input FIPS list to generate data
    if(input$level == "Select Comparison Type") {
      lnError <- tags$h2("Please specify a Comparison Type")
      outputList <<- list(lnError)
    }  else {
      withProgress(message = 'Generating Profile', value = 0, {  # Initialize Progress bar
        #Building fipslist
        if(input$level == "County Ranking")  {
          placeName <- input$level
          fipslist <<- ""
          }
          if(input$level == "Municipal Ranking") {
              placeName <- input$level 
              fipslist <<- ""
          }
        
        if(input$level == "Regional Summary") {
          fipslist <<- listTofips(lvl=input$level,inlist1=RegionList,value1=input$base,inlist2="",value2="")
          placeName <- fipslist$plName1
        } 
        if(input$level == "Region to County") { 
          fipslist <<- listTofips(lvl=input$level,inlist1=RegionList,value1=input$base,inlist2=CountyList,value2=input$comp)
          if(fipslist$length2 == 1){
            pl <- fipslist$plName2[1]
          } else {
            for(i in 1:fipslist$length2){
            if(i == 1) {
              pl <- fipslist$plName2[i] 
            } else {
              pl <- paste0(pl,", ",fipslist$plName2[i])
            }
          }
          }
          placeName <- paste0(fipslist$plName1," Compared to: ",pl)
        }
        if(input$level == "County to County") {  
          fipslist <<- listTofips(lvl=input$level,inlist1=CountyList,value1=input$base,inlist2=CountyList,value2=input$comp)
          if(fipslist$length2 == 1){
            pl <- fipslist$plName2[1]
          } else {
            for(i in 1:fipslist$length2){
              if(i == 1) {
                pl <- fipslist$plName2[1] 
              } else {
                pl <- paste0(pl,", ",fipslist$plName2[i])
              }
          }
          }
          placeName <- paste0(fipslist$plName1," Compared to: ",pl)
        }
        if(input$level == "Municipality to Municipality") {  
          fipslist <<- listTofips(lvl=input$level,inlist1=PlaceList,value1=input$base,inlist2=PlaceList,value2=input$comp)
          if(fipslist$length2 == 1){
            pl <- fipslist$plName2[1]
          } else {
            for(i in 1:fipslist$length2){
              if(i == 1) {
                pl <- fipslist$plName2[1] 
              } else {
                pl <- paste0(pl,", ",fipslist$plName2[i])
              }
          }
          }
          placeName <- paste0(fipslist$plName1," Compared to: ",pl)
        }
        
        #Generate profile UI objects
        
        svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
        
        ln1 <- tags$h1(placeName)
        
        
        
        #creating ids and output flags for multiple counties and small places
         idList <- chkID(lvl=input$level,fipslist= fipslist)
        # Ranking output
        if(input$level == "County Ranking") {

          CountyRank <- CountyRank(DBPool = DOLAPool, CtyList = CountyList, chkList = input$outChk, eYr = curYr, ACS=curACS) 
           
          CirPack <- circlepackeR(CountyRank$outtree, size = "r", color_min = "hsl(56,80%,80%)", 
                                   color_max = "hsl(341,30%,40%)")
          
          
          CtyTab <- CountyRank$data
          output$CtyTabOut <- DT::renderDataTable(CtyTab,
                           options = list(pageLength = 6,
                                          autowidth= TRUE,
                                          scrollX = TRUE,
                                          scrollY = TRUE),
                           rownames = FALSE,caption = "County Rankings.  Click on header to Sort")
          
          CtyRank.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabin each display box.",tags$br(),
                                 tags$br(),
                                 "General information is available here:", tags$br(),
                                 tags$ul(
                                   tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Data",target="_blank")),
                                   tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank"),
                                           tags$br(),tags$br(),downloadObjUI("ctydata")
                                   )))
          
          CtyRank.box0 <- box(width=12,ln1)
          CtyRank.box1 <- box(width=4,renderCirclepackeR(CirPack))
          CtyRank.box2 <- tabBox(width=12, 
                               tabPanel("Table",DT::dataTableOutput("CtyTabOut")),
                               tabPanel("Information",CtyRank.info))
                               
          
          
          
          #building List
          CtyRank.list <<- list(CtyRank.box0, CtyRank.box1, CtyRank.box2)
          
          incProgress()
        }
      
         if(input$level == "Municipal Ranking") {
           MuniRank <- MunicipalRank(DBPool = DOLAPool, MuniList = PlaceList, chkList = input$outChk, eYr = curYr, ACS=curACS) 
           
           MuniPack <- circlepackeR(MuniRank$outtree, size = "r", color_min = "hsl(56,80%,80%)", 
                                   color_max = "hsl(341,30%,40%)")
           
           MuniTab <- MuniRank$data
           output$MuniTabOut <- DT::renderDataTable(MuniTab,
                                                   options = list(pageLength = 9,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE,
                                                                  scrollY = TRUE),rownames = FALSE,caption = "Municipal Rankings.  Click on header to Sort")
           
           MuniRank.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabin each display box.",tags$br(),
                                    tags$br(),
                                    "General information is available here:", tags$br(),
                                    tags$ul(
                                      tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Data",target="_blank")),
                                      tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank"),
                                              tags$br(),tags$br(),downloadObjUI("munidata")
                                      )))
           
           MuniRank.box0 <- box(width=12,ln1)
           MuniRank.box1 <- box( width=4, renderCirclepackeR(MuniPack))
                      
             
           MuniRank.box2 <- tabBox(width=12, 
                                  tabPanel("Table",DT::dataTableOutput("MuniTabOut")),
                                  tabPanel("Information",MuniRank.info))
           
           
           
           
           #building List
           MuniRank.list <<- list(MuniRank.box0, MuniRank.box1, MuniRank.box2)
           
           incProgress()
         }
         
        #stats; Basic Statistics
        if("stats" %in% input$outChk) {
          stats.text <- tags$h2("Basic Statistics")
          
          stat_List <<- statsTable1(DBPool=DOLAPool,lvl=input$level,listID=idList,sYr=2010,eYr=curYr,ACS=curACS)
          stat_map <<- dashboardMAP(DBPool=DOLAPool,lvl=input$level,listID=idList)
          
          #Images
          output$statMap <- renderLeaflet(stat_map)
       
          # Output DT Table
          
           output$StatTabOut <- DT::renderDataTable(stat_List,
                                                   options = list(pageLength = 9,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE,
                                                                  scrollY = TRUE),rownames = FALSE,caption = "Basic Statistics.  Click on header to Sort")

          
          Stats.map <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tab in each display box.",tags$br(),
                                  tags$br(),
                                 "General information is available here:", tags$br(),
                                 tags$ul(
                                   tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Data",target="_blank")),
                                   tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank"),
                                           tags$br(),tags$br(),downloadObjUI("statsplot")
                                   )))
          
          Stats.info <- tags$div(class="dInfo","Individual plots and data may be downloaded by selecting the 'Sources and Downloads' tabin each display box.",tags$br(),
                                  tags$br(),
                                 "General information is available here:", tags$br(),
                                 tags$ul(
                                   tags$li(tags$a(href="https://demography.dola.colorado.gov/data/","State Demography Office Data",target="_blank")),
                                   tags$li(tags$a(href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml","U.S. Census Bureau American Community Survey",target="_blank"),
                                           tags$br(),tags$br(),downloadObjUI("statstabl")
                                   )))
  
          stats.box0 <- box(width=12,ln1)
          stats.box1 <- box(width=5, height=350,leafletOutput("statMap"))
          stats.box2 <- tabBox(width=12, height=400,
                               tabPanel("Table",DT::dataTableOutput("StatTabOut")),
                               tabPanel("Information",Stats.info))
          
          
          
          #building List
          stats.list <<- list(stats.box0, stats.box1, stats.box2)
          
          incProgress()
        }
        # Population Forecasts
        
        if("popf" %in% input$outChk){
          #Chart/Table Objects
          popf1 <<- popPlotly(DBPool=DOLAPool,lvl=input$level,listID=idList)
          outplotp1 <- popf1$plot1
          outplotp2 <- popf1$plot2
          outplotp3 <- popf1$plot3
          
          #infobox Objects
            popf1.info <- tags$div(boxContent(title= "Population Estimates and Forecasts",
                                              description = "The Population Estimates and Forecast Plot provides population estimates derived from SDO Single Year of Age data.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("SDO County Single-Year of Age Forecasts","https://demography.dola.colorado.gov/population/data/sya-county/")) ),
                                   tags$br(),
                                   downloadObjUI("popf1data"))
     
             popf1.box <- tabBox(width=12, height=500,
                              tabPanel("Plot",renderPlotly({outplotp1})),
                              tabPanel("Sources and Downloads",popf1.info))
             
             popf2.info <- tags$div(boxContent(title= "Natural Increase",
                                              description = "Trends in Natural Increase show the difference between Births and Dealths in a County",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("SDO Components of Change","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/#components-of-change")) ),
                                   tags$br(),
                                   downloadObjUI("popf2data"))
     
             popf2.box <- tabBox(width=12, height=500,
                              tabPanel("Plot",renderPlotly({outplotp2})),
                              tabPanel("Sources and Downloads",popf2.info))
             
             
            popf3.info <- tags$div(boxContent(title= "Net Migration",
                                              description = "Trends in Net Migration show the difference between in migration and out migration in a County",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("SDO Components of Change","https://demography.dola.colorado.gov/births-deaths-migration/data/components-change/#components-of-change")) ),
                                   tags$br(),
                                   downloadObjUI("popf3data"))
     
             popf3.box <- tabBox(width=12, height=500,
                              tabPanel("Plot",renderPlotly({outplotp3})),
                              tabPanel("Sources and Downloads",popf3.info))
              #Append to List
              popf.list <<- list(popf1.box, popf2.box, popf3.box)
             
              incProgress()
        }  # popf
        

        # Housing
        if("housing" %in% input$outChk){
          #Generate tables, plots and text...
          poph2 <<- housePRO(DBPool=DOLAPool,lvl=input$level,listID=idList, curYr=curYr) # Housing Unit Table
          
          outploth1 <- poph2$plot1
          outploth2 <- poph2$plot2
          outploth3 <- poph2$plot3
          
          poph2.info <- tags$div(boxContent(title= "Housing Type Plots",
                                            description= "The Housing Type Plots compare the categories of housing types for a selected place to the State.",
                                            MSA= "F", stats = "T", muni = "F", multiCty = "F", PlFilter = "F", 
                                            urlList = list(c("SDO Housing Time Series","https://demography.dola.colorado.gov/population/data/muni-pop-housing/"),
                                                           c("American Community Survey American Fact Finder, Series B25001, B25003, and B25004","https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml")) ),
                                 tags$br(),
                                 downloadObjUI("poph2tabl"),downloadObjUI("poph2data"))
          

          # Bind to boxes
          poph1.box <- tabBox(width=12, height=500,
                              tabPanel("Plot",renderPlotly({outploth1})),
                              tabPanel("Sources and Downloads",poph2.info))
          
          poph2.box <- tabBox(width=12, height=500,
                              tabPanel("Plot",renderPlotly({outploth2})),
                              tabPanel("Sources and Downloads",poph2.info))
        
          poph3.box <- tabBox(width=12, height=500,
                              tabPanel("Plot",renderPlotly({outploth3})),
                              tabPanel("Sources and Downloads",poph2.info))
          
          
          #Append to List
          poph.list <<- list(poph1.box,poph2.box, poph3.box)
          incProgress()
        }
        

        #Employment by Industry
        if("emplind" %in% input$outChk){
          #Generate tables, plots and text...
          popei1 <<-  baseIndustries(DBPool=DOLAPool,lvl=input$level,listID=idList, curyr = curYr)
          popei2 <<- jobsPlot(DBPool=DOLAPool,lvl=input$level,listID=idList, maxyr = curYr)
          eiplot <- popei1$plot
          jobsplot <- popei2$plot1
          firmsplot <- popei2$plot2
          
          #Contents of Information Tabs
          popei1.info <- tags$div(boxContent(title= "Base Industries Plot",
                                            description= "The Base Industries plot shows which industries drive the county economy by bringing in dollars from outside the area.  A county with a diversity of base industries with similar shares of employment will 
                                             generally be more resilient than one that is dominated by one large industry.",
                                            MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                             urlList = list(c("SDO Base Industries Summary","https://drive.google.com/file/d/1Ag0JdOo8XATTBiNuh80BTiuqLV4Kv72T/view"),
                                                            c("SDO Base industries Anaysis","https://demography.dola.colorado.gov/economy-labor-force/data/base-analysis/#base-industries-analysis"))),
                                  tags$br(),
                                  downloadObjUI("popei1data"))
          
         popei2.info <- tags$div(boxContent(title= "Estimated Jobs Plot",
                                             description = "The Estimated Jobs Plot shows the relationship between firms, jobs and periods of economic recession from 2001 to the present. The grey boxes indicate periods of economic recession.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                             urlList = list(c("Jobs: Jobs by Sector (NAICS)","https://demography.dola.colorado.gov/economy-labor-force/data/jobs-by-sector/#jobs-by-sector-naics"))),
                                             tags$br(),  downloadObjUI("popei2data"))
  
         popei3.info <- tags$div(boxContent(title= "Estimated Firms Plot",
                                             description = "The Estimated Firms Plot shows the relationship between firms, jobs and periods of economic recession from 2001 to the present. The grey boxes indicate periods of economic recession.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                             urlList = list(c("Firms: Department of Labor and Employment Quarterly Census of Employment and Wages","https://www.colmigateway.com/gsipub/index.asp?docid=372"))),
                                             tags$br(), downloadObjUI("popei3data"))

         # Bind to boxes
          popei1.box <- tabBox(width=12, height=500,
                     tabPanel("Plot",renderPlotly({eiplot})),
                     tabPanel("Sources and Downloads",popei1.info))
          
          popei2.box <- tabBox(width=12, height=500,
                     tabPanel("Plot",renderPlotly({jobsplot})),
                     tabPanel("Sources and Downloads",popei2.info))

          popei3.box <- tabBox(width=12, height=500,
                     tabPanel("Plot",renderPlotly({firmsplot})),
                     tabPanel("Sources and Downloads",popei3.info))

 
          #Append to List
          popei.list <<- list(popei1.box,popei2.box,popei3.box)
          incProgress()
        }  #Employment by Industry
        
        #Employment and Demographic Forecast
        if("emply" %in% input$outChk){
          #Generate tables, plots and text...
          popem1 <<- jobsPopForecast(DBPool=DOLAPool,lvl=input$level,listID=idList,curyr=curYr)
          popem2 <<- weeklyWages(DBPool=DOLAPool,lvl=input$level,listID=idList,curyr=curYr)
          popem3 <<- unemployment(DBPool=DOLAPool,lvl=input$level,listID=idList,curyr=curYr)  
          
          plotem1 <- popem1$plot
          plotem2 <- popem2$plot
          plotem3 <- popem3$plot
          
          
          #Contents of Information Tabs
          popem1.info <- tags$div(boxContent(title= "Jobs and Population Forecast Table",
                                             description = "The Jobs Forecast Table displays the growth of local jobs by county.",
                                             MSA= "F", stats = "F", muni = "T", multiCty = "F", PlFilter = "F", 
                                             urlList = list(c("SDO Economic Forecasts"," https://demography.dola.colorado.gov/economy-labor-force/economic-forecasts/#economic-forecasts"),
                                                            c("SDO Jobs Forecasts","https://demography.dola.colorado.gov/economy-labor-force/data/labor-force/#labor-force-participation"))),
                                  tags$br(),
                                  downloadObjUI("popem1data"))
          
          popem2.info <- tags$div(boxContent(title= "Average Weekly wages",
                                             description = "The Average Weekly Wages plot shows the trend in average wages from 2010 to the present for selected counties.",
                                             MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                             urlList = list(c("Department of Labor and Employment Quarterly Census of Employment and Wages","https://www.colmigateway.com/gsipub/index.asp?docid=372") )),
                                  tags$br(),
                                  downloadObjUI("popem2plot"),  downloadObjUI("popem2data"))
          
          popem3.info <- tags$div(boxContent(title= "Unemployment Rates",
                                             description = "The Unemployment rate plot shows undmployment by county. The grey boxes indicate periods of economic recession",
                                             MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                             urlList = list(c("United States Bureau of Economic Analysis.","https://www.bea.gov/") )),
                                  tags$br(),
                                  downloadObjUI("popem3data"))
          
          
          # Bind to boxes
          popem1.box <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({plotem1})),
                               tabPanel("Sources and Downloads",popem1.info))
          popem2.box <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({plotem2})),
                               tabPanel("Sources and Downloads",popem2.info))
          popem3.box <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({plotem3})),
                               tabPanel("Sources and Downloads",popem3.info))
         
          
          
          #Append to List
     
          popem.list <<- list(popem1.box,popem2.box,popem3.box) 
          incProgress()
        }  #Employment and Demographic Forecast
        
        
        
        
        incProgress()       
        
      }) #Progress Bar
    }#if input$unit == ""
    
    # Output UI...
    
   if(input$level == "County Ranking") {
     tabs <- CtyRank.list
   } else if (input$level == "Municipal Ranking") {
     tabs <- MuniRank.list
      }   else {
     if(length(outputList) == 0) {
       tabs <- lapply(1:length(input$outChk), function(i) {  # this determines the number of tabs needed
         id <- paste0("tab", i)
         tabPanel(
           title = tabTitle(input$outChk[[i]]), tabList(input$outChk[[i]])
         ) # TabPanel
       })
     }  else {
       tabs <- outputList
     }
   }

    output$ui  <- renderUI({ do.call(tabsetPanel, tabs) }) #renderUI
   
    
    
    
    #Event to outload plots and data files  NEED TO EDIT....
    #CountyRank
    callModule(downloadObj, id = "ctydata", "", "ctydata", CountyRank$data)
    
    #MunicipalRank
    callModule(downloadObj, id = "munidata", "", "munidata", MuniRank$data)
    
    #Basic Statistics
    callModule(downloadObj, id = "statstabl", simpleCap(placeName), "statstabl", stat_List$FlexTable)
    callModule(downloadObj, id = "statsplot", simpleCap(placeName), "statsplot", stat_map)
    
    #Population Forecast
    
    callModule(downloadObj, id = "popf1tabl", simpleCap(input$unit), "popf1tabl", popf1$FlexTable)
    callModule(downloadObj, id = "popf1data", simpleCap(input$unit), "popf1data", popf1$data)
    
    callModule(downloadObj, id = "popf2plot", simpleCap(input$unit),"popf2plot", popf2$plot)
    callModule(downloadObj, id = "popf2data", simpleCap(input$unit),"popf2data", popf2$data)
    
    callModule(downloadObj, id = "popf3plot", simpleCap(input$unit), "popf3plot", popf3$plot)
    callModule(downloadObj, id = "popf3data", simpleCap(input$unit), "popf3data", popf3$data)
    
    callModule(downloadObj, id = "popf4plot", simpleCap(input$unit), "popf4plot", popf4$plot)
    callModule(downloadObj, id = "popf4data", simpleCap(input$unit), "popf4data", popf4$data)
    
    #Age
    callModule(downloadObj, id = "popa1plot", simpleCap(input$unit),"popa1plot", popa1$plot)
    callModule(downloadObj, id = "popa1data", simpleCap(input$unit),"popa1data", popa1$data)
    
    callModule(downloadObj, id = "popa2plot", simpleCap(input$unit),"popa2plot", popa2$plot)
    callModule(downloadObj, id = "popa2tabl", simpleCap(input$unit),"popa2tabl", popa2$FlexTable)
    callModule(downloadObj, id = "popa2data", simpleCap(input$unit),"popa2data", popa2$data)
    
    callModule(downloadObj, id = "popa3plot", simpleCap(input$unit), "popa3plot", popa3$plot)
    callModule(downloadObj, id = "popa3data", simpleCap(input$unit), "popa3data", popa3$data)
    
    callModule(downloadObj, id = "popa4plot", simpleCap(input$unit), "popa4plot", popa4$plot)
    callModule(downloadObj, id = "popa4data", simpleCap(input$unit), "popa4data", popa4$data)
    
    #Population Characteristics
    callModule(downloadObj, id = "popc1plot", simpleCap(input$unit),"popc1plot", popc1$plot)
    callModule(downloadObj, id = "popc1data", simpleCap(input$unit),"popc1data", popc1$data)
    
    callModule(downloadObj, id = "popc2plot", simpleCap(input$unit),"popc2plot", popc2$plot)
    callModule(downloadObj, id = "popc2data", simpleCap(input$unit),"popc2data", popc2$data)
    
    callModule(downloadObj, id = "popc3tabl", simpleCap(input$unit), "popc3tabl", popc3$FlexTable)
    callModule(downloadObj, id = "popc3data", simpleCap(input$unit), "popc3data", popc3$data)
    
    
    callModule(downloadObj, id = "popc4tabl", simpleCap(input$unit), "popc4tabl", popc4$FlexTable)
    callModule(downloadObj, id = "popc4data", simpleCap(input$unit), "popc4data", popc4$data)
    
    #Housing
    callModule(downloadObj, id = "poph1plot", simpleCap(input$unit),"poph1plot", poph1$plot)
    callModule(downloadObj, id = "poph1data", simpleCap(input$unit),"poph1data", poph1$data)
    
    callModule(downloadObj, id = "poph2tabl", simpleCap(input$unit),"poph2tabl", poph2$FlexTable)
    callModule(downloadObj, id = "poph2data", simpleCap(input$unit),"poph2data", poph2$data)
    
    callModule(downloadObj, id = "poph3tabl", simpleCap(input$unit),"poph3tabl", poph3$FlexTable)
    callModule(downloadObj, id = "poph3data", simpleCap(input$unit), "poph3data", poph3$data)
    
    callModule(downloadObj, id = "poph4tabl", simpleCap(input$unit),"poph4tabl", poph4$FlexTable)
    callModule(downloadObj, id = "poph4data", simpleCap(input$unit), "poph4data", poph4$data)
    
   
    
    #commuting
    callModule(downloadObj, id = "popt1plot", simpleCap(input$unit),"popt1plot", popt1$plot)
    
    callModule(downloadObj, id = "popt2tabl", simpleCap(input$unit),"popt2tabl", popt1$Flexcomb)
    callModule(downloadObj, id = "popt2data", simpleCap(input$unit),"popt2data", popt1$data2)
    
    callModule(downloadObj, id = "popt3tabl", simpleCap(input$unit),"popt3tabl", popt1$Flexcomb)
    callModule(downloadObj, id = "popt3data", simpleCap(input$unit),"popt3data", popt1$data2)
    
    callModule(downloadObj, id = "popt4plot", simpleCap(input$unit),"popt4plot", popt2$plot)
    callModule(downloadObj, id = "popt4data", simpleCap(input$unit),"popt4data", popt2$data)
    
    #Employment by Industry
    callModule(downloadObj, id = "popei1data", simpleCap(input$unit),"popei1data", popei1$data)
    callModule(downloadObj, id = "popei2data", simpleCap(input$unit),"popei2data", popei2$data1)
    callModule(downloadObj, id = "popei3data", simpleCap(input$unit),"popei3data", popei3$data2)
   
    
    #Employment and Demographic Forecast
    callModule(downloadObj, id = "popem1tabl", simpleCap(input$unit),"popem1tabl", popem1$FlexTable)
    callModule(downloadObj, id = "popem1data", simpleCap(input$unit),"popem1data", popem1$data)
    callModule(downloadObj, id = "popem2plot", simpleCap(input$unit),"popem2plot", popem2$plot)
    callModule(downloadObj, id = "popem2data", simpleCap(input$unit),"popem2data", popem2$data)
    callModule(downloadObj, id = "popem3tabl", simpleCap(input$unit),"popem3tabl", popem3$FlexTable)
    callModule(downloadObj, id = "popem3data", simpleCap(input$unit),"popem3data", popem3$data)
    
    callModule(downloadObj, id = "popem4plot", simpleCap(input$unit),"popem4plot", popem4$plot)
    callModule(downloadObj, id = "popem4data", simpleCap(input$unit),"popem4data", popem4$data)
   # reset("comp")
   # updateSelectizeInput(session, "comp", choices = outComp)
    
  }) #observeEvent input$profile
  
  
  
}  #server



shinyApp(ui = ui, server = server)
