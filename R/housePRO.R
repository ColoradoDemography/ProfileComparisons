#' housePRO  Produces the housing table
#'  CO Housing Unit Table
#'
#'  This function compares housing occupancy and vacancy rates for a place to the state
#'
#' @param listID the list containing place id and Place names
#' @param HH Specifies the HH data set to be used, reads curHH from Shiny program
#' @return kable formatted  table and data file
#' @export
#'

housePRO=function(DBPool,lvl, listID, curYr){
  
 # Collecting place ids from  idList, setting default values
  
  ctyfips1 <- listID$ctyNum1
  ctyname1 <- listID$ctyName1
  
  ctyfips2 <- listID$ctyNum2
  ctyname2 <- listID$ctyName2
  
  placefips1 <- listID$plNum1
  placename1 <- listID$plName1
  
  placefips2 <- listID$plNum2
  placename2 <- listID$plName2
  
state <- "08"

     # Building county data table

if(lvl == "Regional Summary") {
  f.hh <- data.frame()
  for(i in 1:length(ctyfips1)) {
    HHSQLcty <- paste0("SELECT countyfips, year, totalhousingunits, vacanthousingunits FROM estimates.county_profiles WHERE countyfips = ", ctyfips1[i],";")
    HH <- dbGetQuery(DBPool,HHSQLcty)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips1[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    HH$county <- namex$municipalityname 
    HH$county <- simpleCap(HH$county)
    f.hh <- bind_rows(f.hh,HH)
  }
  f.hh$occupiedhousingunits <-   f.hh$totalhousingunits - f.hh$vacanthousingunits
  
  #Fixing data for Broomfield
  f.hh <- f.hh[!(f.hh$countyfips == 14 & f.hh$year <=2001),]
  
  f.hh <- f.hh[which(f.hh$year >= 1990),]
  
  # Titles
 grTit1 <- paste0("Total Housing Units: ",ctyname1)
 grTit2 <- paste0("Occupied Housing Units: ",ctyname1)
 grTit3 <- paste0("Vacant Housing Units: ",ctyname1)
}

if(lvl == "Region to County") {

  f.hh <- data.frame()
  for(i in 1:length(ctyfips1)) {
    HHSQLcty <- paste0("SELECT countyfips, year, totalhousingunits, vacanthousingunits FROM estimates.county_profiles WHERE countyfips = ", ctyfips1[i],";")
    HH <- dbGetQuery(DBPool,HHSQLcty)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips1[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    HH$county <- namex$municipalityname 
    HH$county <- simpleCap(HH$county)
    f.hh <- bind_rows(f.hh,HH)
  }
  f.hhR <- f.hh %>%
       group_by(year) %>%
       summarize(totalhousingunits = sum(totalhousingunits),
                 vacanthousingunits = sum(vacanthousingunits))
  f.hhR$countyfips <- 1000
  f.hhR$county <- ctyname1
  f.hhR <- f.hhR[,c(4,5,1,2,3)]
  

  #County
 f.hhC <- data.frame()
  for(i in 1:length(ctyfips2)) {
    HHSQLcty <- paste0("SELECT countyfips, year, totalhousingunits, vacanthousingunits FROM estimates.county_profiles WHERE countyfips = ", ctyfips2[i],";")
    HH <- dbGetQuery(DBPool,HHSQLcty)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips2[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    HH$county <- namex$municipalityname 
    HH$county <- simpleCap(HH$county)
    f.hhC <- bind_rows(f.hhC,HH)
  }
 
 f.hhC <- f.hhC[,c(1,5,2,3,4)]
 f.hh <- rbind(f.hhC,f.hhR)

 f.hh$occupiedhousingunits <-   f.hh$totalhousingunits - f.hh$vacanthousingunits
  
  #Fixing data for Broomfield
  f.hh <- f.hh[!(f.hh$countyfips == 14 & f.hh$year <=2001),]
  
  f.hh <- f.hh[which(f.hh$year >= 1990),]
  
 # Title
 ctynames <-ctyname2[1]
 if(length(ctyname2) > 1){
    for(i in 2:length(ctyname2)){
         ctynames <- paste0(ctynames,", ",ctyname2[i])
     }
 }

    ctynames <- paste0(ctynames," and ",ctyname1)
    
# Titles
 grTit1 <- paste0("Total Housing Units: ",ctynames)
 grTit2 <- paste0("Occupied Housing Units: ",ctynames)
 grTit3 <- paste0("Vacant Housing Units: ",ctynames)
} 

if(lvl == "County to County") {
  # Build dataset
  
    fips <- as.numeric(c(ctyfips1,ctyfips2))
    f.hh <- data.frame()
    for(i in 1:length(fips)) {
    HHSQLcty <- paste0("SELECT countyfips, year, totalhousingunits, vacanthousingunits FROM estimates.county_profiles WHERE countyfips = ", fips[i],";")
    HH <- dbGetQuery(DBPool,HHSQLcty)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(fips[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    HH$county <- namex$municipalityname 
    HH$county <- simpleCap(HH$county)
    f.hh <- bind_rows(f.hh,HH)
  }

 f.hh$occupiedhousingunits <-   f.hh$totalhousingunits - f.hh$vacanthousingunits
  
  #Fixing data for Broomfield
 f.hh <- f.hh[!(f.hh$countyfips == 14 & f.hh$year <=2001),]
  
  f.hh <- f.hh[which(f.hh$year >= 1990),]
  
 # Title
 ctynames <-ctyname2[1]
 if(length(ctyname2) > 1){
    for(i in 2:length(ctyname2)){
         ctynames <- paste0(ctynames,", ",ctyname2[i])
     }
 }

    ctynames <- paste0(ctynames," and ",ctyname1)
    
# Titles
 grTit1 <- paste0("Total Housing Units: ",ctynames)
 grTit2 <- paste0("Occupied Housing Units: ",ctynames)
 grTit3 <- paste0("Vacant Housing Units: ",ctynames)
}

if(lvl == "Municipality to Municipality") {
      fips <- c(placefips2,placefips1)
      fips <- substr(fips,3,7)
      fips <- as.numeric(fips)
      
      plName <- c(placename2, placename1)
      
    f.hh <- data.frame()
    for(i in 1:length(fips)) {
        HHSQLmuni <- paste0("SELECT countyfips, placefips, municipalityname, year, totalhousingunits, occupiedhousingunits, vacanthousingunits FROM estimates.muni_pop_housing WHERE (placefips = ", fips[i],");")
        HH <- dbGetQuery(DBPool,HHSQLmuni)
        HH$municipalityname <- plName[i]
        f.hh <- bind_rows(f.hh,HH)
    }
  # Remove Municipal Total
    f.hh <- f.hh[which(f.hh$countyfips != 999),]
    f.hh <- f.hh[which(f.hh$year >= 1990),]
    
    f.hh <- f.hh %>%
          group_by(placefips,municipalityname, year) %>%
          summarize(totalhousingunits = sum(totalhousingunits),
                    occupiedhousingunits = sum(occupiedhousingunits),
                    vacanthousingunits = sum(vacanthousingunits))  
          
    
  
  
 # Title
 placenames <-placename2[1]
 if(length(placename2) > 1){
    for(i in 2:length(placename2)){
         placenames <- paste0(placenames,", ",placename2[i])
     }
 }

    placenames <- paste0(placenames," and ",placename1)
    
# Titles
 grTit1 <- paste0("Total Housing Units: ",placenames)
 grTit2 <- paste0("Occupied Housing Units: ",placenames)
 grTit3 <- paste0("Vacant Housing Units: ",placenames)
}

 # Setting axis labels 
 x  <- list(title = "")
 y1 <- list(title = "Total Housing Units")
 y2 <- list(title = "Occupied Housing Units")
 y3 <- list(title = "Vacant Housing Units")
 

 
 # Legend
 l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bg_color = "#DCDCDC",
    orientation = "h",
    bordercolor = "#FFFFFF",
    borderwidth = 2)  
if(lvl == "Municipality to Municipality") {
     rollText1 <- paste0(f.hh$municipalityname, "<br>", f.hh$year,": ", format(f.hh$totalhousingunits, scientific=FALSE,big.mark = ","))
   rollText2 <- paste0(f.hh$municipalityname, "<br>", f.hh$year,": ", format(f.hh$occupiedhousingunits, scientific=FALSE,big.mark = ","))
   rollText3 <- paste0(f.hh$municipalityname, "<br>", f.hh$year,": ", format(f.hh$vacanthousingunits, scientific=FALSE,big.mark = ","))

tPlot<-  plot_ly(x=f.hh$year, y=f.hh$totalhousingunits, 
                      type="scatter",mode='lines', color=f.hh$municipalityname,
                      transforms = list( type = 'groupby', groups = f.hh$municipalityname),
                      hoverinfo = "text",
                      text = rollText1) %>% 
               layout(title = grTit1,
                        xaxis = x,
                        yaxis = y1,
                      legend = l,
                      hoverlabel = "right",
                      margin = list(l = 50, r = 50, t = 60, b = 100),   #This Works 
                      annotations = list(text = captionSrc("SDO",""),
                              font = list(size = 12),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3))

oPlot<-  plot_ly(x=f.hh$year, y=f.hh$occupiedhousingunits, 
                      type="scatter",mode='lines', color=f.hh$municipalityname,
                      transforms = list( type = 'groupby', groups = f.hh$municipalityname),
                      hoverinfo = "text",
                      text = rollText2) %>% 
               layout(title = grTit2,
                        xaxis = x,
                        yaxis = y2,
                      legend = l,
                      hoverlabel = "right",
                      margin = list(l = 50, r = 50, t = 60, b = 100),   #This Works 
                      annotations = list(text = captionSrc("SDO",""),
                              font = list(size = 12),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3))

vPlot<-  plot_ly(x=f.hh$year, y=f.hh$vacanthousingunits, 
                      type="scatter",mode='lines', color=f.hh$municipalityname,
                      transforms = list( type = 'groupby', groups = f.hh$municipalityname),
                      hoverinfo = "text",
                      text = rollText3) %>% 
               layout(title = grTit3,
                        xaxis = x,
                        yaxis = y3,
                      legend = l,
                      hoverlabel = "right",
                      margin = list(l = 50, r = 50, t = 60, b = 100),   #This Works 
                      annotations = list(text = captionSrc("SDO",""),
                              font = list(size = 12),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3))
} else {
   rollText1 <- paste0(f.hh$county, " County<br>", f.hh$year,": ", format(f.hh$totalhousingunits, scientific=FALSE,big.mark = ","))
   rollText2 <- paste0(f.hh$county, " County<br>", f.hh$year,": ", format(f.hh$occupiedhousingunits, scientific=FALSE,big.mark = ","))
   rollText3 <- paste0(f.hh$county, " County<br>", f.hh$year,": ", format(f.hh$vacanthousingunits, scientific=FALSE,big.mark = ","))

  tPlot<-  plot_ly(x=f.hh$year, y=f.hh$totalhousingunits, 
                      type="scatter",mode='lines', color=f.hh$county,
                      transforms = list( type = 'groupby', groups = f.hh$county),
                      hoverinfo = "text",
                      text = rollText1) %>% 
               layout(title = grTit1,
                        xaxis = x,
                        yaxis = y1,
                      legend = l,
                      hoverlabel = "right",
                      margin = list(l = 50, r = 50, t = 60, b = 100),   #This Works 
                      annotations = list(text = captionSrc("SDO",""),
                              font = list(size = 12),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3))

 oPlot<-  plot_ly(x=f.hh$year, y=f.hh$occupiedhousingunits, 
                      type="scatter",mode='lines', color=f.hh$county,
                      transforms = list( type = 'groupby', groups = f.hh$county),
                      hoverinfo = "text",
                      text = rollText2) %>% 
               layout(title = grTit2,
                        xaxis = x,
                        yaxis = y2,
                      legend = l,
                      hoverlabel = "right",
                      margin = list(l = 50, r = 50, t = 60, b = 100),   #This Works 
                      annotations = list(text = captionSrc("SDO",""),
                              font = list(size = 12),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3))

vPlot<-  plot_ly(x=f.hh$year, y=f.hh$vacanthousingunits, 
                      type="scatter",mode='lines', color=f.hh$county,
                      transforms = list( type = 'groupby', groups = f.hh$county),
                      hoverinfo = "text",
                      text = rollText3) %>% 
               layout(title = grTit3,
                        xaxis = x,
                        yaxis = y3,
                      legend = l,
                      hoverlabel = "right",
                      margin = list(l = 50, r = 50, t = 60, b = 100),   #This Works 
                      annotations = list(text = captionSrc("SDO",""),
                              font = list(size = 12),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = -0.3))
}
 
  if(lvl == "Municipality to Municipality") {
     names(f.hh) <- c("Place FIPS", "Municipality Name","Year","Total Housing Units","Occupied Housing Units","Vacant Housing Units")
  } else {
    f.hh <- f.hh[,c(1,5,2,3,6,4)]
   names(f.hh) <- c("County FIPS","County Name","Year","Total Housing Units","Occupied Housing Units","Vacant Housing Units")
  }
   
   outList <- list("plot1" = tPlot, "plot2" = oPlot, "plot3" = vPlot,
                   "data" = f.hh)
   return(outList)
  }

