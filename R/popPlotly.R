#' popPlotly Outputs Population Estimates and Forecasts Maps for Counties
#' 
#'
#' @param lvl the data level, Counties, Municipalities 
#' @param listID the list containing place id and Place names
#' @param sYr Start Year
#' @param eYr End year
#' @return kable formatted  table and data file
#' @export
#'
popPlotly <- function(DBPool,lvl,listID) {
  outMessage  <- ""
  sYr = 1990
  eYr = 2050

  # Collecting place ids from  idList, setting default values
  
  ctyfips1 <- listID$ctyNum1
  ctyname1 <- listID$ctyName1
  
  ctyfips2 <- listID$ctyNum2
  ctyname2 <- listID$ctyName2
  
  placefips1 <- listID$plNum1
  placename1 <- listID$plName1
  
  placefips2 <- listID$plNum2
  placename2 <- listID$plName2

  yrs <- seq(sYr,eYr,by=1)
  
if(lvl == "Regional Summary") { 
 
# Build Fips List
fips <- as.numeric(ctyfips1)

# Build dataset
d <- county_sya(fips, yrs)  %>%
      group_by(countyfips, county, year, datatype) %>%
      summarize(Tot_pop = sum(as.numeric(totalpopulation)))

#Fixing data for Broomfield
d <- d[!(d$countyfips == 14 & d$year <= 2001),]

 d$Tot_pop <- round(d$Tot_pop,0)
 d_estimate <- d[which(d$datatype == "Estimate"),] 
 d_forecast <- d[which(d$datatype == "Forecast"),] 
 
# Natural Increase and Net Migration

   f.cocreg <- data.frame()
  for(i in 1:length(ctyfips1)) {
    cocStr <- paste0("SELECT countyfips, year, naturalincrease, netmigration FROM estimates.county_profiles WHERE countyfips = ",as.numeric(ctyfips1[i]),";")
    cocx <- dbGetQuery(DBPool,cocStr)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips1[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    cocx$county <- namex$municipalityname 
    f.cocreg <- bind_rows(f.cocreg,cocx)
  }
  
  f.migr <-  f.cocreg %>%
         mutate(countyfips = countyfips, 
                county = county, 
                year = as.numeric(year),
                Nat_incr = as.numeric(naturalincrease),
                Net_Mig = as.numeric(netmigration))

  #Fixing data for Broomfield

f.migr <- f.migr[!(f.migr$countyfips == 14 & f.migr$year <= 2001),]
f.migr <- f.migr[which(f.migr$year >= 1990),]

# Titles
 grTit1 <- paste0("Total Population Estimates and Forecasts: ",ctyname1)
 grTit2 <- paste0("Natural Increase: ",ctyname1)
 grTit3 <- paste0("Net Migration: ",ctyname1)
   
}

if(lvl == "Region to County") {

# Build dataset
#region
fipsR <- as.numeric(ctyfips1)
d_region <- county_sya(fipsR, yrs)  %>%
      group_by(year, datatype) %>%
      summarize(Tot_pop = sum(as.numeric(totalpopulation)))
d_region$countyfips <- 1000
d_region$county <- ctyname1
d_region <- d_region[,c(4,5,1,2,3)]

#County
fipsC <-  as.numeric(ctyfips2)
d_county <- county_sya(fipsC, yrs)  %>%
      group_by(countyfips, county, year, datatype) %>%
      summarize(Tot_pop = sum(as.numeric(totalpopulation)))

d <- bind_rows(d_county,d_region)

#Fixing data for Broomfield
d <- d[!(d$countyfips == 14 & d$year <= 2001),]

 d$Tot_pop <- round(d$Tot_pop,0)
 d_estimate <- d[which(d$datatype == "Estimate"),] 
 d_forecast <- d[which(d$datatype == "Forecast"),] 

 # Natural Increase and Net Migration

  f.cocreg <- data.frame()
  for(i in 1:length(ctyfips1)) {
    cocStr <- paste0("SELECT countyfips, year, naturalincrease, netmigration FROM estimates.county_profiles WHERE countyfips = ",as.numeric(ctyfips1[i]),";")
    cocx <- dbGetQuery(DBPool,cocStr)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips1[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    cocx$county <- namex$municipalityname 
    f.cocreg <- bind_rows(f.cocreg,cocx)
  }

   f.cocregsum <- f.cocreg %>%
        group_by(year) %>%
      summarize(Nat_incr = sum(as.numeric(naturalincrease)),
                Net_Mig = sum(as.numeric(netmigration)))
   
   f.cocregsum$countyfips <- 1000
   f.cocregsum$county <- ctyname1
   f.cocregsum <- f.cocregsum[,c(4,5,1,2,3)]
   
# County
    cocStr <- paste0("SELECT countyfips, year, naturalincrease, netmigration FROM estimates.county_profiles WHERE countyfips = ",as.numeric(ctyfips2),";")
    cocx <- dbGetQuery(DBPool,cocStr)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips2)," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    cocx$county <- namex$municipalityname 
    f.coccty <- cocx %>%
      mutate(countyfips = countyfips, 
                county = county, 
                year = as.numeric(year),
                Nat_incr = as.numeric(naturalincrease),
                Net_Mig = as.numeric(netmigration))
    f.coccty <- f.coccty[,c(1,2,5,6,7)]
  
  f.migr <-  bind_rows(f.coccty,f.cocregsum) 
         

  #Fixing data for Broomfield
f.migr <- f.migr[!(f.migr$countyfips == 14 & f.migr$year <= 2001),]
f.migr <- f.migr[which(f.migr$year >= 1990),]


 # Title
 ctynames <-ctyname2[1]
 if(length(ctyname2) > 1){
    for(i in 2:length(ctyname2)){
         ctynames <- paste0(ctynames,", ",ctyname2[i])
     }
 }

    ctynames <- paste0(ctynames," and ",ctyname1)
    
# Titles
 grTit1 <- paste0("Total Population Estimates and Forecasts: ",ctynames)
 grTit2 <- paste0("Natural Increase: ",ctynames)
 grTit3 <- paste0("Net Migration: ",ctynames)
}
  
if(lvl == "County to County") {
# Build dataset
fips <- as.numeric(c(ctyfips1,ctyfips2))

d <- county_sya(fips, yrs)  %>%
      group_by(countyfips, county, year, datatype) %>%
      summarize(Tot_pop = sum(as.numeric(totalpopulation)))


#Fixing data for Broomfield
d <- d[!(d$countyfips == 14 & d$year <= 2001),]

 d$Tot_pop <- round(d$Tot_pop,0)
 d_estimate <- d[which(d$datatype == "Estimate"),] 
 d_forecast <- d[which(d$datatype == "Forecast"),] 


 # Natural Increase and Net Migration

  f.cocreg <- data.frame()
  for(i in 1:length(fips)) {
    cocStr <- paste0("SELECT countyfips, year, naturalincrease, netmigration FROM estimates.county_profiles WHERE countyfips = ",as.numeric(fips[i]),";")
    cocx <- dbGetQuery(DBPool,cocStr)
    nameStr <- paste0("SELECT countyfips, municipalityname FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(fips[i])," and year = 1990 and placefips = 0;")
    namex <- dbGetQuery(DBPool,nameStr)
    cocx$county <- namex$municipalityname 
    f.cocreg <- bind_rows(f.cocreg,cocx)
  }

   f.migr <- f.cocreg %>%
      mutate(countyfips = countyfips, 
                county = county, 
                year = as.numeric(year),
                Nat_incr = as.numeric(naturalincrease),
                Net_Mig = as.numeric(netmigration))
         

#Fixing data for Broomfield
f.migr <- f.migr[!(f.migr$countyfips == 14 & f.migr$year <= 2001),]
f.migr <- f.migr[which(f.migr$year >= 1990),]


 # Title
 ctynames <-ctyname2[1]
  if(length(ctyname2) > 1){
    for(i in 2:length(ctyname2)){
         ctynames <- paste0(ctynames,", ",ctyname2[i])
     }
  }
    ctynames <- paste0(ctynames," and ",ctyname1)
    
# Titles
 grTit1 <- paste0("Total Population Estimates and Forecasts: ",ctynames)
 grTit2 <- paste0("Natural Increase: ",ctynames)
 grTit3 <- paste0("Net Migration: ",ctynames)
}

 # Setting axis labels 
 x  <- list(title = "")
 y1 <- list(title = "Total Population")
 y2 <- list(title = "Natural Increase")
 y3 <- list(title = "Net Migration")
 
 rollText1 <- paste0(d_estimate$county, " County<br>", d_estimate$year,": ", format(d_estimate$Tot_pop, scientific=FALSE,big.mark = ","),"<br>", d_estimate$datatype) 
 rollText2 <- paste0(d_forecast$county, " County<br>", d_forecast$year,": ", format(d_forecast$Tot_pop, scientific=FALSE,big.mark = ","),"<br>",d_forecast$datatype)
 rollText3 <- paste0(f.migr$county, " County<br>", f.migr$year,": ", format(f.migr$Nat_incr, scientific=FALSE,big.mark = ","))
 rollText4 <- paste0(f.migr$county, " County<br>", f.migr$year,": ", format(f.migr$Net_Mig, scientific=FALSE,big.mark = ","))
 
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


countyplot <-  plot_ly(x=d_estimate$year, y=d_estimate$Tot_pop, 
                      type="scatter",mode='lines', color=d_estimate$county,
                      transforms = list( type = 'groupby', groups = d_estimate$county),
                      hoverinfo = "text",
                      text = rollText1) %>% 
               add_lines(x=d_forecast$year, y=d_forecast$Tot_pop, 
                      type="scatter",mode='lines', color=d_forecast$county, line = list(dash="dash"),
                      transforms = list(type = 'groupby', groups = d_forecast$county),
                      hoverinfo = "text",
                      text = rollText2) %>%
               layout(title = grTit1,
                        xaxis = x,
                        yaxis = y1,
                      showlegend = FALSE,
                      hoverlabel = "right")


natIncrease <-  plot_ly(x=f.migr$year, y=f.migr$Nat_incr, 
                      type="scatter",mode='lines', color=f.migr$county,
                      transforms = list( type = 'groupby', groups = f.migr$county),
                      hoverinfo = "text",
                      text = rollText3) %>% 
               layout(title = grTit2,
                        xaxis = x,
                        yaxis = y2,
                      legend = l,
                      hoverlabel = "right")

netMigr <-  plot_ly(x=f.migr$year, y=f.migr$Net_Mig, 
                      type="scatter",mode='lines', color=f.migr$county,
                      transforms = list( type = 'groupby', groups = f.migr$county),
                      hoverinfo = "text",
                      text = rollText4) %>% 
               layout(title = grTit3,
                        xaxis = x,
                        yaxis = y3,
                      legend = l,
                      hoverlabel = "right")


if(lvl == "Regional Summary") {
   f.migr <- f.migr[,c(1,5,2,6,7)]
}

names(f.migr) <- c("County FIPS","County Name","Year","Natural Increase", "Net Migration")
names(d) <- c("County FIPS","County Name","Year",	"Data Type","Population")

outlist <- list("plot1" = countyplot, "plot2" = natIncrease, "plot3" = netMigr,
                "data1" =  d, "data2" = f.migr)


return(outlist)
  }
  
