#' jobsPopForecast Produces a plot and table showing changes in forecasted jobs and population
#'  for the period from 2000 to 2035
#'
#'   This includes code to output data for the Denver-Boulder MSA when Adams, Arapahoe, Boulder,
#'     Broomfield, Denver, Douglas, or Jefferson County are selected.
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic, a html or latex table and a dataset
#' @export

jobsPopForecast <- function(DBPool,lvl,listID, curyr, base=10){

     # Collecting place ids from  idList, setting default values
  PMSA <- c("001", "005", "013", "014", "031", "035", "059")
  PMSANames <- c("Adams County","Arapahoe County","Boulder County","Broomfield County",
              "Denver County","Douglas County","Jefferson County")

  
  ctyfips1 <- listID$ctyNum1
  ctyname1 <- listID$ctyName1
  
  ctyfips2 <- listID$ctyNum2
  ctyname2 <- listID$ctyName2
  
  placefips1 <- listID$plNum1
  placename1 <- listID$plName1
  
  placefips2 <- listID$plNum2
  placename2 <- listID$plName2
  
  #fips is the 3-digit character string

 if(lvl == "Regional Summary") { 

  f.jobsind <- data.frame()
  if(ctyname1 == "Denver PMSA") {
     jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast where countyfips= 500;")
     f.jobsind <- dbGetQuery(DBPool, jobsSQL)
     f.jobsind$county <- ctyname1 
  } else {
      for(i in 1:length(ctyfips1)) {
         jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast WHERE countyfips = ",as.numeric(ctyfips1[i]), ";")
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsBase$county <- CountyName(paste0("08",ctyfips1[i]))  # Adding County name
         f.jobsind <- bind_rows(f.jobsind,f.jobsBase)
      }
  }

  grTitle <- paste0("Jobs Estimate and Forecast: ",ctyname1)
 }
  
  if(lvl == "Region to County"){

   #Building regional data
   f.jobsindR <- data.frame()
  if(ctyname1 == "Denver PMSA") {
     jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast where countyfips = 500;")
     f.jobsindR <- dbGetQuery(DBPool, jobsSQL)
     f.jobsindR$county <- ctyname1 
  } else {
      for(i in 1:length(ctyfips1)) {
         jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast WHERE countyfips = ",as.numeric(ctyfips1[i]), ";")
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsBase$county <- CountyName(paste0("08",ctyfips1[i]))  # Adding County name
         f.jobsindR <- bind_rows(f.jobsindR,f.jobsBase)
      }
    #Summarize record
    f.jobsindR <- f.jobsindR %>% group_by(population_year,datatype) %>%
        summarise(totaljobs = sum(totaljobs))

    f.jobsindR$county <- ctyname1
    f.jobsindR$countyfips <- 1000
  }
   
  
   #Building County data
   
   ctyfips2 <- ctyfips2[which(!ctyfips2 %in% PMSA )]
   ctyname2 <- ctyname2[which(!ctyname2 %in% PMSANames)]
   
   f.jobsindC <- data.frame()
    for(i in 1:length(ctyfips2)) {
         jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast WHERE countyfips = ",as.numeric(ctyfips2[i]), ";")
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsBase$county <- CountyName(paste0("08",ctyfips2[i]))  # Adding County name
         f.jobsindC <- bind_rows(f.jobsindC,f.jobsBase)
      }
     
    
    f.jobsind <- bind_rows(f.jobsindR,f.jobsindC)
 
    revCty <- toString(ctyname2,sep=', ')
    grTitle <- paste0("Jobs Estimate and Forecast: ",revCty, " compared to ",ctyname1)
}

  if(lvl == "County to County") {
    # Selecting out records 
    if((ctyfips1 %in% PMSA) || (ctyfips2 %in% PMSA)) {
            jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast where countyfips = 500;")
            f.jobsindR <- dbGetQuery(DBPool, jobsSQL)
            f.jobsindR$county <- "Denver PMSA"  # Adding County name
    }
    
    ctyfips <- c(ctyfips1[which(!ctyfips1 %in% PMSA)], ctyfips2[which(!ctyfips2 %in% PMSA )])
    ctynames <- c(ctyfips1[which(!ctyname1 %in% PMSANames)], ctyname2[which(!ctyname2 %in% PMSANames )])
    # Adjustment for ctyname1
    if(ctyname1 %in% PMSANames) {
      ctyname1 <- paste0(ctyname1,"/Denver PMSA")
    }
      #Building County data
   f.jobsindC <- data.frame()
    for(i in 1:length(ctyfips)) {
        jobsSQL <- paste0("SELECT * FROM estimates.jobs_forecast WHERE countyfips = ",as.numeric(ctyfips[i]), ";")
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsBase$county <- CountyName(paste0("08",ctyfips[i]))  # Adding County name
         f.jobsindC <- bind_rows(f.jobsindC,f.jobsBase)
      }
     
    
    if(exists("f.jobsindR")) {
      f.jobsind <- bind_rows(f.jobsindR,f.jobsindC)
    } else {
      f.jobsind <- f.jobsindC
    }
    
    
     revCty <- toString(ctynames,sep=', ')
    grTitle <- paste0("Jobs Estimate and Forecast: ",revCty, " compared to ",ctyname1)
  }


  f.plotdata <- f.jobsind
 
  f.plotdata <- f.plotdata[which(f.plotdata$population_year >= 2010 & f.plotdata$population_year <= 2040),]
  f.plotdata$totaljobs <- round(f.plotdata$totaljobs,digits=0)
 
  
 f.plotest <-  f.plotdata[which(f.plotdata$datatype == "ESTIMATE"),] 
 f.plotfore <-  f.plotdata[which(f.plotdata$datatype == "FORECAST"),] 

  f.plotest$datatype <- simpleCap(f.plotest$datatype) 
  f.plotfore$datatype <- simpleCap(f.plotfore$datatype)
  
   # Setting axis labels 
 x  <- list(title = "")
 y1 <- list(title = "Total Jobs")

 
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

rollText <- paste0(f.plotfore$county, "<br>", f.plotfore$population_year,": ", format(f.plotfore$totaljobs, scientific=FALSE,big.mark = ","),"<br>",f.plotfore$datatype)
browser()
jobsplot <-  plot_ly(x=f.plotest$population_year, y=f.plotest$totaljobs, 
                      type="scatter",mode='lines', color=f.plotest$county,
                      transforms = list( type = 'groupby', groups = f.plotest$county),
                      hoverinfo = "text",
                      text = ~paste0(f.plotest$county, "<br>", f.plotest$population_year,": ", format(f.plotest$totaljobs, scientific=FALSE,big.mark = ","),"<br>", f.plotest$datatype)) %>% 
               add_lines(x=f.plotfore$population_year, y=f.plotfore$totaljobs, 
                      type="scatter",mode='lines', color=f.plotfore$county, line = list(dash="dash"),
                      transforms = list(type = 'groupby', groups = f.plotfore$county),
                      hoverinfo = "text",
                      text = rollText, showlegend=FALSE) %>%
               layout(title = grTitle,
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

f.plotdata$totaljobs <- format(as.numeric(f.plotdata$totaljobs), big.mark=",", scientific=FALSE)
f.plotdata$datatype <- simpleCap(f.plotdata$datatype)
f.plotdata <- f.plotdata[,c(2,6,5,3,4)]
names(f.plotdata) <- c("County FIPS","County Name","Data Type","Year","Total Jobs")
  
  outList <- list("plot"= jobsplot, "data" = f.plotdata)


  return(outList)
}
