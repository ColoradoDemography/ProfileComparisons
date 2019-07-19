#' Unemployment Creates a plot comapairing unemployment and percent of populaton in labor force
#'
#'
#' @param listID the list containing place id and Place names
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphics and associated data sets
#' @export

unemployment <- function(DBPool,lvl,listID, curyr, base=10){
  
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
   f.unemp <- data.frame()
   for(i in 1:length(ctyfips1)){
    UnempSQL <-  paste0("SELECT * FROM estimates.bls_unemployment WHERE fips = ",as.numeric(ctyfips1[i]), ";")
    f.unempPL <- dbGetQuery(DBPool, UnempSQL)
    f.unempPL$county <- CountyName(paste0("08",ctyfips1[i]))  # Adding County name
    f.unemp <- bind_rows(f.unemp,f.unempPL)
   }
 
  f.unemp$unemprate <- f.unemp$unemprate/100
   grTitle <- paste0("Unemployment Rate: ",ctyname1)
}

if(lvl == "Region to County"){

   #Building regional data
    f.unemp <- data.frame()
   for(i in 1:length(ctyfips1)){
    UnempSQL <-  paste0("SELECT * FROM estimates.bls_unemployment WHERE fips = ",as.numeric(ctyfips1[i]), ";")
    f.unempPL <- dbGetQuery(DBPool, UnempSQL)
    f.unemp <- bind_rows(f.unemp,f.unempPL)
   }
   
   # Combining wage and population
   
     
   f.unempR <- f.unemp %>% group_by(year) %>%
        summarise(tot_lfpart = sum(lfpart),
                  tot_unempl = sum(unemployment)) 
   
   f.unempR$unemprate = round((f.unempR$tot_unempl/f.unempR$tot_lfpart),digits=3)
   
   f.unempR$county <- ctyname1
   f.unempR$fips <- 1000
   
   #Building County data
   f.unempC <- data.frame()
    for(i in 1:length(ctyfips2)) {
        UnempSQL <-  paste0("SELECT * FROM estimates.bls_unemployment WHERE fips = ",as.numeric(ctyfips2[i]), ";")
        f.unempPL <- dbGetQuery(DBPool, UnempSQL)
        f.unempPL$county <- CountyName(paste0("08",ctyfips2[i]))  # Adding County name
        f.unempC <- bind_rows(f.unempC,f.unempPL)
      }
      f.unempC$unemprate <- f.unempC$unemprate/100
    
    f.unemp <- bind_rows(f.unempR[,c(6,1,5,4)],f.unempC[,c(1,2,7,6)])
    
    revCty <- toString(ctyname2,sep=', ')
    grTitle <- paste0("Unemployment Rate: ",revCty, " compared to ",ctyname1)
}

  if(lvl == "County to County") {
     ctyfips <- c(ctyfips2, ctyfips1)

  
    f.unemp <- data.frame()
    for(i in 1:length(ctyfips)) {
        UnempSQL <-  paste0("SELECT * FROM estimates.bls_unemployment WHERE fips = ",as.numeric(ctyfips[i]), ";")
        f.unempPL <- dbGetQuery(DBPool, UnempSQL)
        f.unempPL$county <- CountyName(paste0("08",ctyfips[i]))  # Adding County name
        f.unemp <- bind_rows(f.unemp,f.unempPL)
      }

    f.unemp$unemprate <- f.unemp$unemprate/100
     revCty <- toString(ctyname2,sep=', ')
    grTitle <- paste0("Unemployment Rate: ",revCty, " compared to ",ctyname1)
  }
 
 #Fixing data for Broomfield
 f.unemp$unemprate <- ifelse(f.unemp$fips == 14 & f.unemp$year <= 2001, NA,f.unemp$unemprate)

 maxUnemp <- max(f.unemp$unemprate,na.rm=TRUE) 
 
    # Setting axis labels 
 x  <- list(title = "")
 y1 <- list(title = "Unemployment Rate", tickformat= "%")

 
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


unemplplot <-  plot_ly(x=f.unemp$year, y=f.unemp$unemprate, 
                      type="scatter",mode='lines', color=f.unemp$county,
                      transforms = list( type = 'groupby', groups = f.unemp$county),
                      hoverinfo = "text",
                      text = ~paste0(f.unemp$county, "<br>", f.unemp$year,": ", paste0(round(f.unemp$unemprate*100,digits=1),"%"))) %>% 
               layout(title = grTitle,
                        xaxis = x,
                        yaxis = y1,
                        legend = l,
                        hoverlabel = "right",
                     shapes = list(
               list(type = "rect",
                    fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                    x0 = "2001", x1 = "2002", xref = "x",
                    y0 = 0, y1 = maxUnemp, yref = "y"),
               list(type = "rect",
                 fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                 x0 = "2008", x1 = "2010", xref = "x",
                 y0 = 0, y1 = maxUnemp, yref = "y")))

  
  outList <- list("plot"= unemplplot, "data" = f.unemp)
  return(outList)
}
