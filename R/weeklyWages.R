#' weeklyWages Produces a plot and dataset showing  average weekly wages
#'  for the period from 2000 to the present
#'
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic, a html or lates table and a dataset
#' @export
#'

weeklyWages <- function(DBPool,lvl, listID, curyr,base=10){
  
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
   f.wages <- data.frame()
   for(i in 1:length(ctyfips1)){
    wagePLSQL <- paste0("SELECT fips, geoname, year, weekly_wage FROM estimates.weekly_wages WHERE fips = ",as.numeric(ctyfips1[i]), ";")
    f.wagePL <- dbGetQuery(DBPool, wagePLSQL)
    f.wages <- bind_rows(f.wages,f.wagePL)
   }
  grTitle <- paste0("Average Weekly Wage: ",ctyname1)
}

if(lvl == "Region to County"){

   #Building regional data
   f.wagesR <- data.frame()
   f.popR <- data.frame()
   for(i in 1:length(ctyfips1)) {
     #Wages data
      wagePLSQL <- paste0("SELECT fips, geoname, year, weekly_wage FROM estimates.weekly_wages WHERE fips = ",as.numeric(ctyfips1[i]), ";")
      f.wagePL <- dbGetQuery(DBPool, wagePLSQL)
      f.wagesR <- bind_rows(f.wagesR,f.wagePL)
     
     # Population data
      popSQL <- paste0("SELECT countyfips, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips1[i]), " and placefips = 0;")
      f.popPL <- dbGetQuery(DBPool,popSQL)
      f.popR <- bind_rows(f.popR,f.popPL)
   }
   
   # Combining wage and population
   
   
   f.wagepop <- inner_join(f.wagesR,f.popR, by=c("fips" = "countyfips","year" = "year"))
   f.wagepop$wagetot <- f.wagepop$weekly_wage * f.wagepop$totalpopulation
     
   f.wagesR <- f.wagepop %>% group_by(year) %>%
        summarise(tot_pop = sum(totalpopulation),
                  weekly_sum = sum(wagetot)) 
   
   f.wagesR$weekly_wage = round(f.wagesR$weekly_sum/f.wagesR$tot_pop,digits=0)
   
   f.wagesR$geoname <- ctyname1
   f.wagesR$fips <- 1000
   
   #Building County data
   
   f.wagesC <- data.frame()
    for(i in 1:length(ctyfips2)) {
        wagePLSQL <- paste0("SELECT fips, geoname, year, weekly_wage FROM estimates.weekly_wages WHERE fips = ",as.numeric(ctyfips2[i]), ";")
        f.wageBase <- dbGetQuery(DBPool, wagePLSQL)
        f.wagesC <- bind_rows(f.wagesC,f.wageBase)
      }
     
    
    f.wages <- bind_rows(f.wagesR[,c(6,1,5,4)],f.wagesC[,c(1,3,2,4)])
    
    revCty <- toString(ctyname2,sep=', ')
    grTitle <- paste0("Average Weekly Wages: ",revCty, " compared to ",ctyname1)
}

  if(lvl == "County to County") {
    ctyfips <- c(ctyfips1, ctyfips2)
    ctyname <- c(ctyname1, ctyname2)
    
  
    f.wages <- data.frame()
    for(i in 1:length(ctyfips)) {
        wagePLSQL <- paste0("SELECT fips, geoname, year, weekly_wage FROM estimates.weekly_wages WHERE fips = ",as.numeric(ctyfips[i]), ";")
        f.wageBase <- dbGetQuery(DBPool, wagePLSQL)
        f.wages <- bind_rows(f.wages,f.wageBase)
      }


     revCty <- toString(ctyname2,sep=', ')
    grTitle <- paste0("Average Weekly Wages: ",revCty, " compared to ",ctyname1)
  }
 
 #Fixing data for Broomfield
 f.wages <- f.wages[!(f.wages$fips == 14 & f.wages$year <= 2001),]
  
    # Setting axis labels 
 x  <- list(title = "")
 y1 <- list(title = "Average Weekly Wage", tickformat= "$")

 
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

rollText <- paste0(f.wages$geoname, "<br>", f.wages$year,": ", paste0("$", formatC(f.wages$weekly_wage, format="f", digits=2, big.mark=",")))

wageplot <-  plot_ly(x=f.wages$year, y=f.wages$weekly_wage, 
                      type="scatter",mode='lines', color=f.wages$geoname,
                      transforms = list( type = 'groupby', groups = f.wages$geoname),
                      hoverinfo = "text",
                      text = rollText) %>% 
               layout(title = grTitle,
                        xaxis = x,
                        yaxis = y1,
                      legend = l,
                      hoverlabel = "right")


f.wages[is.na(f.wages)] <- ""
f.wages$weekly_wage <- paste0("$", format(round(as.numeric(f.wages$weekly_wage),digits=2), big.mark=",", scientific=FALSE)) 
f.wages$weekly_wage <- ifelse(gsub(" ","",f.wages$weekly_wage) == "$NA"," ",f.wages$weekly_wage)

names(f.wages) <- c("County FIPS","County Name","Year","Average Weekly Wage") 
  outList <- list("plot"= wageplot, "data" = f.wages)
  
  return(outList)
}
