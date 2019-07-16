#' baseIndustries outputs the base_industries job plot and data
#'
#'   This includes code to output data for the Denver-Boulder MSA when Adams, Arapahoe, Boulder,
#'     Broomfield, Denver, Douglas, or Jefferson County are selected.
#'
#' @param  fips numeric, county-level FIPS code
#' @param  ctyname County Name string, from input$unit
#' @param  curyr the current year
#' @return Plotly bar chart with dropdown and dataset
#' @export

baseIndustries <- function(DBPool,lvl,listID, curyr, oType,base=10){
  

   # Collecting place ids from  idList, setting default values
  
  ctyfips1 <- listID$ctyNum1
  ctyname1 <- listID$ctyName1
  
  ctyfips2 <- listID$ctyNum2
  ctyname2 <- listID$ctyName2
  
  placefips1 <- listID$plNum1
  placename1 <- listID$plName1
  
  placefips2 <- listID$plNum2
  placename2 <- listID$plName2


if(lvl == "Regional Summary") { 
  f.jobsind <- data.frame()
  if(ctyname1 == "Denver PMSA") {
     jobsSQL <- paste0("SELECT * FROM estimates.base_analysis where fips= '500';")
     f.jobsind <- dbGetQuery(DBPool, jobsSQL)
  } else {
      for(i in 1:length(ctyfips1)) {
         jobsSQL <- paste0("SELECT * FROM estimates.base_analysis WHERE fips = '",ctyfips1[i], "';")
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsind <- bind_rows(f.jobsind,f.jobsBase)
      }
  }
   grTitle <- paste0(curyr, " Base Industries (without Indirect): ",ctyname1)
} 
 
if(lvl == "Region to County"){

   #Building regional data
   f.jobsindR <- data.frame()
  if(ctyname1 == "Denver PMSA") {
     jobsSQL <- paste0("SELECT * FROM estimates.base_analysis where fips = '500';")
     f.jobsindR <- dbGetQuery(DBPool, jobsSQL)
  } else {
      for(i in 1:length(ctyfips1)) {
         jobsSQL <- paste0("SELECT * FROM estimates.base_analysis WHERE fips = '",ctyfips1[i], "';")
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsindR <- bind_rows(f.jobsindR,f.jobsBase)
      }
    #Summarize record
    f.jobsindR <- f.jobsindR %>% summarise_at(vars(employment:direct_basic_emp), sum, na.rm = TRUE)
    f.jobsindR$id <- 0
    f.jobsindR$fips <- "0"
    f.jobsindR$ctype <- 0
    f.jobsindR$cname <- ctyname1
    f.jobsindR <- f.jobsindR[,c(31:34,1:30)]
  }
 
   #Building County data
   f.jobsindC <- data.frame()
    for(i in 1:length(ctyfips2)) {
       if(ctyfips2[i] %in% c("001", "005", "013", "014", "031", "035", "059")) {
            jobsSQL <- paste0("SELECT * FROM estimates.base_analysis where fips = '500';")
       } else {
         jobsSQL <- paste0("SELECT * FROM estimates.base_analysis WHERE fips = '",ctyfips2[i], "';")
       } 
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsindC <- bind_rows(f.jobsindC,f.jobsBase)
      }
     
    f.jobsindC <-  distinct(f.jobsindC,fips, .keep_all = TRUE)
    
    f.jobsind <- bind_rows(f.jobsindR,f.jobsindC)
    grTitle <- paste0(curyr, " Base Industries (without Indirect): ",ctyname2, " compared to ",ctyname1)
}

  if(lvl == "County to County") {
    ctyfips <- c(ctyfips1, ctyfips2)
      #Building County data
   f.jobsindC <- data.frame()
    for(i in 1:length(ctyfips)) {
       if(ctyfips2[i] %in% c("001", "005", "013", "014", "031", "035", "059")) {
            jobsSQL <- paste0("SELECT * FROM estimates.base_analysis where fips = '500';")
       } else {
         jobsSQL <- paste0("SELECT * FROM estimates.base_analysis WHERE fips = '",ctyfips[i], "';")
       } 
         f.jobsBase <- dbGetQuery(DBPool, jobsSQL)
         f.jobsindC <- bind_rows(f.jobsindC,f.jobsBase)
      }
     
    f.jobsindC <-  distinct(f.jobsindC,fips, .keep_all = TRUE)
    
    f.jobsind <- f.jobsindC
    grTitle <- paste0(curyr, " Base Industries (without Indirect): ",ctyname1, " compared to ",ctyname2)
  }
  
  # convert datasets to long
    

  f.jobsindL <- gather(f.jobsind, industry, jobs, employment:direct_basic_emp)

  f.jobsindL$industry <- if_else(f.jobsindL$industry == "agri_emp","Agriculture",
                                  if_else(f.jobsindL$industry == "mining_emp","Mining",
                                          if_else(f.jobsindL$industry == "manuf_emp","Manufacturing",
                                                  if_else(f.jobsindL$industry == "govt_emp","Government",
                                                          if_else(f.jobsindL$industry == "regl_serv_emp","Regional Services",
                                                                  if_else(f.jobsindL$industry == "tourism_emp","Tourism",
                                                                          if_else(f.jobsindL$industry == "other_hhd_emp","Other Household",
                                                                                  if_else(f.jobsindL$industry == "retiree_emp","Retiree(s)",
                                                                                          if_else(f.jobsindL$industry == "commuter_emp", "Commuter",
                                                                                                  if_else(f.jobsindL$industry == "other_inc_emp","Other Income","drop"))))))))))


# Building Plot Data
 f.jobsBaseFin <- f.jobsindL[which(f.jobsindL$industry != "drop"),]

 f.jobsBaseFin <-  f.jobsBaseFin %>%
   group_by(cname,industry) %>%
   summarize(Njobs = sum(jobs)) %>%
   mutate (prop = Njobs/sum(Njobs), 
            pct = percent(prop * 100))
 
 f.jobsBaseFin$indText  <- paste0(f.jobsBaseFin$cname,": ",f.jobsBaseFin$pct)  
 

indList <- unique(f.jobsBaseFin$industry)

# Regional Plotly chart


BaseBars <- f.jobsBaseFin %>%
  plot_ly(
    type = 'bar', 
    x = ~cname, 
    y = ~prop,
  #  color=~cname,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~industry,
        operation = '=',
        value = unique(f.jobsBaseFin$industry)[1]
      )
  )) %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='County'),
          showlegend = FALSE,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[1]),
               label = unique(f.jobsBaseFin$industry)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[2]),
               label = unique(f.jobsBaseFin$industry)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[3]),
               label = unique(f.jobsBaseFin$industry)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[4]),
               label = unique(f.jobsBaseFin$industry)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[5]),
               label = unique(f.jobsBaseFin$industry)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[6]),
               label = unique(f.jobsBaseFin$industry)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[7]),
               label = unique(f.jobsBaseFin$industry)[7]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[8]),
               label = unique(f.jobsBaseFin$industry)[8]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[9]),
               label = unique(f.jobsBaseFin$industry)[9]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.jobsBaseFin$industry)[10]),
               label = unique(f.jobsBaseFin$industry)[10])
      )
  )))

  outList <- list("plot" = BaseBars, "data"=f.jobsBaseFin)
  return(outList)
}
