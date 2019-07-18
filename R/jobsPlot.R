#' jobsPlot Creates a Chart showing the Total Estimated Numer of Firms and Jobs series for each County in Colorado
#'
#' Modified from ms_jobs March, 2019 AB
#'
#' Uses State Demography Office data to create a chart showing the timeseries of Total Estimated Jobs
#' (which means it includes Proprietors and Agricultural Workers) for a selected Colorado County
#'
#' @param listID the list containing place id and Place names
#' @param maxyr The maximum year value, from CurYr
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
#' @return ggplot graphic and data file
#' @export
#'
jobsPlot=function(DBPool, lvl,listID, maxyr){
  
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
    jobs_data <- data.frame()
    firms_data <- data.frame()
    for(i in 1:length(ctyfips1)) {
      # Jobs Data
       jobsStr <- paste0("SELECT * FROM estimates.jobs_by_sector WHERE area_code = ", as.numeric(ctyfips1[i]), " AND sector_id = '0';")
       jobs_cty <-   dbGetQuery(DBPool,jobsStr) %>%
                      filter(population_year >= 2001 & population_year <= maxyr) %>%
                      mutate(jobs=round(total_jobs,0),
                             year=population_year)
       jobs_data <- bind_rows(jobs_data,jobs_cty)
       
    # firms data
        sqlStrFirms <- paste0("SELECT * FROM estimates.firm_count WHERE fips = ", ctyfips1[i],";")
        firms_cty <-  dbGetQuery(DBPool, sqlStrFirms)
        firms_data <- bind_rows(firms_data,firms_cty)
    }
     jobs_data <- jobs_data[which(!is.na(jobs_data$jobs)),]
     firms_data <- subset(firms_data, (!is.na(firms_data$firms)))
     
       #Adding county to jobs_data
    counties <- unique(firms_data[,c(1,2)])
    names(counties) <- c("area_code","geoname")
    jobs_data <- inner_join(jobs_data,counties, by="area_code")
    
     grTit1 <- paste0("Estimated Jobs: ",ctyname1)
     grTit2 <- paste0("Estimated Firms: ",ctyname1)
  }

 if(lvl == "Region to County"){
    jobs_data1 <- data.frame()
    firms_data1 <- data.frame()
    for(i in 1:length(ctyfips1)) {
      # Jobs Data
       jobsStr <- paste0("SELECT * FROM estimates.jobs_by_sector WHERE area_code = ", as.numeric(ctyfips1[i]), " AND sector_id = '0';")
       jobs_cty <-   dbGetQuery(DBPool,jobsStr) %>%
                      filter(population_year >= 2001 & population_year <= maxyr) %>%
                      mutate(jobs=round(total_jobs,0),
                             year=population_year)
       jobs_data1 <- bind_rows(jobs_data1,jobs_cty)
       
    # firms data
        sqlStrFirms <- paste0("SELECT * FROM estimates.firm_count WHERE fips = ", ctyfips1[i],";")
        firms_cty <-  dbGetQuery(DBPool, sqlStrFirms)
        firms_data1 <- bind_rows(firms_data1,firms_cty)
    }
    
    jobs_data2 <- data.frame()
    firms_data2 <- data.frame()
    for(i in 1:length(ctyfips2)) {
      # Jobs Data
       jobsStr <- paste0("SELECT * FROM estimates.jobs_by_sector WHERE area_code = ", as.numeric(ctyfips2[i]), " AND sector_id = '0';")
       jobs_cty <-   dbGetQuery(DBPool,jobsStr) %>%
                      filter(population_year >= 2001 & population_year <= maxyr) %>%
                      mutate(jobs=round(total_jobs,0),
                             year=population_year)
       jobs_data2 <- bind_rows(jobs_data2,jobs_cty)
       
    # firms data
        sqlStrFirms <- paste0("SELECT * FROM estimates.firm_count WHERE fips = ", ctyfips2[i],";")
        firms_cty <-  dbGetQuery(DBPool, sqlStrFirms)
        firms_data2 <- bind_rows(firms_data2,firms_cty)
    }
    
    #Aggregating Regional records
    jobs_data1 <- jobs_data1 %>% group_by(year) %>%
          summarize(jobs= sum(jobs))
    jobs_data1$geoname <- ctyname1
    
    firms_data1 <- firms_data1 %>% group_by(year) %>%
         summarize(firms= sum(firms))
    firms_data1$geoname <- ctyname1     
    
     
       #Adding county to jobs_data
    counties <- unique(firms_data2[,c(1,2)])
    names(counties) <- c("area_code","geoname")
    jobs_data2 <- inner_join(jobs_data2,counties, by="area_code")

     jobs_data <- bind_rows(jobs_data1,jobs_data2[,c(9,8,10)])
     firms_data <- bind_rows(firms_data1[,c(1,3,2)],firms_data2[,c(3,2,4)])
     
     jobs_data <- jobs_data[which(!is.na(jobs_data$jobs)),]
     firms_data <- subset(firms_data, (!is.na(firms_data$firms)))
    
     grTit1 <- paste0("Estimated Jobs: ",ctyname2, " compared to ",ctyname1)
     grTit2 <- paste0("Estimated Firms: ",ctyname2, " compared to ",ctyname1)
 }
  
 if(lvl == "County to County"){
    ctyfips <- c(ctyfips1,ctyfips2)
    jobs_data <- data.frame()
    firms_data <- data.frame()
    for(i in 1:length(ctyfips)) {
      # Jobs Data
       jobsStr <- paste0("SELECT * FROM estimates.jobs_by_sector WHERE area_code = ", as.numeric(ctyfips[i]), " AND sector_id = '0';")
       jobs_cty <-   dbGetQuery(DBPool,jobsStr) %>%
                      filter(population_year >= 2001 & population_year <= maxyr) %>%
                      mutate(jobs=round(total_jobs,0),
                             year=population_year)
       jobs_data <- bind_rows(jobs_data,jobs_cty)
       
    # firms data
        sqlStrFirms <- paste0("SELECT * FROM estimates.firm_count WHERE fips = ", ctyfips[i],";")
        firms_cty <-  dbGetQuery(DBPool, sqlStrFirms)
        firms_data <- bind_rows(firms_data,firms_cty)
     }
     jobs_data <- jobs_data[which(!is.na(jobs_data$jobs)),]
     firms_data <- subset(firms_data, (!is.na(firms_data$firms)))
     
  #Adding county to jobs_data
    counties <- unique(firms_data[,c(1,2)])
    names(counties) <- c("area_code","geoname")
    jobs_data <- inner_join(jobs_data,counties, by="area_code")
  
     grTit1 <- paste0("Estimated Jobs: ",ctyname2, " compared to ",ctyname1)
     grTit2 <- paste0("Estimated Firms: ",ctyname2, " compared to ",ctyname1)
 } 
  
 
  # Setting axis labels 
 x  <- list(title = "")
 y1 <- list(title = "Number of Jobs")
 y2 <- list(title = "Number of Firms")
 maxJobs <- max(jobs_data$jobs)
 maxFirms <- max(firms_data$firms) 
 
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
 
 
 jobs_plot <-  plot_ly(x=jobs_data$year, y=jobs_data$jobs, 
                      type="scatter",mode='lines', color=jobs_data$geoname,
                      transforms = list( type = 'groupby', groups = jobs_data$geoname),
                      hoverinfo = "text",
                      text = ~paste0(jobs_data$geoname, "<br>", jobs_data$year,": ", format(jobs_data$jobs, scientific=FALSE,big.mark = ","))) %>% 
               layout(title = grTit1,
                        xaxis = x,
                        yaxis = y1,
                        legend = l,
                        hoverlabel = "right",
                     shapes = list(
               list(type = "rect",
                    fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                    x0 = "2001", x1 = "2002", xref = "x",
                    y0 = 0, y1 = maxJobs, yref = "y"),
               list(type = "rect",
                 fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                 x0 = "2008", x1 = "2010", xref = "x",
                 y0 = 0, y1 = maxJobs, yref = "y")))
 
 firms_plot <-  plot_ly(x=firms_data$year, y=firms_data$firms, 
                      type="scatter",mode='lines', color=firms_data$geoname,
                      transforms = list( type = 'groupby', groups = firms_data$geoname),
                      hoverinfo = "text",
                      text = ~paste0(firms_data$geoname, "<br>", firms_data$year,": ", format(firms_data$firms, scientific=FALSE,big.mark = ","))) %>% 
               layout(title = grTit2,
                        xaxis = x,
                        yaxis = y2,
                        legend = l,
                        hoverlabel = "right",
                     shapes = list(
               list(type = "rect",
                    fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                    x0 = "2001", x1 = "2002", xref = "x",
                    y0 = 0, y1 = maxFirms, yref = "y"),
               list(type = "rect",
                 fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                 x0 = "2008", x1 = "2010", xref = "x",
                 y0 = 0, y1 = maxFirms, yref = "y")))
  

  
  outList <- list("plot1" = jobs_plot, "data1" = jobs_data, "plot2"= firms_plot, "data2" = firms_data)
  return(outList)
}