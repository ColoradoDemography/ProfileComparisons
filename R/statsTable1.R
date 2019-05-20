#' statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API
#'
#' @param listID list containing id numbers and place names
#' @param sYr Start Year
#' @param eYr End year
#' @param ACS American Cummunity Survey Data series
#' @param oType Controls the rendering of the table, HTML or Latex
#' @return kable formatted table
#' @export

statsTable1 <- function(DBPool,lvl,listID,sYr,eYr,ACS){
  #outputs the top table in the dashboard

  state <- "08"
  jobsChk <- 0
  
  if(lvl == "Regional Summary") {
    ctyfips <- listID$ctyNum1
    
    f.tPopyr1r <- data.frame()
    f.tPopyr2r <- data.frame()
    f.jobsr <- data.frame()
    f.hhincr <- data.frame()
    f.medhhvaluer <- data.frame()
    f.povertyr <- data.frame()
    f.nativer <- data.frame()
    
    for(i in 1:length(ctyfips)){
      # Building up data files
      
      sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips[i])," and year = ", sYr," and placefips = 0;")
      sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips[i])," and year = ", eYr," and placefips = 0;")
      sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",as.numeric(ctyfips[i])," and population_year = ",eYr,
                           " and sector_id = '0';")
      f.tPopyr1 <-  dbGetQuery(DBPool, sqlStrPop1)
      f.tPopyr2 <-  dbGetQuery(DBPool, sqlStrPop2)
      f.Jobs <- dbGetQuery(DBPool, sqlStrJobs)
      

      hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      nativec <- codemog_api(data="b05002",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      
      f.tPopyr1r <- bind_rows(f.tPopyr1r,f.tPopyr1)
      f.tPopyr2r <- bind_rows(f.tPopyr2r,f.tPopyr2)
      f.jobsr <- bind_rows(f.jobsr,f.Jobs)
      f.hhincr <- bind_rows(f.hhincr,hhincc)
      f.medhhvaluer <- bind_rows(f.medhhvaluer,MedHHValuec)
      f.povertyr <- bind_rows(f.povertyr,povertyc)
      f.nativer <- bind_rows(f.nativer,nativec)
    }

    #Creating summary records at the region level
    # Population
    f.tpop1r <- f.tPopyr1r %>% summarize(tpopyr1 = sum(totalpopulation))
    f.tpop2r <- f.tPopyr2r %>% summarize(tpopyr2 = sum(totalpopulation))
    f.tpopr <- bind_cols(f.tpop1r,f.tpop2r) %>% mutate(tpopchngr = tpopyr2 - tpopyr1)
    
    #Jobs
    f.jobssumr <- f.jobsr %>% summarize(total_jobs = sum(total_jobs))
    
    #Median HH income
    f.medhhinr <-  f.hhincr %>% summarize(medianinc = median(as.numeric(b19013001)))
                                         
    #Median House Value                                     
    f.medHHValr <- f.medhhvaluer %>% summarize(MedianHH = median(as.numeric(b25077001)))   
                                              
    # % in poverty
    f.povertypctr <- f.povertyr %>% summarize(npoverty = sum(as.numeric(b17001002)), tpop = sum(as.numeric(b17001001))) %>%
              mutate(povpct = percent((npoverty/tpop)*100))
    
    # % Born In colorado                                          
    f.nativepctr <- f.nativer %>% summarize(nnative = sum(as.numeric(b05002003)), tpop = sum(as.numeric(b05002001))) %>%
      mutate(nativepct = percent((nnative/tpop)*100))
    
    regionData <- c(f.tpopr$tpopyr2,f.tpopr$tpopchngr,f.jobssumr$total_jobs,f.medhhinr$medianinc,
                    f.medHHValr$MedianHH,f.povertypctr$povpct,f.nativepctr$nativepct)
    
    # creating County data files...
 
    #Total Population
    names(f.tPopyr1r)[4] <- "tpopyr1"
    names(f.tPopyr2r)[4] <- "tpopyr2"
    f.tpopC <- inner_join(f.tPopyr1r,f.tPopyr2r,by="countyfips") %>% mutate(tpopchngr = tpopyr2 - tpopyr1)
    f.tpopC <- f.tpopC[,c(1,2,7,8)]
    
    #Jobs
    f.jobssumC <- f.jobsr %>% mutate(total_jobs = as.numeric(total_jobs))
    names(f.jobssumC)[1] <- "countyfips"
    
    #Median HH income
    f.medhhinc <-  f.hhincr %>% mutate(medianinc = as.numeric(b19013001))
    f.medhhinc <- f.medhhinc[,c(3,9)] %>% mutate(countyfips = as.numeric(county))
    
    #Median House Value                                     
    f.medHHValc <- f.medhhvaluer %>% mutate(MedianHH = as.numeric(b25077001))   
    f.medHHValc <- f.medHHValc[,c(3,9)] %>% mutate(countyfips = as.numeric(county))
    
    # % in poverty
    f.povertypctc <- f.povertyr %>% mutate(npoverty = as.numeric(b17001002), 
                                          tpop = as.numeric(b17001001),
                                          povpct = percent((npoverty/tpop)*100))
    f.povertypctc <- f.povertypctc[,c(3,69)] %>% mutate(countyfips = as.numeric(county))
    
    # % Born In colorado                                          
    f.nativepctc <- f.nativer %>% mutate(nnative = as.numeric(b05002003), 
                                         tpop = as.numeric(b05002001),
                                         nativepct = percent((nnative/tpop)*100))
    f.nativepctc <- f.nativepctc[,c(3,37)] %>% mutate(countyfips = as.numeric(county))
    
    countyData <- left_join(f.tpopC,  f.jobssumC, by='countyfips') %>%
      left_join(., f.medhhinc, by='countyfips') %>%
      left_join(., f.medHHValc, by='countyfips') %>%
      left_join(., f.povertypctc, by='countyfips') %>%
      left_join(., f.nativepctc, by= 'countyfips')
    countyData <- countyData[,c(1:4,6,8,10,12,14)] 
    names(countyData)[2] <- "geoname"
    countyData$geoname <- paste0(countyData$geoname," County")
    
    #state Values
    #Population
    
    sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = 0 and year = ", sYr," and placefips = 0;")
    sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = 0 and year = ", eYr," and placefips = 0;")
    sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = 0 and population_year = ",eYr,
                         " and sector_id = '0';")
    f.tPopyr1s <-  dbGetQuery(DBPool, sqlStrPop1)
    f.tPopyr2s <-  dbGetQuery(DBPool, sqlStrPop2)
    f.Jobss <- dbGetQuery(DBPool, sqlStrJobs)
    
    
    hhincs <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", state), meta="no")
    MedHHValues <- codemog_api(data="b25077",db=ACS, geonum=paste0("1", state), meta="no")
    povertys <- codemog_api(data="b17001",db=ACS, geonum=paste0("1", state), meta="no")
    natives <- codemog_api(data="b05002",db=ACS, geonum=paste0("1", state), meta="no")
    
    
    tpop1ST <- f.tPopyr1s$totalpopulation
    
    tpop2ST <- f.tPopyr2s$totalpopulation
    
    tpopchngST <- tpop2ST - tpop1ST
    
    #Jobs
    jobsValST <- f.Jobss$total_jobs
    
    medhhincST <- hhincs$b19013001

    medhhvalST <-  MedHHValues$b25077001
    
    povertyST <- percent(as.numeric(povertys$b17001002)/as.numeric(povertys$b17001001)*100)
    
    nativeST <- percent(as.numeric(natives$b05002003)/as.numeric(natives$b05002001)*100)
    
    
    stateData <- c(tpop2ST,tpopchngST,jobsValST,medhhincST,medhhvalST,povertyST,nativeST)
  }

 
  #Region to County
  if(lvl == "Region to County") {
   
    #Building Region Data
    ctyfips <- listID$ctyNum1
    
    
    f.tPopyr1r <- data.frame()
    f.tPopyr2r <- data.frame()
    f.jobsr <- data.frame()
    f.hhincr <- data.frame()
    f.medhhvaluer <- data.frame()
    f.povertyr <- data.frame()
    f.nativer <- data.frame()
    
    for(i in 1:length(ctyfips)){
      # Building up data files
      
      sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips[i])," and year = ", sYr," and placefips = 0;")
      sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips[i])," and year = ", eYr," and placefips = 0;")
      sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",as.numeric(ctyfips[i])," and population_year = ",eYr,
                           " and sector_id = '0';")
      f.tPopyr1 <-  dbGetQuery(DBPool, sqlStrPop1)
      f.tPopyr2 <-  dbGetQuery(DBPool, sqlStrPop2)
      f.Jobs <- dbGetQuery(DBPool, sqlStrJobs)
      
      
      hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      nativec <- codemog_api(data="b05002",db=ACS, geonum=paste0("1", state, ctyfips[i]), meta="no")
      
      f.tPopyr1r <- bind_rows(f.tPopyr1r,f.tPopyr1)
      f.tPopyr2r <- bind_rows(f.tPopyr2r,f.tPopyr2)
      f.jobsr <- bind_rows(f.jobsr,f.Jobs)
      f.hhincr <- bind_rows(f.hhincr,hhincc)
      f.medhhvaluer <- bind_rows(f.medhhvaluer,MedHHValuec)
      f.povertyr <- bind_rows(f.povertyr,povertyc)
      f.nativer <- bind_rows(f.nativer,nativec)
    }
    #Creating summary records
    # Population
    f.tpop1r <- f.tPopyr1r %>% summarize(tpopyr1 = sum(totalpopulation))
    f.tpop2r <- f.tPopyr2r %>% summarize(tpopyr2 = sum(totalpopulation))
    f.tpop <- bind_cols(f.tpop1r,f.tpop2r) %>% mutate(tpopchngr = tpopyr2 - tpopyr1)
    
    #Jobs
    f.jobssum <- f.jobsr %>% summarize(total_jobs = sum(total_jobs))
    
    #Median HH income
    f.medhhinc <-  f.hhincr %>% summarize(medianinc = median(as.numeric(b19013001)))
    
    #Median House Value                                     
    f.medHHVal <- f.medhhvaluer %>% summarize(MedianHH = median(as.numeric(b25077001)))   
    
    # % in poverty
    f.povertypct <- f.povertyr %>% summarize(npoverty = sum(as.numeric(b17001002)), tpop = sum(as.numeric(b17001001))) %>%
      mutate(povpct = percent((npoverty/tpop)*100))
    
    # % Born In colorado                                          
    f.nativepct <- f.nativer %>% summarize(nnative = sum(as.numeric(b05002003)), tpop = sum(as.numeric(b05002001))) %>%
      mutate(nativepct = percent((nnative/tpop)*100))
    
    regionData <- c(f.tpop$tpopyr2,f.tpop$tpopchngr,f.jobssum$total_jobs,f.medhhinc$medianinc,
                    f.medHHVal$MedianHH,f.povertypct$povpct,f.nativepct$nativepct)
    
    #Generating County Data
    ctyfips2 <- listID$ctyNum2
    f.tPopyr1c <- data.frame()
    f.tPopyr2c <- data.frame()
    f.jobsc <- data.frame()
    f.hhincc <- data.frame()
    f.medhhvaluec <- data.frame()
    f.povertyc <- data.frame()
    f.nativec <- data.frame()
    
    for(i in 1:length(ctyfips2)){
      # Building up data files
      
      sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips2[i])," and year = ", sYr," and placefips = 0;")
      sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips2[i])," and year = ", eYr," and placefips = 0;")
      sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",as.numeric(ctyfips2[i])," and population_year = ",eYr,
                           " and sector_id = '0';")
      f.tPopyr1 <-  dbGetQuery(DBPool, sqlStrPop1)
      f.tPopyr2 <-  dbGetQuery(DBPool, sqlStrPop2)
      f.Jobs <- dbGetQuery(DBPool, sqlStrJobs)
      
      
      hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      nativec <- codemog_api(data="b05002",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      
      f.tPopyr1c <- bind_rows(f.tPopyr1c,f.tPopyr1)
      f.tPopyr2c <- bind_rows(f.tPopyr2c,f.tPopyr2)
      f.jobsc <- bind_rows(f.jobsc,f.Jobs)
      f.hhincc <- bind_rows(f.hhincc,hhincc)
      f.medhhvaluec <- bind_rows(f.medhhvaluec,MedHHValuec)
      f.povertyc <- bind_rows(f.povertyc,povertyc)
      f.nativec <- bind_rows(f.nativec,nativec)
    }
    # Calculating  Summary Values
    #Total Population
    names(f.tPopyr1c)[4] <- "tpopyr1"
    names(f.tPopyr2c)[4] <- "tpopyr2"
    f.tpopC <- inner_join(f.tPopyr1c,f.tPopyr2c,by="countyfips") %>% mutate(tpopchngr = tpopyr2 - tpopyr1)
    f.tpopC <- f.tpopC[,c(1,7,8)]
    
    #Jobs
    f.jobssumC <- f.jobsc %>% mutate(total_jobs = as.numeric(total_jobs))
    names(f.jobssumC)[1] <- "countyfips"
    
    #Median HH income
    f.medhhinc <-  f.hhincc %>% mutate(medianinc = as.numeric(b19013001))
    f.medhhinc <- f.medhhinc[,c(3,9)] %>% mutate(countyfips = as.numeric(county))
    
    #Median House Value                                     
    f.medHHValc <- f.medhhvaluec %>% mutate(MedianHH = as.numeric(b25077001))   
    f.medHHValc <- f.medHHValc[,c(3,9)] %>% mutate(countyfips = as.numeric(county))
    
    # % in poverty
    f.povertypctc <- f.povertyc %>% mutate(npoverty = as.numeric(b17001002), 
                                          tpop = as.numeric(b17001001),
                                          povpct = percent((npoverty/tpop)*100))
    f.povertypctc <- f.povertypctc[,c(3,69)] %>% mutate(countyfips = as.numeric(county))
    
    # % Born In colorado                                          
    f.nativepctc <- f.nativec %>% mutate(nnative = as.numeric(b05002003), 
                                         tpop = as.numeric(b05002001),
                                         nativepct = percent((nnative/tpop)*100))
    f.nativepctc <- f.nativepctc[,c(3,37)] %>% mutate(countyfips = as.numeric(county))
  
   countyData <- left_join(f.tpopC,  f.jobssumC, by='countyfips') %>%
                 left_join(., f.medhhinc, by='countyfips') %>%
                 left_join(., f.medHHValc, by='countyfips') %>%
                 left_join(., f.povertypctc, by='countyfips') %>%
                 left_join(., f.nativepctc, by= 'countyfips')
   countyData <- countyData[,c(1:3,5,7,9,11,13)] 
  }

  
  if(lvl == "County to County") {
    #Generating County Data
    ctyfips2 <- unique(c(listID$ctyNum2,listID$ctyNum1))
    f.tPopyr1c <- data.frame()
    f.tPopyr2c <- data.frame()
    f.jobsc <- data.frame()
    f.hhincc <- data.frame()
    f.medhhvaluec <- data.frame()
    f.povertyc <- data.frame()
    f.nativec <- data.frame()
    
    for(i in 1:length(ctyfips2)){
      # Building up data files
      
      sqlStrPop1 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips2[i])," and year = ", sYr," and placefips = 0;")
      sqlStrPop2 <- paste0("SELECT countyfips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",as.numeric(ctyfips2[i])," and year = ", eYr," and placefips = 0;")
      sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",as.numeric(ctyfips2[i])," and population_year = ",eYr,
                           " and sector_id = '0';")
      f.tPopyr1 <-  dbGetQuery(DBPool, sqlStrPop1)
      f.tPopyr2 <-  dbGetQuery(DBPool, sqlStrPop2)
      f.Jobs <- dbGetQuery(DBPool, sqlStrJobs)
      
      
      hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      nativec <- codemog_api(data="b05002",db=ACS, geonum=paste0("1", state, ctyfips2[i]), meta="no")
      
      f.tPopyr1c <- bind_rows(f.tPopyr1c,f.tPopyr1)
      f.tPopyr2c <- bind_rows(f.tPopyr2c,f.tPopyr2)
      f.jobsc <- bind_rows(f.jobsc,f.Jobs)
      f.hhincc <- bind_rows(f.hhincc,hhincc)
      f.medhhvaluec <- bind_rows(f.medhhvaluec,MedHHValuec)
      f.povertyc <- bind_rows(f.povertyc,povertyc)
      f.nativec <- bind_rows(f.nativec,nativec)
    }
    # Calculating  Summary Values
    #Total Population
    names(f.tPopyr1c)[4] <- "tpopyr1"
    names(f.tPopyr2c)[4] <- "tpopyr2"
    f.tpopC <- inner_join(f.tPopyr1c,f.tPopyr2c,by="countyfips") %>% mutate(tpopchngr = tpopyr2 - tpopyr1)
    f.tpopC <- f.tpopC[,c(1,7,8)]
    
    #Jobs
    f.jobssumC <- f.jobsc %>% mutate(total_jobs = as.numeric(total_jobs))
    names(f.jobssumC)[1] <- "countyfips"
    
    #Median HH income
    f.medhhinc <-  f.hhincc %>% mutate(medianinc = as.numeric(b19013001))
    f.medhhinc <- f.medhhinc[,c(3,9)] %>% mutate(countyfips = as.numeric(county))
    
    #Median House Value                                     
    f.medHHValc <- f.medhhvaluec %>% mutate(MedianHH = as.numeric(b25077001))   
    f.medHHValc <- f.medHHValc[,c(3,9)] %>% mutate(countyfips = as.numeric(county))
    
    # % in poverty
    f.povertypctc <- f.povertyc %>% mutate(npoverty = as.numeric(b17001002), 
                                          tpop = as.numeric(b17001001),
                                          povpct = percent((npoverty/tpop)*100))
    f.povertypctc <- f.povertypctc[,c(3,69)] %>% mutate(countyfips = as.numeric(county))
    
    # % Born In colorado                                          
    f.nativepctc <- f.nativec %>% mutate(nnative = as.numeric(b05002003), 
                                         tpop = as.numeric(b05002001),
                                         nativepct = percent((nnative/tpop)*100))
    f.nativepctc <- f.nativepctc[,c(3,37)] %>% mutate(countyfips = as.numeric(county))
    
    countyData <- left_join(f.tpopC,  f.jobssumC, by='countyfips') %>%
      left_join(., f.medhhinc, by='countyfips') %>%
      left_join(., f.medHHValc, by='countyfips') %>%
      left_join(., f.povertypctc, by='countyfips') %>%
      left_join(., f.nativepctc, by= 'countyfips')
    countyData <- countyData[,c(1:3,5,7,9,11,13)] 
  }
  
  if(lvl == "Municipality to Municipality") {   
    plfips2 <- unique(c(listID$plNum2,listID$plNum1))
    f.tPopyr1m <- data.frame()
    f.tPopyr2m <- data.frame()
    f.jobsm <- data.frame()
    f.hhincm <- data.frame()
    f.medhhvaluem <- data.frame()
    f.povertym <- data.frame()
    f.nativem <- data.frame()
    
    for(i in 1:length(plfips2)){
      # Building up data files
      muninum <- as.numeric(substr(plfips2[i],3,nchar(plfips2[i])))
      
      sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE year = ", sYr," and placefips = ", muninum,";")
      sqlStrPop2 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE year = ", eYr," and placefips = ", muninum,";")
      sqlStrJobs <- paste0("SELECT ctyfips, placefips, geoname, year, jobs FROM estimates.muni_jobs_long WHERE placefips = ",muninum," and year = ", eYr, ";")
      
      f.tPopyr1 <-  dbGetQuery(DBPool, sqlStrPop1)
      f.tPopyr2 <-  dbGetQuery(DBPool, sqlStrPop2)
      f.Jobs <- dbGetQuery(DBPool, sqlStrJobs)
      
      # Correcting for multiple-counties Municipalities
      if(nrow(f.tPopyr1) > 1) {
        f.tPopyr1 <- f.tPopyr1[which(f.tPopyr1$countyfips == 999),]
      }
      if(nrow(f.tPopyr2) > 1) {
        f.tPopyr2 <- f.tPopyr2[which(f.tPopyr2$countyfips == 999),]
      }
      
      hhincm <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", plfips2[i]), meta="no")
      MedHHValuem <- codemog_api(data="b25077",db=ACS, geonum=paste0("1", plfips2[i]), meta="no")
      povertym <- codemog_api(data="b17001",db=ACS, geonum=paste0("1",  plfips2[i]), meta="no")
      nativem <- codemog_api(data="b05002",db=ACS, geonum=paste0("1", plfips2[i]), meta="no")
      
      f.tPopyr1m <- bind_rows(f.tPopyr1m,f.tPopyr1)
      f.tPopyr2m <- bind_rows(f.tPopyr2m,f.tPopyr2)
      f.jobsm <- bind_rows(f.jobsm,f.Jobs)
      f.hhincm <- bind_rows(f.hhincm,hhincm)
      f.medhhvaluem <- bind_rows(f.medhhvaluem,MedHHValuem)
      f.povertym <- bind_rows(f.povertym,povertym)
      f.nativem <- bind_rows(f.nativem,nativem)
    }
    # Calculating  Summary Values
    #Total Population
    
    names(f.tPopyr1m)[5] <- "tpopyr1"
    names(f.tPopyr2m)[5] <- "tpopyr2"
    f.tpopm <- inner_join(f.tPopyr1m,f.tPopyr2m,by="placefips") %>% mutate(tpopchngr = tpopyr2 - tpopyr1)
    f.tpopm <- f.tpopm[,c(2,3.5,9,10)]
    
    #Jobs  No Changes
   
    #Median HH income
    f.medhhinm <-  f.hhincm %>% mutate(medianinc = as.numeric(b19013001),
                                       placefips = as.numeric(place))
    f.medhhinm <- f.medhhinm[,c(10,1,9)] 
    
    #Median House Value                                     
    f.medHHValm <- f.medhhvaluem %>% mutate(MedianHH = as.numeric(b25077001),
                                            placefips = as.numeric(place))   
    f.medHHValm <- f.medHHValm[,c(10,1,9)] 
    
    # % in poverty
    f.povertypctm <- f.povertym %>% mutate(npoverty = as.numeric(b17001002), 
                                          tpop = as.numeric(b17001001),
                                          povpct = percent((npoverty/tpop)*100),
                                          placefips = as.numeric(place))
    f.povertypctm <- f.povertypctm[,c(70,1,69)] 
    
    # % Born In colorado                                          
    f.nativepctm <- f.nativem %>% mutate(nnative = as.numeric(b05002003), 
                                         tpop = as.numeric(b05002001),
                                         nativepct = percent((nnative/tpop)*100),
                                         placefips = as.numeric(place))
    f.nativepctm <- f.nativepctm[,c(38,1,37)] 
    
    muniData <- left_join(f.tpopm,  f.jobsm, by='placefips') %>%
      left_join(., f.medhhinm, by='placefips') %>%
      left_join(., f.medHHValm, by='placefips') %>%
      left_join(., f.povertypctm, by='placefips') %>%
      left_join(., f.nativepctm, by= 'placefips')
    muniData <- muniData[,c(1:4,8,10,12,14,16)] 
  }
  

  #Preparing table

 if(lvl == "Regional Summary"){
   ctyName2 <- c(countyData$geoname)
   names_spaced <- c(" ",unique(c(ctyName2,listID$ctyName1,"Colorado")))
    Ncols <- nrow(countyData) 
    outTab <- matrix(" ",nrow=7,ncol=Ncols+3)
    
    for(i in 1:Ncols){
      #County
      outTab[1,i+1] <- format(as.numeric(countyData[i,3]),nsmall=0, big.mark=",")
      outTab[2,i+1] <- format(as.numeric(countyData[i,4]),nsmall=0, big.mark=",")
      outTab[3,i+1] <- format(round(as.numeric(countyData[i,5]),digits=0),nsmall=0, big.mark=",")
      outTab[4,i+1] <- paste0("$",format(round(as.numeric(countyData[i,6]),digits=0),nsmall=0, big.mark=","))
      outTab[5,i+1] <- paste0("$",format(round(as.numeric(countyData[i,7]),digits=0),nsmall=0, big.mark=","))
      outTab[6,i+1] <- countyData[i,8]
      outTab[7,i+1] <- countyData[i,9]
    }
    #Region
    outTab[1,Ncols+2] <- format(as.numeric(regionData[1]),nsmall=0, big.mark=",")
    outTab[2,Ncols+2] <- format(as.numeric(regionData[2]),nsmall=0, big.mark=",")
    outTab[3,Ncols+2] <- format(round(as.numeric(regionData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,Ncols+2] <- paste0("$",format(round(as.numeric(regionData[4]),digits=0),nsmall=0, big.mark=","))
    outTab[5,Ncols+2] <- paste0("$",format(round(as.numeric(regionData[5]),digits=0),nsmall=0, big.mark=","))
    outTab[6,Ncols+2] <- regionData[6]
    outTab[7,Ncols+2] <- regionData[7]
    
    
    #State
    outTab[1,Ncols+3] <- format(as.numeric(stateData[1]),nsmall=0, big.mark=",")
    outTab[2,Ncols+3] <- format(as.numeric(stateData[2]),nsmall=0, big.mark=",")
    outTab[3,Ncols+3] <- format(round(as.numeric(stateData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,Ncols+3] <- paste0("$",format(round(as.numeric(stateData[4]),digits=0),nsmall=0, big.mark=","))
    outTab[5,Ncols+3] <- paste0("$",format(round(as.numeric(stateData[5]),digits=0),nsmall=0, big.mark=","))
    outTab[6,Ncols+3] <- stateData[6]
    outTab[7,Ncols+3] <- stateData[7]
  }   
  
  if(lvl == "Region to County"){
    names_spaced <- c(" ",unique(c(listID$ctyName2,listID$ctyName1)))
    Ncols <- nrow(countyData) + 2
    outTab <- matrix(" ",nrow=7,ncol=Ncols)
    
    for(i in 1:nrow(countyData)){
      #County
      outTab[1,i+1] <- format(as.numeric(countyData[i,2]),nsmall=0, big.mark=",")
      outTab[2,i+1] <- format(as.numeric(countyData[i,3]),nsmall=0, big.mark=",")
      outTab[3,i+1] <- format(round(as.numeric(countyData[i,4]),digits=0),nsmall=0, big.mark=",")
      outTab[4,i+1] <- paste0("$",format(round(as.numeric(countyData[i,5]),digits=0),nsmall=0, big.mark=","))
      outTab[5,i+1] <- paste0("$",format(round(as.numeric(countyData[i,6]),digits=0),nsmall=0, big.mark=","))
      outTab[6,i+1] <- countyData[i,7]
      outTab[7,i+1] <- countyData[i,8]
    }

    #Region
    outTab[1,Ncols] <- format(as.numeric(regionData[1]),nsmall=0, big.mark=",")
    outTab[2,Ncols] <- format(as.numeric(regionData[2]),nsmall=0, big.mark=",")
    outTab[3,Ncols] <- format(round(as.numeric(regionData[3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,Ncols] <- paste0("$",format(round(as.numeric(regionData[4]),digits=0),nsmall=0, big.mark=","))
    outTab[5,Ncols] <- paste0("$",format(round(as.numeric(regionData[5]),digits=0),nsmall=0, big.mark=","))
    outTab[6,Ncols] <- regionData[6]
    outTab[7,Ncols] <- regionData[7]
  } 
 

  if(lvl == "County to County") {
    names_spaced <- c(" ",unique(c(listID$ctyName2,listID$ctyName1)))
    outTab <- matrix(" ",nrow=7,ncol=nrow(countyData)+1)
    Ncols <- nrow(countyData) 
    for(i in 1:Ncols){
      #County
      outTab[1,i+1] <- format(as.numeric(countyData[i,2]),nsmall=0, big.mark=",")
      outTab[2,i+1] <- format(as.numeric(countyData[i,3]),nsmall=0, big.mark=",")
      outTab[3,i+1] <- format(round(as.numeric(countyData[i,4]),digits=0),nsmall=0, big.mark=",")
      outTab[4,i+1] <- paste0("$",format(round(as.numeric(countyData[i,5]),digits=0),nsmall=0, big.mark=","))
      outTab[5,i+1] <- paste0("$",format(round(as.numeric(countyData[i,6]),digits=0),nsmall=0, big.mark=","))
      outTab[6,i+1] <- countyData[i,7]
      outTab[7,i+1] <- countyData[i,8]
    }
  }
  if(lvl == "Municipality to Municipality") {
    names_spaced <- c(" ",unique(c(listID$plName2,listID$plName1)))
    outTab <- matrix(" ",nrow=7,ncol=nrow(muniData)+1)
    Ncols <- nrow(muniData)
    for(i in 1:Ncols){
      outTab[1,i+1] <- format(as.numeric(muniData[i,3]),nsmall=0, big.mark=",")
      outTab[2,i+1] <- format(as.numeric(muniData[i,4]),nsmall=0, big.mark=",")
      if(muniData[i,5] == -9) {
        outTab[3,i+1] <- " "
      } else {
        outTab[3,i+1] <- format(round(as.numeric(muniData[i,5]),digits=0),nsmall=0, big.mark=",")
      }
      outTab[4,i+1] <- paste0("$",format(round(as.numeric(muniData[i,6]),digits=0),nsmall=0, big.mark=","))
      outTab[5,i+1] <- paste0("$",format(round(as.numeric(muniData[i,7]),digits=0),nsmall=0, big.mark=","))
      outTab[6,i+1] <- muniData[i,8]
      outTab[7,i+1] <- muniData[i,9]
    }
  }
  
 #Row Labels 
  outTab[1,1] <- paste0("Population (",eYr,")",footnote_marker_symbol(1))
  outTab[2,1] <- paste0("Population Change (",sYr," to ",eYr, ")",footnote_marker_symbol(1))
  outTab[3,1] <- paste0("Total Employment (",eYr,")",footnote_marker_symbol(1))
  outTab[4,1] <- paste0("Median Household Income",footnote_marker_symbol(2))
  outTab[5,1] <- paste0("Median House Value",footnote_marker_symbol(2))
  outTab[6,1] <- paste0("Percentage of Population with Incomes lower than the Poverty Line",footnote_marker_symbol(2))
  outTab[7,1] <- paste0("Percentage of Population Born in Colorado",footnote_marker_symbol(2))
  
  

  if(lvl == "Regional Summary") {
    header_span <- c(" "= 1, "Counties" = Ncols, "Region" = 1,"State"=1)
    Nvars <- Ncols + 3
    align_spec <- c('l',rep('r',Nvars))
  } 
  
  if(lvl == "Region to County") {
    header_span <- c(" " = 1,"Comparison(s)" = Ncols-2,"Region" = 1)
    Nvars <- Ncols
    align_spec <- c('l',rep('r',Nvars))
  }

 if(lvl == "County to County") {
    header_span <- c(" " = 1,"Comparison(s)" = Ncols-1,"Reference Location" = 1)
    Nvars <- Ncols
    align_spec <- c('l',rep('r',Nvars))
  }
 
  if(lvl == "Municipality to Municipality") {
    header_span <- c(" " = 1,"Comparison(s)" = Ncols-1,"Reference Location" = 1)
    Nvars <- Ncols
    align_spec <- c('l',rep('r',Nvars))
  } 
  
 
  #Generating HTMl File, kable Table
  outHTML <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                    digits=1,
                    row.names=FALSE,
                    align=align_spec,
                    col.names = names_spaced,
                    caption="Community Quick Facts",
                    escape = FALSE)   %>%
    kable_styling() %>%
    column_spec(1, width = "4in") %>%
    add_header_above(header_span)  #%>%
   # footnote(symbol= c("Source: State Demography Office",captionSrc("ACS",ACS)), footnote_as_chunk = T) 

  #Generate Flextable
  outTab <- gsub("<sup>"," ",outTab)
  outTab <- gsub("</sup>","",outTab)
  outTab <- gsub("&dagger;"," ^",outTab)
  f.Flex <- as.data.frame(outTab)
  
  # Building Header_df Data Frame and Hd_ row

  for(i in 1:ncol(f.Flex)){
    if(i == 1) {
      V <- paste0("V",i)
      hd_row <- " "
    } else{
      V <- c(V,paste0("V",i))
      hd_row <- c(hd_row," ")
    }
  }

  
  
  if(lvl == "Regional Summary") {
    
    pos1 <- round(ncol(f.Flex)/2,digits=0)
    pos2 <- ncol(f.Flex) - 1
    pos3 <- ncol(f.Flex)
    hd_row[pos1] <- "Comparison(s)"
    hd_row[pos2] <- "Region"
    hd_row[pos3] <- "State"
  }
  if(lvl == "Region to County") {
    pos1 <- round(ncol(f.Flex)/2,digits=0) + 1
    pos2 <- ncol(f.Flex)
    hd_row[pos1] <- "Comparison(s)"
    hd_row[pos2] <- "Region"
  }
  
  if(lvl == "County to County") {
    pos1 <- round(ncol(f.Flex)/2,digits=0) + 1
    pos2 <- ncol(f.Flex)
    hd_row[pos1] <- "Comparison Location(s)"
    hd_row[pos2] <- "Reference Location"
  }
  
  if(lvl == "Municipality to Municipality") {
    pos1 <- round(ncol(f.Flex)/2,digits=0) + 1
    pos2 <- ncol(f.Flex)
    hd_row[pos1] <- "Comparison Location(s)"
    hd_row[pos2] <- "Reference Location"
  }
 
  headr_tab <- data.frame(
    col_keys = V,
    what = hd_row,
    measure = names_spaced,
    stringsAsFactors = FALSE
  )

  FlexOut <- flextable(f.Flex) %>%  
             set_header_df(mapping = headr_tab, key = "col_keys" ) %>%
             add_footer_row(values=paste0("^",captionSrc("ACS",ACS)),  colwidths = ncol(f.Flex)) %>%
             add_footer_row(, values="* State Demography Office",  colwidths = ncol(f.Flex)) %>%
             autofit() %>%
             align(i=1:2, align="center", part="header") %>%
             align(j=1,align="left", part="body")
  
 
 #Row Labels for Latex table
 outTab[1,1] <- paste0("Population (",eYr,")",footnote_marker_symbol(1,"latex"))
 outTab[2,1] <- paste0("Population Change (",sYr," to ",eYr, ")",footnote_marker_symbol(1,"latex"))
 outTab[3,1] <- paste0("Total Employment (",eYr,")",footnote_marker_symbol(1,"latex"))
 outTab[4,1] <- paste0("Median Household Income",footnote_marker_symbol(2,"latex"))
 outTab[5,1] <- paste0("Median House Value",footnote_marker_symbol(2,"latex"))
 outTab[6,1] <- paste0("Percentage of Population with Incomes lower than the Poverty Line",footnote_marker_symbol(2,"latex"))
 outTab[7,1] <- paste0("Percentage of Population Born in Colorado",footnote_marker_symbol(2,"latex"))
 
  

  names(f.Flex) <- names_spaced
      
  
  outList <- list("Htable" = outHTML, "FlexTable" = FlexOut, "data"=f.Flex)
  return(outList)

}

