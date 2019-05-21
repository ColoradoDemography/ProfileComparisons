#' CountyRank Creates County Ranking Tables and D3 Circle Packing diagram 5/2019
#'
#' @param DBPool Database pool
#' @param CtyList List of Counties
#' @param eYr end Year (Current Year)
#' @param chkList list of topical items selected
#' @param ACS American Cummunity Survey Data series
#' @return DT table  showing ranking
#' @export


CountyRank <- function(DBPool, CtyList, chkList, eYr, ACS) {
  withProgress(message = 'Generating County Ranking', value = 0, {  # Initialize Progress bar
  sYr <- 2010
  state = "08"
  allCty <- CtyList$countyfips
  allCty0 <- formatC(allCty, width = 3, format = "d", flag = "0")
  
  f.ctyfull <- CtyList[,c(1,3)]
  colName2 <- c("County" = "name")
  #Total Population
  if("totpop" %in% chkList){
    f.ctypop <- CtyList[,c(1,5)]
    
    f.ctyfull <- inner_join(f.ctyfull,f.ctypop,by="countyfips")
    popnames <- c("Total Population" = "totalpopulation")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  # Population Growth Rate
  if("popgr" %in% chkList) {
    yrs <- as.character(c(sYr,eYr))
    f.popGr <- data.frame()
    for(i in 1:length(allCty)) {
      ctyPopGrowth <- paste0("SELECT countyfips, year, totalpopulation FROM estimates.county_muni_timeseries WHERE countyfips = ",allCty[i]," AND placefips = 0;") 
      mCty <- dbGetQuery(DBPool,ctyPopGrowth) %>%
        filter(year %in% yrs)
      f.popGr <- bind_rows(f.popGr,mCty)
    }
    
    f.ctyGR <- f.popGr %>%   
      arrange(countyfips,year)%>%
      group_by(countyfips) %>%
      mutate(year=as.numeric(year),
             totalpopulation=as.numeric(totalpopulation),
             growthRate=signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100))
    f.ctyGR <-  f.ctyGR[which( f.ctyGR$year == eYr),]  
    f.ctyfull <- inner_join(f.ctyfull,f.ctyGR[,c(1,4)],by="countyfips")
    
    popnames <- c("Average Population Growth Rate" = "growthRate")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  
  # Calculation of age variables
  f.agesya <- county_sya(allCty, eYr)
  f.agesya$age2564 <- ifelse(f.agesya$age >= 25 & f.agesya$age < 65,1,0)
  f.agesya$age65 <- ifelse(f.agesya$age >= 65,1,0)
  f.agesya$totalpopulation <- as.numeric(f.agesya$totalpopulation)
  
  f.ageTot <- f.agesya %>%
    group_by(countyfips) %>%
    summarize(tpop = sum(totalpopulation))
  
  if("pop2564" %in% chkList) {
    f.age2564 <- f.agesya %>%
      group_by(countyfips, age2564) %>%
      summarize(tpop2564 = sum(totalpopulation))
    f.age2564 <- f.age2564[which(f.age2564$age2564 == 1),]
    
    f.age2564pct <- inner_join(f.age2564, f.ageTot,by="countyfips")
    f.age2564pct$tpop2564pct <- f.age2564pct$tpop2564/f.age2564pct$tpop
    
    f.ctyfull <- inner_join(f.ctyfull,f.age2564pct[,c(1,5)], by="countyfips")
    
    popnames <- c("Percent of Population Age 25 to 64" = "tpop2564pct")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  if("pop65" %in% chkList) {
    f.age65 <- f.agesya %>%
      group_by(countyfips, age65) %>%
      summarize(tpop65 = sum(totalpopulation))
    f.age65 <- f.age65[which(f.age65$age65 == 1),]
    
    f.age65pct <- inner_join(f.age65, f.ageTot,by="countyfips")
    f.age65pct$tpop65pct <- f.age65pct$tpop65/f.age65pct$tpop
    
    
    f.ctyfull <- inner_join(f.ctyfull,f.age65pct[,c(1,5)], by="countyfips")
    popnames <- c("Percent of Population Age 65 and Older" = "tpop65pct")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  #percent non-white
  
  if("pctNW" %in% chkList) {
    f.nonwhite <- data.frame()
    for(i in 1:length(allCty0)) {
      ACSRaceCTY <- codemog_api(data="b03002", db=ACS, geonum=paste0("1", state, allCty0[i]),meta="no")
      f.nonwhite <- bind_rows(f.nonwhite,ACSRaceCTY)
    }
    f.nonwhite[,c(3,8:28)] <-  lapply(f.nonwhite[,c(3,8:28)], function(x) as.numeric(as.character(x)))
    names(f.nonwhite)[3] <- "countyfips"
    
    f.nonwhite <- f.nonwhite %>%
      select(geoname:b03002012) %>%
      mutate(TotalPop=b03002001,
             NHWhite=b03002003,
             pctNonWhite = (TotalPop - NHWhite)/TotalPop)
    
    f.nonwhite <- f.nonwhite[,c(3,22)]
    f.ctyfull <- inner_join(f.ctyfull, f.nonwhite, by="countyfips")
    popnames <- c("Percent of Non-White Persons" = "pctNonWhite")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  # Number of people with a bachelor's degree or higher
  #county Education Value
  if("educ" %in% chkList) {
    d13cty <- data.frame()
    for(i in 1: length(allCty0)) {
      x <- codemog_api(data="b15003",db=ACS,geonum=paste("1", state , allCty0[i], sep=""),meta="no")
      d13cty <- bind_rows(d13cty,x)
    }
    
    d13cty[,c(3,8:32)] <-  lapply(d13cty[,c(3,8:32)], function(x) as.numeric(as.character(x)))
    names(d13cty)[3] <- "countyfips"
    
    f.educVal <- d13cty %>% group_by(countyfips) %>%
      summarise(total = b15003001+b15003002+b15003003 + b15003004 + b15003005+b15003006 +
                  b15003007+b15003008+b15003009 + b15003010 + b15003023+b15003011 +
                  b15003012+b15003013+b15003014 + b15003015 + b15003016+b15003017 + b15003018 +
                  b15003019+b15003020+b15003021 + b15003022 + b15003023+b15003024+b15003025,
                baplus = b15003019+b15003020+b15003021 + b15003022 + b15003023+b15003024+b15003025) %>%
      mutate(bapct = baplus/total)
    
    f.ctyfull <- inner_join(f.ctyfull, f.educVal[,c(1,4)], by="countyfips")
    popnames <- c("Percentage of Persons with a Bachelor's Degree or Higher" = "bapct")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  # Total Estimated Jobs
  if("jobs" %in% chkList) {
    f.ctyJobs <- data.frame()
    for(i in 1: length(allCty)) {
      sqlStrJobs <- paste0("SELECT area_code, population_year, total_jobs FROM estimates.jobs_by_sector WHERE area_code = ",allCty[i]," and population_year = ",eYr,
                           " and sector_id = '0';")
      f.Jobs <- dbGetQuery(DBPool, sqlStrJobs)
      f.ctyJobs <- bind_rows(f.ctyJobs,f.Jobs)
    }
    names(f.ctyJobs)[1] <- "countyfips" 
    
    f.ctyfull <- inner_join(f.ctyfull, f.ctyJobs[,c(1,3)], by="countyfips")
    popnames <- c("Total Estimated Jobs"= "total_jobs")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  # Median Household Income
  if("medinc" %in% chkList) {
    f.ctymedinc <- data.frame()
    for(i in 1: length(allCty0)) { 
      hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste0("1", state, allCty0[i]), meta="no")
      f.ctymedinc <- bind_rows(f.ctymedinc,hhincc)
    }
    f.ctymedinc <-   f.ctymedinc %>% mutate(medianinc = as.numeric(b19013001))
    names(f.ctymedinc)[3] <- "countyfips"
    f.ctymedinc$countyfips <- as.numeric(f.ctymedinc$countyfips)
    
    f.ctyfull <- inner_join(f.ctyfull,f.ctymedinc[,c(3,9)],by="countyfips")
    popnames <- c("Median Household Income"= "medianinc")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  # % in poverty
  if("poverty" %in% chkList) {
    f.povertyr <- data.frame()
    for(i in 1:length(allCty0)) {
      povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste0("1", state, allCty0[i]), meta="no")
      f.povertyr <- bind_rows(f.povertyr,povertyc)
    }
    names(f.povertyr)[3]  <- "countyfips"
    f.povertyr$countyfips <- as.numeric(f.povertyr$countyfips)
    
    f.povertypct <- f.povertyr %>% 
      group_by(countyfips) %>%
      summarize(npoverty = sum(as.numeric(b17001002)), tpop = sum(as.numeric(b17001001))) %>%
      mutate(povpct = (npoverty/tpop)) 
    
    f.ctyfull <- inner_join(f.ctyfull,f.povertypct[,c(1,4)],by="countyfips")
    popnames <- c("Percent of Persons Below the Poverty Line"= "povpct")
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  
  #Standardizing and Formatting
  f.ctyfull$aggVal <- 0  
  if("totpop" %in% chkList) {
    f.ctyfull$popZ <- scale(f.ctyfull$totalpopulation)
    f.ctyfull$totalpopulation <- formatC(f.ctyfull$totalpopulation,digits=0,format="d", big.mark=",")
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$popZ
  }
  if("popgr" %in% chkList) {
    f.ctyfull$grZ <- scale(f.ctyfull$growthRate) 
    f.ctyfull$growthRate <- percent(f.ctyfull$growthRate)
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$grZ
  }
  
  if("pop2564" %in% chkList) {
    f.ctyfull$tpop2564pctZ <- scale(f.ctyfull$tpop2564pct)
    f.ctyfull$tpop2564pct <- percent(f.ctyfull$tpop2564pct*100)
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$tpop2564pctZ
  }
  
  if("pop65" %in% chkList) {
    f.ctyfull$tpop65pctZ <- scale(f.ctyfull$tpop65pct)
    f.ctyfull$tpop65pct <- percent(f.ctyfull$tpop65pct*100)
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$tpop65pctZ
  }  
  
  if("pctNW" %in% chkList) {
    f.ctyfull$nonwZ <- scale(f.ctyfull$pctNonWhite)
    f.ctyfull$pctNonWhite <- percent(f.ctyfull$pctNonWhite* 100)
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$nonwZ
  }  
  
  if("educ" %in% chkList) {
    f.ctyfull$bapctZ <- scale(f.ctyfull$bapct)
    f.ctyfull$bapct<- percent( f.ctyfull$bapct * 100)
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$bapctZ
  }  
  
  if("jobs" %in% chkList)  {
    f.ctyfull$jobsZ <- scale(f.ctyfull$total_jobs)
    f.ctyfull$total_jobs <- formatC(f.ctyfull$total_jobs,digits=0,format="d", big.mark=",")
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$jobsZ
  }
  
  
  if("medinc" %in% chkList) {
    f.ctyfull$incZ <- scale(f.ctyfull$medianinc)
    f.ctyfull$medianinc <- paste0("$",formatC(f.ctyfull$medianinc,digits=0,format="d", big.mark=","))
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$incZ
  }  
  
  if("poverty" %in% chkList) {
    f.ctyfull$povpctZ <- scale(f.ctyfull$povpct)
    f.ctyfull$povpct <- percent(f.ctyfull$povpct*100)
    f.ctyfull$aggVal <- f.ctyfull$aggVal + f.ctyfull$povpctZ
  }  
  f.ctyfull$aggRank <- 65 - rank(f.ctyfull$aggVal)  
  popnames <- c("Aggregate Rank"= "aggRank")
  colName2 <- c(colName2,popnames)
  
  # extracting AggVal
  f.ctyAggVal <- f.ctyfull[,c("municipalityname","aggVal")]
  names(f.ctyAggVal) <- c("name","r")
  f.ctyAggVal$r <- round(abs(f.ctyAggVal$r),digits=2) + 1
  incProgress()
  
  # Locate column numbers
  aggN <- which( colnames(f.ctyfull)=="aggVal" ) 
  aggR <- ncol(f.ctyfull)
  f.ctyTab <- f.ctyfull[,c(2:(aggN-1),aggR)]
  f.ctyTab <- f.ctyTab[order(f.ctyTab$aggRank),]
  f.ctyCluster <- f.ctyfull[,c(2,(aggN+1):(aggR-1))]
  
  
  # Generating Output  
  
  f.ctynames <- f.ctyCluster[,1]
  
  f.ctyClus2 <- f.ctyCluster[,c(2:ncol(f.ctyCluster))]
  
  rownames(f.ctyClus2) <- f.ctynames
  
  # performing the clustering
  # Distance Matrix
  ctyDist <- dist(f.ctyClus2)
  f.clusStr <- hclust(ctyDist,method="ward.D2")
  
  
  # Generating optimal clusters  based on PBC (Point by Serial Correlation)
  clusRange <- as.clustrange(f.clusStr,diss=ctyDist,ncluster=20)
  NClus <- as.data.frame(summary(clusRange, max.rank=2))
  
 
  fin_clus <- as.data.frame(cutree(f.clusStr,k=NClus[1,1]))
  cty_clus <- cbind(f.ctynames,fin_clus)
  rownames(cty_clus) <- c()
  names(cty_clus) <- c("name","cluster")
  cty_clus$name  <- as.character(cty_clus$name)
  
  
  f.ctyfinclus <- inner_join(cty_clus,f.ctyAggVal, by="name") 
  incProgress()

  #Outputting Circle Packing Diagram
  
  f.ctyfinclus$name <- gsub(" County","",f.ctyfinclus$name)
  
  
  # Output data tree
  f.ctyfinclus$clusterName <- paste0("Cluster ",f.ctyfinclus$cluster)
  
  f.ctyfinclus$pathString <- paste("state", f.ctyfinclus$clusterName, f.ctyfinclus$name, sep="/")
  f.tree <-as.Node(f.ctyfinclus)
  


 
  
# outputting DT ranking Table  

names(f.ctyTab)[1] <- "name"
f.ctyTab <- inner_join(cty_clus,f.ctyTab, by="name") 
f.ctyTab <- f.ctyTab[order(f.ctyTab$aggRank),]
popnames2 <- c("Cluster" = "cluster")
popnames1 <- colName2[1]
popnamesend <- colName2[2:length(colName2)]

colName2 <- c(popnames1,popnames2,popnamesend)


 browser()
names(f.ctyTab) <-  names(colName2)
f.ctyTab <- f.ctyTab[,c(1,2,ncol(f.ctyTab),3:(ncol(f.ctyTab)-1))]

OutObj <- list("outtree" = f.tree, "data" = f.ctyTab)
return(OutObj)
})
}
  