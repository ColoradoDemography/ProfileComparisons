#' MunicipalRank Creates Municipal Ranking Tables and D3 Circle Packing diagram 5/2019
#'
#' @param DBPool Database pool
#' @param MuniList List of Municipalities
#' @param eYr end Year (Current Year)
#' @param chkList list of topical items selected
#' @param ACS American Cummunity Survey Data series
#' @return DT table  showing ranking
#' @export


MunicipalRank <- function(DBPool, MuniList, chkList, eYr, ACS) {
  withProgress(message = 'Generating Municipal Ranking', value = 0, {  # Initialize Progress bar

  sYr <- 2010
  state = "08"
  # remove duplicates
  MuniListu <- MuniList[!duplicated(MuniList$placefips),]

  allmuni <- MuniListu$placefips
  allmuni0 <- paste0(state,formatC(allmuni, width = 5, format = "d", flag = "0"))  # This is the list of municipalities
 
  
  f.munifull <- MuniListu[,c(2,3)]
  names(f.munifull) <- c("GEOID","Municipality")
  f.munifull$GEOID <- paste0(state,formatC(f.munifull$GEOID, width = 5, format = "d", flag = "0"))
  
  colName2 <- c("Municipality" = "Municipality")
  #Total Population
  if("totpop" %in% chkList){
    f.munipop <- MuniListu[,c(2,5)]
    names(f.munipop)[1] <- "GEOID"
    f.munipop$GEOID <- paste0(state,formatC(f.munipop$GEOID, width = 5, format = "d", flag = "0"))
    
    
    f.munifull <- inner_join(f.munifull,f.munipop,by="GEOID")
    inName <- paste0("Total Population (",eYr,")")
    popnames <- setNames( c("totalpopulation"),as.list(inName))
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  # Population Growth Rate
  if("popgr" %in% chkList) {
    yrs <- as.character(c(sYr,eYr))

    muniPopGrowth <- "SELECT countyfips, placefips, year, totalpopulation FROM estimates.county_muni_timeseries WHERE (placefips != 0 and placefips != 99990);" 
    popdata <- dbGetQuery(DBPool,muniPopGrowth)

    
    popdata$totalpopulation <- ifelse(is.na(popdata$totalpopulation),0,popdata$totalpopulation)
    names(popdata)[2] <- "GEOID"
    popdata$GEOID <- paste0(state,formatC(popdata$GEOID, width = 5, format = "d", flag = "0"))
       
   f.popGR <- popdata %>% filter(year %in% yrs) %>%
     group_by(GEOID, year) %>%
     summarize(totalpopulation = sum(totalpopulation)) %>%
     arrange(GEOID,year)
     
        
   f.muniGR <- f.popGR %>%  
      group_by(GEOID) %>%
      mutate(growthRate=signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100))
   
    f.muniGR <-  f.muniGR[which( f.muniGR$year == eYr),]  
    f.munifull <- inner_join(f.munifull,f.muniGR[,c(1,4)],by="GEOID")
    inName <- paste0("Average Population Growth Rate (",eYr,")")
    popnames <- setNames( c("growthRate"),as.list(inName))
    
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  
  # Calculation of age variables
   f.placeAge <- codemog_api(data="b01001",db=ACS,sumlev="160",geography="sumlev",meta="no")
   
   # Conver to numeric
   f.placeAge[,8:ncol(f.placeAge)] <- sapply(f.placeAge[,8:ncol(f.placeAge)],as.numeric)
   # Change geonum to geoid
   names(f.placeAge)[7] <- "GEOID"
   f.placeAge$GEOID <- substr(f.placeAge$GEOID,2,nchar(f.placeAge$GEOID))
   
  
  f.place2 <- f.placeAge %>%
    filter(GEOID %in% allmuni0) %>%
    mutate(
      total = b01001001,
      tpop2564 = b01001011 + b01001012 + b01001013 + b01001014 + b01001015 + b01001016 + b01001017 + b01001018 + b01001019 +
              b01001035 + b01001036 + b01001037 + b01001038 + b01001039 + b01001040 + b01001041 + b01001042 + b01001043,
      tpop65 = b01001020 + b01001021 + b01001022 + b01001023 + b01001024 + b01001025 +
                 b01001044 + b01001045 + b01001046 + b01001047 + b01001048 + b01001049,
      tpop2564pct = tpop2564/total,
      tpop65pct =  tpop65/total) %>%
    select(GEOID,total,tpop2564, tpop2564pct,  tpop65,  tpop65pct)
 
  if("pop2564" %in% chkList) {
    f.munifull <- inner_join(f.munifull,f.place2[,c(1,4)], by="GEOID")
    inName <- paste0("Percent of Population Age 25 to 64 (",eYr,")")
    popnames <- setNames( c("tpop2564pct"),as.list(inName))

    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  if("pop65" %in% chkList) {
    f.munifull <- inner_join(f.munifull,f.place2[,c(1,6)], by="GEOID")
    inName <- paste0("Percent of Population Age 65 and Older (",eYr,")")
    popnames <- setNames( c("tpop65pct"),as.list(inName))

    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  #percent non-white

  if("pctNW" %in% chkList) {
    f.HISP <-codemog_api(data="b03002",db=ACS,sumlev="160",geography="sumlev",meta="no")
    # Conver to numeric
    f.HISP[,8:ncol(f.HISP)] <- sapply(f.HISP[,8:ncol(f.HISP)],as.numeric)
    # Change geonum to geoid
    names(f.HISP)[7] <- "GEOID"
    f.HISP$GEOID <- substr(f.HISP$GEOID,2,nchar(f.HISP$GEOID))
    
    f.HISP <- f.HISP %>%
      filter(GEOID %in% allmuni0) %>%
      mutate(TotalPop= b03002001,
             HISP= b03002012,
             pctHISP = HISP/TotalPop)
    
    
    f.munifull <- inner_join(f.munifull, f.HISP[,c(7,31)], by="GEOID")
    inName <- paste0("Percent of Hispanic Population (",eYr,")")
    popnames <- setNames( c("pctHISP"),as.list(inName))

    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  # Number of people with a bachelor's degree or higher
      if("educ" %in% chkList) {
      f.educ <- codemog_api(data="b15003",db=ACS,sumlev="160",geography="sumlev",meta="no")
      
      # Conver to numeric
      f.educ[,8:ncol(f.educ)] <- sapply(f.educ[,8:ncol(f.educ)],as.numeric)
      # Change geonum to geoid
      names(f.educ)[7] <- "GEOID"
      f.educ$GEOID <- substr(f.educ$GEOID,2,nchar(f.educ$GEOID))
      
      f.educVal <- f.educ %>% 
        filter(GEOID %in% allmuni0) %>%
        mutate(total = b15003001,
               baplus =  b15003022 + b15003023 + b15003024 + b15003025,
               bapct = baplus/total)
      
      f.munifull <- inner_join(f.munifull, f.educVal[,c(7,35)], by="GEOID")
      inName <- paste0("Percentage of Persons with a Bachelor's Degree or Higher (",eYr,")")
      popnames <- setNames( c("bapct"),as.list(inName))
  
      colName2 <- c(colName2,popnames)
      incProgress()
  }
  
  # Total Estimated Jobs
  if("jobs" %in% chkList) {
      sqlStrJobs <- paste0("SELECT placefips, year, jobs FROM estimates.muni_jobs_long WHERE year = ",eYr,";")
                          
      f.muniJobs <- dbGetQuery(DBPool, sqlStrJobs)
      names(f.muniJobs)[1] <- "GEOID"
      f.muniJobs$GEOID <- paste0(state,formatC(f.muniJobs$GEOID, width = 5, format = "d", flag = "0"))
      names(f.muniJobs)[3] <- "total_jobs"
    
    f.munifull <- inner_join(f.munifull, f.muniJobs[,c(1,3)], by="GEOID")
    inName <- paste0("Total Estimated Jobs (",eYr,")")
    popnames <- setNames( c("total_jobs"),as.list(inName))

    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  # Median Household Income
  if("medinc" %in% chkList) {
    f.munimedinc <- codemog_api(data="b19013",db=ACS,sumlev="160",geography="sumlev",meta="no")
    
    # Conver to numeric
    f.munimedinc[,8:ncol(f.munimedinc)] <- sapply(f.munimedinc[,8:ncol(f.munimedinc)],as.numeric)
    # Change geonum to geoid
    names(f.munimedinc)[7] <- "GEOID"
    f.munimedinc$GEOID <- substr(f.munimedinc$GEOID,2,nchar(f.munimedinc$GEOID))
    
    f.munimedinc$b19013001 <- ifelse(is.na(f.munimedinc$b19013001),0,f.munimedinc$b19013001)
    
    f.munimedinc <-   f.munimedinc %>% 
      filter(GEOID %in% allmuni0) %>%
      mutate(medianinc = b19013001)
    
    f.munifull <- inner_join(f.munifull,f.munimedinc[,c(7,9)],by="GEOID")
    inName <- paste0("Median Household Income (",eYr,")")
    popnames <- setNames( c("medianinc"),as.list(inName))
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  
  # % in poverty
  if("poverty" %in% chkList) {
    f.povertyr <- codemog_api(data="b17001",db=ACS,sumlev="160",geography="sumlev",meta="no")
    
    # Conver to numeric
    f.povertyr[,8:ncol(f.povertyr)] <- sapply(f.povertyr[,8:ncol(f.povertyr)],as.numeric)
    # Change geonum to geoid
    names(f.povertyr)[7] <- "GEOID"
    f.povertyr$GEOID <- substr(f.povertyr$GEOID,2,nchar(f.povertyr$GEOID))
    
    
    f.povertypct <- f.povertyr %>% 
      filter(GEOID %in% allmuni0) %>%
      mutate(npoverty = b17001002,
             tpop = b17001001,
             povpct = (npoverty/tpop)) 
    
    f.munifull <- inner_join(f.munifull,f.povertypct[,c(7,69)],by="GEOID")
    inName <- paste0("Percent of Persons Below the Poverty Line (",eYr,")")
    popnames <- setNames( c("povpct"),as.list(inName))
    
    colName2 <- c(colName2,popnames)
    incProgress()
  }
  
  #Standardizing and Formatting
  f.munifull$aggVal <- 0  
  if("totpop" %in% chkList) {
    f.munifull$popZ <- scale(f.munifull$totalpopulation)
    f.munifull$totalpopulation <- formatC(f.munifull$totalpopulation,digits=0,format="d", big.mark=",")
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$popZ
  }
  if("popgr" %in% chkList) {
    f.munifull$grZ <- scale(f.munifull$growthRate) 
    f.munifull$growthRate <- percent(f.munifull$growthRate)
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$grZ
  }
  
  if("pop2564" %in% chkList) {
    f.munifull$tpop2564pctZ <- scale(f.munifull$tpop2564pct)
    f.munifull$tpop2564pct <- percent(f.munifull$tpop2564pct*100)
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$tpop2564pctZ
  }
  
  if("pop65" %in% chkList) {
    f.munifull$tpop65pctZ <- scale(f.munifull$tpop65pct)
    f.munifull$tpop65pct <- percent(f.munifull$tpop65pct*100)
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$tpop65pctZ
  }  
  
  if("pctNW" %in% chkList) {
    f.munifull$HISPZ <- scale(f.munifull$pctHISP)
    f.munifull$pctHISP <- percent(f.munifull$pctHISP* 100)
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$HISPZ
  }  
  
  if("educ" %in% chkList) {
    f.munifull$bapctZ <- scale(f.munifull$bapct)
    f.munifull$bapct<- percent( f.munifull$bapct * 100)
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$bapctZ
  }  
  
  if("jobs" %in% chkList)  {
    f.munifull$jobsZ <- scale(f.munifull$total_jobs)
    f.munifull$total_jobs <- formatC(f.munifull$total_jobs,digits=0,format="d", big.mark=",")
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$jobsZ
  }
  
  
  if("medinc" %in% chkList) {
    f.munifull$incZ <- scale(f.munifull$medianinc)
    f.munifull$medianinc <- paste0("$",formatC(f.munifull$medianinc,digits=0,format="d", big.mark=","))
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$incZ
  }  
  
  if("poverty" %in% chkList) {
    f.munifull$povpctZ <- scale(f.munifull$povpct)
    f.munifull$povpct <- percent(f.munifull$povpct*100)
    f.munifull$aggVal <- f.munifull$aggVal + f.munifull$povpctZ
  }  
  f.munifull$aggRank <- (nrow(f.munifull) + 1) - rank(f.munifull$aggVal)  
  popnames <- c("Aggregate Rank"= "aggRank")
  colName2 <- c(colName2,popnames)
  
  # extracting AggVal
  f.muniAggVal <- f.munifull[,c("Municipality","aggVal")]
  names(f.muniAggVal)[2] <- "r"
  f.muniAggVal$r <- round(abs(f.muniAggVal$r),digits=2) + 1
  incProgress()
  
  # Locate column numbers
  aggN <- which( colnames(f.munifull)=="aggVal" ) 
  aggR <- ncol(f.munifull)
  f.muniTab <- f.munifull[,c(2:(aggN-1),aggR)]
  f.muniTab <- f.muniTab[order(f.muniTab$aggRank),]
  f.muniCluster <- f.munifull[,c(2,(aggN+1):(aggR-1))]
  
  
  # Generating Output  
  
  f.muninames <- f.muniCluster[,1]
  
  f.muniClus2 <- f.muniCluster[,c(2:ncol(f.muniCluster))]
  
  rownames(f.muniClus2) <- f.muninames
  
  # performing the clustering
  # Distance Matrix
  muniDist <- dist(f.muniClus2)
  f.clusStr <- hclust(muniDist,method="ward.D2")
  
  
  # Generating optimal clusters  based on PBC (Point by Serial Correlation)
  clusRange <- as.clustrange(f.clusStr,diss=muniDist,ncluster=20)
  NClus <- as.data.frame(summary(clusRange, max.rank=2))
  
 if(NClus[1,1] == 2) {
   fin_clus <- as.data.frame(cutree(f.clusStr,k=3))
 } else {
   fin_clus <- as.data.frame(cutree(f.clusStr,k=NClus[1,1]))
 }
 
  muni_clus <- cbind(f.muninames,fin_clus)
  rownames(muni_clus) <- c()
  names(muni_clus) <- c("Municipality","cluster")
  muni_clus$Municipality  <- as.character(muni_clus$Municipality)
  
  
  f.munifinclus <- inner_join(muni_clus,f.muniAggVal, by="Municipality") 
  incProgress()

  #Outputting data for Circle Packing Diagram
 
  color_min = "hsl(56,80%,80%)" 
  color_max = "hsl(341,30%,40%)"
  size = "r"
  # Output data tree
  f.munifinclus$clusterName <- paste0("Cluster ",f.munifinclus$cluster)
  
  f.munifinclus$pathString <- paste("state", f.munifinclus$clusterName, f.munifinclus$Municipality, sep="/")
  f.tree <-as.Node(f.munifinclus)
  
  
  
# outputting DT ranking Table  

names(f.muniTab)[1] <- "Municipality"
f.muniTab <- inner_join(muni_clus,f.muniTab, by="Municipality") 
f.muniTab <- f.muniTab[order(f.muniTab$aggRank),]
popnames2 <- c("Cluster" = "cluster")
popnames1 <- colName2[1]
popnamesend <- colName2[2:length(colName2)]

colName2 <- c(popnames1,popnames2,popnamesend)


names(f.muniTab) <-  names(colName2)
f.muniTab <- f.muniTab[,c(1,2,ncol(f.muniTab),3:(ncol(f.muniTab)-1))]

OutObj <- list("outtree" = f.tree, "data" = f.muniTab)
return(OutObj)
})
}
  