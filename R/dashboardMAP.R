#' dashboardMAP Creates a simple map that highlights a Colorado County or place
#'   Modified from cp_countymap  AB 2/2018
#'   Revised 3/2018 to account for standalone JSON dataset
#'
#' This function creates a map to be used in the profile process,
#'    If a planning region is selected, the plannign region is colored in
#'    If a county is selected, the county is colored in and the planning region is outlined
#'    if a place is selected, the county is outlined and a dagger is posted at the center of the place.
#'
#'
#' @param listID the list containing place id and Place names
#' @export

dashboardMAP <- function(DBPool,lvl,listID){

# Setting up colors for leaflet map  counties
  geCol <- rgb(red = 160, green = 160, blue = 160,max=255)
  grCol <- rgb(red = 0, green = 102, blue = 0,max=255)
  blCol <- rgb(red = 0, green = 0, blue = 255,max=255)
  factpal <- colorFactor(c(geCol,grCol,blCol), c(0,1,2))
  
  getColor <- function(indata) {
    sapply(indata$sel, function(x) {
      if(x == 0) {
        "grey"
      } else if(x == 1) {
        "green"
      } else {
        "blue"
      } })
  }  
  
  
  # Extracting the county data  
  # mapcty has the county fips code, the county name and the coordinates of the county center 
  #Accessing JSON file, with Counties 
  
  data_file <- "www/County_GEN_2014.geojson"
  data_json <- geojson_read(data_file, what = "sp")
  
  # This returns the centroid of each ploygon (county)  with the proper label
  centroids <- coordinates(data_json)
  
  centroids2 <- as.data.frame(SpatialPointsDataFrame(coords=centroids, data=data_json@data))
  mapcty <- centroids2[,c(3,7,11,12)]
  names(mapcty) <-c("ctyfips","ctyname","Long","Lat")
  
  # Do different stuff for each type of comparison

  if(lvl == "Regional Summary") {
    ctyList <-  listID$ctyNum1
    
    mapcty$sel <- ifelse(mapcty$ctyfips %in% ctyList,1,0)
    mapcty$ctyname <- ifelse(mapcty$sel == 1, listID$ctyName1,"")
    
    
    mapctylab <- mapcty[which(mapcty$sel == 1),] %>%
      group_by(ctyname) %>%
      summarize(Long = mean(Long),
                Lat = mean(Lat))
    
    
    
    # produce leaflet map
    m <-  leaflet(data_json) %>%
      addPolygons(color = "black",stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1,
                  fillColor = ~factpal(mapcty$sel))  %>% 
      addLabelOnlyMarkers(lat = ~ mapctylab$Lat, lng = ~ mapctylab$Long, label = ~ htmlEscape(mapctylab$ctyname),
                          labelOptions = labelOptions(noHide = T, direction = 'auto'),
                          options = markerOptions(riseOnHover = TRUE))
    
    
    }
  
  if(lvl == "Region to County") {
    mapcty$sel <- ifelse(mapcty$ctyfips %in% listID$ctyNum1,1,0)
    mapcty$sel <- ifelse(mapcty$ctyfips %in% listID$ctyNum2,2,mapcty$sel)
    
    # Calculating center of region
    mapctylab <- mapcty[which(mapcty$sel == 1),] %>%
      group_by(sel) %>%
      summarize(Long = mean(Long),
                Lat = mean(Lat))
    mapctylab$ctyname <- listID$ctyName1
    mapctylab$ctyfips <- "000"
    mapctylab <- mapctylab[,c(5,4,2,3,1)]
    
    mapcty2 <- mapcty[which(mapcty$sel == 2),]
    mapctylab <- bind_rows(mapctylab,mapcty2)
    
    # produce leaflet map
    m <-  leaflet(data_json) %>%
      addPolygons(color = "black",stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1,
                  fillColor = ~factpal(mapcty$sel))  %>% 
      addLabelOnlyMarkers(lat = ~ mapctylab$Lat, lng = ~ mapctylab$Long, label = ~ htmlEscape(mapctylab$ctyname),
                          labelOptions = labelOptions(noHide = T, direction = 'auto'),
                          options = markerOptions(riseOnHover = TRUE))
    
  }
  
  if(lvl == "County to County") {
    mapcty$sel <- ifelse(mapcty$ctyfips %in% listID$ctyNum1,1,
                         ifelse(mapcty$ctyfips %in% listID$ctyNum2,2,0))
    
    mapctylab <- mapcty[which(mapcty$sel != 0),]
    
    # produce leaflet map
    m <-  leaflet(data_json) %>%
      addPolygons(color = "black",stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1,
                  fillColor = ~factpal(mapcty$sel))  %>% 
      addLabelOnlyMarkers(lat = ~ mapctylab$Lat, lng = ~ mapctylab$Long, label = ~ htmlEscape(mapctylab$ctyname),
                          labelOptions = labelOptions(noHide = T, direction = 'auto'),
                          options = markerOptions(riseOnHover = TRUE) )
     }
  
  
  if(lvl == "Municipality to Municipality"){

    plfips2 <- unique(c(listID$plNum2,listID$plNum1))
    
    f.muni <- data.frame()
    
    for(i in 1:length(plfips2)) {
      muni <- dbGetQuery(DBPool, paste0("SELECT geoid, placefp, name, x, y FROM bounds.place_centroids WHERE  geoid = '",plfips2[i], "';"))
      muni$sel <- 0
      if(muni$geoid %in% listID$plNum1){
        muni$sel <- 1
      }
      if(muni$geoid %in% listID$plNum2){
        muni$sel <- 2
      }
      
      f.muni <- bind_rows(f.muni,muni)
    }
    
    f.muni$Lat <- as.numeric(f.muni$y)
    f.muni$Long <- as.numeric(f.muni$x)
   
    # Build Icoms
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(f.muni)
    )
    
    
    # produce leaflet map
    m <-  leaflet(data_json) %>%
      addPolygons(color = "black",stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1,
                  fillColor = geCol)  %>% 
      addAwesomeMarkers(lng = ~ f.muni$Long, lat = ~ f.muni$Lat, label = ~ htmlEscape(f.muni$name),
                 icon = icons, 
                 labelOptions = labelOptions(noHide = T, direction = 'auto'),
                 options = markerOptions(riseOnHover = TRUE))
  }  
  
  
  return(m)
}