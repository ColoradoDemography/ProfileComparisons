#' chkID Returns a set of ids
#'
#' @param lvl the data level from input$level
#' @param fipslist the list of fips codes 
#' @param plName the placeName variable
#' @param ctyList the list of county names and information
#' @param plList the list of place names and information
#' @return a list of 6 items: ctyNum, ctyName, plNum, plName, multiCty and PlFilter
#' @export
#'
chkID <- function(lvl,fipslist) {
  if(lvl == "County Ranking") {
    ctyName1 <- ""
    ctyNum1 <- ""
    plNum1 <- ""
    plName1 <- ""
    ctyName2 <- ""
    ctyNum2 <- ""
    plNum2 <- ""
    plName2 <- ""
  }
  if(lvl == "Municipal Ranking") {
    ctyName1 <- ""
    ctyNum1 <- ""
    plNum1 <- ""
    plName1 <- ""
    ctyName2 <- ""
    ctyNum2 <- ""
    plNum2 <- ""
    plName2 <- ""
  }
  if(lvl == "Regional Summary") {
    ctyName1 <- fipslist$plName1
    ctyNum1 <- substr(fipslist$list1,3,5)
    plNum1 <- ""
    plName1 <- ""
    ctyName2 <- ""
    ctyNum2 <- ""
    plNum2 <- ""
    plName2 <- ""
  }

  if(lvl == "Region to County") {
    ctyName1 <- fipslist$plName1
    ctyNum1 <- substr(fipslist$list1,3,5)
    plNum1 <- ""
    plName1 <- ""
    ctyName2 <- fipslist$plName2
    ctyNum2 <- substr(fipslist$list2,3,5)
    plNum2 <- ""
    plName2 <- ""
  }
  
  if(lvl == "County to County") {
    ctyName1 <- fipslist$plName1
    ctyNum1 <- substr(fipslist$list1,3,5)
    plNum1 <- ""
    plName1 <- ""
    ctyName2 <- fipslist$plName2
    ctyNum2 <- substr(fipslist$list2,3,5)
    plNum2 <- ""
    plName2 <- ""
  }
  
  if(lvl == "Municipality to Municipality") { 
    ctyName1 <- ""
    ctyNum1 <- ""
    plNum1 <- fipslist$list1
    plName1 <- fipslist$plName1
    ctyName2 <- ""
    ctyNum2 <- ""
    plNum2 <- fipslist$list2
    plName2 <- fipslist$plName2
  }    

 
  outList <- list("ctyName1" = ctyName1, "ctyNum1" = ctyNum1, "plName1" = plName1, "plNum1" = plNum1,
                  "ctyName2" = ctyName2, "ctyNum2" = ctyNum2, "plName2" = plName2, "plNum2" = plNum2)
  return(outList)
}