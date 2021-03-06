#' downloadObjUI  File Download Modules
#'
#' downloadObjUI is the UI function that creates the download buttons
#'  Updated 6/2018 to facilitate table downloads 
#' @param id is the data name and creates the module/namespace ID
#' @return output button type (plot, table  or data)
#' @export

downloadObjUI <- function(id) {
 
  ns <- NS(id)
  #Identifying data object and type
  if(nchar(id) == 7) {
    dtype <- substr(id,4,7)
  }
  if(nchar(id) == 8) {
    dtype <- substr(id,5,8)
  }
  if(nchar(id) == 9) {
       dtype <- substr(id,6,9)
  }
  if(nchar(id) == 10) {
    dtype <- substr(id,7,10)
  }


  #setting button label
  outLabel <- ifelse(dtype == "plot","Download Plot",
              ifelse(dtype == "data","Download Data","Download Table"))

  downloadButton(ns("download"),outLabel)
}
