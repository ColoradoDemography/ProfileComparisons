#' downloadObj  File Download Modules
#'
#' downloadObj is the server function that facilitates the download
#'
#' @param place is the place name, typically the value of input$unit
#' @param oname the input data description object
#' @param dboj is the data object to be output
#' @export

downloadObj <- function(input, output, session, place, oname, dobj) {

  prefix <- switch(oname,
                   "ctydata" = " County Ranking",
                   "munidata" = " Municipality Ranking",
                   "statstabl" =  " Basic Statistics",
                   "popf1data" = " Population Estimates and Forecasts",
                   "popf2data" = " Components of Change",
                   "popf3data" = " Components of Change",
                   "poph1data" = " Housing Type Estimates",
                   "poph3data" = " Housing Type Estimates",
                   "poph2data" = " Housing Type Estimates",
                   "popei1data" = " Base Industries",
                   "popei2data" = " Estimated Jobs",
                   "popei3data" = " Estimated Firms",
                   "popem1data" = " Job Estimates and Forecast",
                   "popem2data" = " Weekly Wages",
                   "popem3data" = " Unempolyment Rates"

  )

  suffix <- " Data.csv"

  output$download <-  downloadHandler(
    filename = function() {
        paste0(place,prefix,suffix)
    },
    content = function(file) {
        write_csv(dobj, file)
     } #content
  ) #DowhloadHandler
} #downloadObj
