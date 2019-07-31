#'  listTofips : Produces a vector of FIPS codes from an input list of Census County and Place Name Codes.
#'
#' @param  lvl the comparison level ("Regional Summary","Region to County","County to County","Municipality to Municipality")
#' @param inlist1 The data frame for the base of comparison
#' @param value1  The selected item for the base of the comparison
#' @param inlist2 The data frame for the comparison
#' @param value2  The selected item(s) for the comparisons
#' @return the fipscode(s) for a selected data level
#' @export

listTofips <- function(lvl,inlist1,value1,inlist2,value2){
  # Function to produce a vector of FIPS codes from an input list of names and codes
  if(lvl == "Regional Summary") {
           reglist <- switch(value1,
                    "Denver PMSA" = c("08001", "08005", "08014", "08031", "08035", "08059"),
                    "Denver-Boulder Metro Area" = c("08001", "08005", "08013", "08014", "08031", "08035", "08059"),
                    "Denver-Boulder-Greely CMSA" = c("08001", "08005", "08013", "08014", "08031", "08035", "08059", "08123"),
                    "10 County Denver Metro Area" = c("08001", "08005", "08014", "08019", "08031", "08035", "08041", "08047", "08059", "08093"),
                    "Central Mountains" = c("08015", "08019", "08027", "08043", "08047", "08055", "08065", "08071", "08093"),	
                    "Eastern Plains" = c("08009", "08011", "08017", "08025", "08039", "08061", "08063", "08073", "08075", "08087", "08089", "08095", "08099", "08115", "08121", "08125"),
                    "Front Range" = c("08001", "08005", "08013", "08014", "08031", "08035", "08041", "08059", "08069", "08101","08119","08123"),
                    "San Luis Valley" = c("08003", "08021", "08023", "08079", "08105", "08109"),
                    "Western Slope" = c("08007", "08029", "08033", "08037", "08045", "08049", "08051", "08053", "08057", "08067", "08077", "08081", "08083", "08085", "08091", "08097", "08103", "08107", "08111", "08113", "08117"),
                    "Region  1: Northern Eastern Plains"	= c("08075","08087","08095","08115","08121","08125"),
                    "Region  2: Northern Front Range"	= c("08069","08123"),
                    "Region  3: Denver Metropolitan Area"	= c("08001","08005","08013","08014","08019","08031","08035","08047","08059"),
                    "Region  4: Southern Front Range"	= c("08041","08093","08119"),
                    "Region  5: Central Eastern Plains"	= c("08017","08039","08063","08073"),
                    "Region  6: Southern Eastern Plains"	= c("08009","08011","08025","08061","08089","08099"),
                    "Region  7: Pueblo County"	= c("08101"),
                    "Region  8: San Juan Valley"	= c("08003","08021","08023","08079","08105","08109"),
                    "Region  9: Southern Western Slope"	= c("08007","08033","08067","08083","08111"),
                    "Region 10: Central Western Slope" = c("08029","08051","08053","08085","08091","08113"),
                    "Region 11: Northern Western Slope" = c("08045","08077","08081","08103","08107"),
                    "Region 12: Northern Mountains" = c("08037","08049","08057","08097","08117"),
                    "Region 13: Central Mountains" = c("08015","08027","08043","08065"),
                    "Region 14: Southern Mountains" = c("08055","08071")
           )
          fipsl <- list("length1" = length(reglist),"plName1" = value1, "list1"= reglist,"length2" = 0, "plName2" = CountyName(reglist), "list2" = "")
         }
  
  if(lvl == "Region to County") {
    reglist <- switch(value1,
                      "Denver PMSA" = c("08001", "08005", "08014", "08031", "08035", "08059"),
                      "Denver-Boulder Metro Area" = c("08001", "08005", "08013", "08014", "08031", "08035", "08059"),
                      "Denver-Boulder-Greely CMSA" = c("08001", "08005", "08013", "08014", "08031", "08035", "08059", "08123"),
                      "10 County Denver Metro Area" = c("08001", "08005", "08014", "08019", "08031", "08035", "08041", "08047", "08059", "08093"),
                      "Central Mountains" = c("08015", "08019", "08027", "08043", "08047", "08055", "08065", "08071", "08093"),	
                      "Eastern Plains" = c("08009", "08011", "08017", "08025", "08039", "08061", "08063", "08073", "08075", "08087", "08089", "08095", "08099", "08115", "08121", "08125"),
                      "Front Range" = c("08001", "08005", "08013", "08014", "08031", "08035", "08041", "08059", "08069", "08101","08119","08123"),
                      "San Luis Valley" = c("08003", "08021", "08023", "08079", "08105", "08109"),
                      "Western Slope" = c("08007", "08029", "08033", "08037", "08045", "08049", "08051", "08053", "08057", "08067", "08077", "08081", "08083", "08085", "08091", "08097", "08103", "08107", "08111", "08113", "08117"),
                      "Region  1: Northern Eastern Plains"	= c("08075","08087","08095","08115","08121","08125"),
                      "Region  2: Northern Front Range"	= c("08069","08123"),
                      "Region  3: Denver Metropolitan Area"	= c("08001","08005","08013","08014","08019","08031","08035","08047","08059"),
                      "Region  4: Southern Front Range"	= c("08041","08093","08119"),
                      "Region  5: Central Eastern Plains"	= c("08017","08039","08063","08073"),
                      "Region  6: Southern Eastern Plains"	= c("08009","08011","08025","08061","08089","08099"),
                      "Region  7: Pueblo County"	= c("08101"),
                      "Region  8: San Juan Valley"	= c("08003","08021","08023","08079","08105","08109"),
                      "Region  9: Southern Western Slope"	= c("08007","08033","08067","08083","08111"),
                      "Region 10: Central Western Slope" = c("08029","08051","08053","08085","08091","08113"),
                      "Region 11: Northern Western Slope" = c("08045","08077","08081","08103","08107"),
                      "Region 12: Northern Mountains" = c("08037","08049","08057","08097","08117"),
                      "Region 13: Central Mountains" = c("08015","08027","08043","08065"),
                      "Region 14: Southern Mountains" = c("08055","08071")
    )
    # Building County List
      ctylist <- list()
       for(i in 1:length(value2)){
         ctylist <- c(ctylist, paste0("08",formatC(inlist2[which(inlist2$municipalityname == value2[i]),1],digits=0, width=3, format="f",flag= "0")))
      }
    fipsl <- list("length1" = length(reglist),"plName1" = value1,"list1" = reglist,"length2" = length(ctylist), "plName2" = value2, "list2" = ctylist)
  }
if(lvl == "County to County") {
    ctylist1 <- paste0("08",formatC(inlist1[which(inlist1$municipalityname == value1),1],digits=0, width=3, format="f",flag= "0"))
    ctylist2 <- list()
    for(i in 1:length(value2)){
      ctylist2 <- c(ctylist2,paste0("08",formatC(inlist2[which(inlist2$municipalityname == value2[i]),1],digits=0, width=3, format="f",flag= "0")))
    }
   
    fipsl <- list("length1" = length(ctylist1),"plName1" = value1, "list1" = ctylist1,"length2" = length(ctylist2),"plName2" = value2, "list2" = ctylist2)
}
if(lvl == "Municipality to Municipality") {
    munilist1 <-unique(paste0("08",sprintf("%05d",inlist1[which(inlist1$municipalityname == value1),2])))
    munilist2 <- list()
    for(i in 1:length(value2)){
      x <- unique(paste0("08",sprintf("%05d",inlist2[which(inlist2$municipalityname == value2[i]),2])))
      munilist2 <- c(munilist2, x)
    }
  fipsl <- list("length1" = length(munilist1),"plName1" = value1, "list1" = munilist1,"length2"= length(munilist2),"plName2" = value2, "list2" = munilist2)
} #Municipalities

  return(fipsl)
} #end listTofips
