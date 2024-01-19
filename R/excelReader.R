fileName = c(FHA = "FHA Q2 July-Sept 2023.xlsx",
             VIHA = "VIHA Q1 April-June 2023.xlsx",
             IHA = "IHA Q2 July-Sept 2023.xlsx",
             NHA = "NHA Q2 Jul-Sep 2023.xlsx",
             PHSA = "PHSA_Q2 July-Sept 2023.xlsx",
             VCH = "VCH Q2 July-Sept 2023.xlsx"

)

fileLocation <- "data//"

# Taken from FHA file, might not match with others
tabName <- c("Adult SU Treatment Beds",
             "Adult SU Supportive Recovery ",
             "Adult SU Withdrawal Mgmt. Beds",
             "Budget 2021")


#serviceTypeColName = "ServiceType"
tabColName = "TabName"

#' Title
#'
#' @param Path
#' @param sheetName
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
readExceltab <- function(sheetName,
                            path,
                            skip = 0,
                            na = "N/A",
                            col_names = FALSE){

  df <- readxl::read_xlsx(path = path,
                          sheet = sheetName,
                          skip = 0,
                          na = "N/A",
                          col_names = FALSE)

  colTokeep <- unlist(lapply(names(df), function(x, y) isAllna(y[, x, drop = T]), df))
  df[, !colTokeep]
}


readExcelFile <- function(path){
  svcTypeTable <- makeServiceTypeTable()
  tabList <- readxl::excel_sheets(path = path)
  tblWithTabName <- attachTabToServiceType(serviceTypeTable = svcTypeTable,
                                           tabList = tabList,
                                           colToLook = svcType,
                                           colToAdd = tabColName)

  tblWithTabName <- tblWithTabName[!is.na(tblWithTabName[, tabColName, drop = FALSE]), , drop = FALSE]
  output <- lapply(setNames(tblWithTabName[, tabColName, drop = TRUE],
                  tblWithTabName[, svcType, drop = TRUE]),
         readExceltab,
         path = path)

  addSvcTypeColumn(output)
}



