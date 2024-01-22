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



nameExcelColumn <- function(df,
                            indicatorColumn = 1,
                            rowForColumnName  =  2,
                            colToStart =  2){

  names(df)[[indicatorColumn]] <- indicatorExcelName


  for( i in colToStart:ncol(df)){
    quarter <- timeSeries[unlist(lapply(timeSeries,
                                        function(x) isTextMatch(df[rowForColumnName, i, drop = TRUE],
                                                                x)))]
    if (length(quarter) > 0){
      columnName <- names(quarter)[[1]]
      names(df)[[i]] <- columnName
    }

  }
  df
}



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

  print(paste("Reading Tab:", sheetName, ", file: ", path))
  df <- readxl::read_xlsx(path = path,
                          sheet = sheetName,
                          skip = 0,
                          na = "N/A",
                          col_names = FALSE)

  colTokeep <- unlist(lapply(names(df), function(x, y) isAllna(y[, x, drop = T]), df))
  df <- df[, !colTokeep]
  nameExcelColumn(df)

}



readExceltab0 <- function(sheetName,
                         path,
                         skip = 0,
                         na = "N/A",
                         col_names = FALSE,
                         spNameLocator = "indicator"){

  df <- readExceltab(sheetName = sheetName, path = path, skip = skip)

# this rquires a seperate funciton
  spNameIndex <- which(isTextMatch(df[, indicatorExcelName, drop = TRUE], spNameLocator) |
                         isTextMatch(df[, indicatorExcelName, drop = TRUE], "yes"))-1

  spNameIndex <- c(spNameIndex, nrow(df))
  # split table
  if(length(spNameIndex)>1){
    comp <- lapply(c(2:length(spNameIndex)),
                   function(x, y) orgnizeSingleSP(y[spNameIndex[x-1]:(spNameIndex[x]-1), ]), df)

    comp <- dplyr::bind_rows(comp)
  }
  else if(length(spNameIndex) == 1){
    comp <- orgnizeSingleSP(df)
  }

  else{
    return(NA)
  }

  comp <- comp [!is.na(comp[, indicatorExcelName, drop = TRUE]), ]

  comp %>%
    tidyr::pivot_longer(cols = all_of(names(comp)[!names(comp)
                                                  %in% c(indicatorExcelName, spName)]),
                        names_to = "Quarter",
                 values_to = "Value")
  }


readExcelFile <- function(path, tabReader = readExceltab){
  svcTypeTable <- makeServiceTypeTable()
  tabList <- readxl::excel_sheets(path = path)
  tblWithTabName <- attachTabToServiceType(serviceTypeTable = svcTypeTable,
                                           tabList = tabList,
                                           colToLook = svcType,
                                           colToAdd = tabColName)

  tblWithTabName <- tblWithTabName[!is.na(tblWithTabName[, tabColName, drop = FALSE]), , drop = FALSE]

  if(nrow(tblWithTabName) < 1){
    print(paste(path, ": No tab match"))
  }
  output <- lapply(setNames(tblWithTabName[, tabColName, drop = TRUE],
                  tblWithTabName[, svcType, drop = TRUE]),
                  tabReader,
         path = path)

  addSvcTypeColumn(output)
}



# id, indicatorName, value (char), servicetype, serviceProvider, date

#split the table as per provider
# fix the name
# get rid of first two row
# add column name serviceprovider.
#iterate throw sp index.

# organizeExcelData <- function(df, matchingCol = "Q1FY23.24", offst = -1){
#   serviceProviderIndex <- which(isTextMatch(df[, matchingCol, drop = TRUE],
#                                             timeSeries[[matchingCol]])) + offst
#
#
#
#
#     }

orgnizeSingleSP <- function(df, spNameRow = 1, dateRow = 2){
  spNametext <- getServiceProviderName(df[spNameRow, indicatorExcelName, drop = T])
  df <- df[3:nrow(df), ]
  df[, spName] <- spNametext
  df
}

extractDateColName <- function(df, dateRow = 2, startingCol = 2){
  namelist <- df[dateRow, startingCol:ncol(df), drop = TRUE]
  namelist <- unlist(namelist)
  namelist[!is.na(namelist)]
}


