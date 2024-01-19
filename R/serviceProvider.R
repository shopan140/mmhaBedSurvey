
getServiceProviderName <- function(spNameText, pattern = "[()]", keep = 1){

  spNameText <- trimws(spNameText)
  spNameText <- gsub("\\([^)]*\\)|\\d|\\b\\w\\.", "", spNameText)

  if (gregexpr("\\(", spNameText) == 1){keep = 2}
  output <- trimws(unlist(strsplit(spNameText, split = pattern)))

  output <- output[output != ""]

  if (length(output) <= keep){
    return (output[[keep]])
  }
  else return (output[[1]])

}


getServiceProviderBedNumber <- function(spNameText, pattern = "\\d+"){
  spNameText <- trimws(spNameText)
  output <- stringr::str_extract_all(spNameText, "\\d+")
  output
}



makeServiceProviderTable <- function(dList,
                                     haName,
                                     keyItem = "indicator",
                                     off = -1,
                                     id = idCol,
                                     nameCOl = spName,
                                     haCol = haShortName,
                                     dateCol = updateDate){
  dList <- dList[findItems(keyItem, dList, offset = off)]

  dList <- unlist(lapply(dList, getServiceProviderName))

  colNm <- c(id, nameCOl, haCol, dateCol)

  df <- data.frame(matrix(nrow = length(dList), ncol = length(colNm)))
  names(df) <- colNm
  df[, id] <- 1:length(dList)
  df[, nameCOl] <-  dList
  df[, haCol] <- haName
  df[, dateCol] <- as.Date(Sys.Date())

  df

}

combineServiceProviderTable <- function(df){
  sp <- lapply(unique(df[, haShortName, drop = TRUE]),
               function(x, y) makeServiceProviderTable(
                 y[y[, haShortName] == x, 1, drop = TRUE],
                 haName = x), df)

  sp <- bind_rows(sp)
  sp[, idCol] <- seq_along(sp[, idCol])
  sp
}
