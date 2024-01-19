#' compare two vector element by element
#'
#' @param vec1
#' @param vec2
#'
#' @return a bool vector equal to the highest length vec
#' @export
#'
#' @examples compare(c("2", "vec"), c("3, "vec", "vec3))
compare <- function(vec1, vec2){

  if(length(vec1) > length(vec2)) {
    #stop("If vector size is different, please put the higher length second.")
    temp <- vec1
    vec1 <- vec2
    vec2 <- temp
}
  output <- vector(mode = "logical", length = length(vec2))

  for( i in seq_along(vec1)){
       for( j in seq_along(vec2)){
      if (vec1[i] == vec2[j] & !is.na(vec1[i] == vec2[j])){
        output[i] <- TRUE
      }
    }
  }

  output
}

#' find the item position index
#'
#' @param pattern is string to match
#' @param itemList list of items where pattern is matched
#' @param offset finding index relative to the pattern position
#'
#' @return a vector of index
#' @export
#'
#' @examples
findItems <- function(pattern, itemList, offset = 0){
  pattern <- tolower(pattern)
  output <- grep(pattern, tolower(itemList)) + offset
  output[output != 0]
}

isExist <- function(pattern, aList, isCompleteWord = FALSE){
  if (isCompleteWord){
    pattern <-  paste0("\\b", pattern, "\\b")
  }

  output <- aList[grepl(pattern, aList, ignore.case = TRUE)]

  if(length(output) == 0) {
    return (NA)
  }
  output
}

makeTable <- function(vec, columnNames = NULL, idColName = idCol){

  nameList <- names(vec)
  names(vec) <- NULL

  if(is.null(nameList)){
    df <- data.frame(vec)
  }
  else {
    df <- data.frame(list(nameList, vec))
  }

  if(!is.null(columnNames)){
    names(df) <- columnNames
  }
  columnNames <- names(df)
  df[, idColName] <- 1:nrow(df)

  df[, c(idColName, columnNames)]

}


attachcolForValue <- function(mainTable,
                                   listToAdd,
                                   colToLook,
                                   newColName ){



  for( i in seq_along(mainTable[[colToLook]])){
    mainTable[i, newColName] <- isExist(mainTable[i, colToLook, drop = TRUE], listToAdd)
  }

  mainTable
}

mergeDataFrame <- function(dfList, colName){
  nm <- names(dfList)

  for(i in seq_along(dfList)){
    dfList[[i]][, colName, drop = FALSE] <- nm[[i]]
  }

  dplyr::bind_rows(dfList)

}


addSvcTypeColumn <- function(dfList, colName = svcType){

  mergeDataFrame(dfList = dfList, colName = svcType)

}


isAllna <- function(vec, checkFor = NA){
  if(sum(!is.na(vec))>0){
    return (FALSE)
  }
  return (TRUE)
}
