
healthAuthority <- c("FHA" = "Fraser Health Authority",
                     "IHA" = "Interior Health Authority",
                     "NHA" = "Northern Health Authority",
                     "PHSA" = "Provincial Health Services Authority",
                     "VCH" = "Vancouver Coastal Health Authority",
                     "VIHA" = "Vancouver Island Health Authority")


makeHealtAuthorityTable <- function(ha= healthAuthority,
                                    columnNames = c(haShortName, haFullName)){
  makeTable(vec = ha, columnNames = columnNames)
}

attachFilenNametoHA <- function(haTable,
                                fileNames,
                                haColumnName = haShortName,
                                colToAdd = haFullName){

  haTable <- haTable[order(haTable[, haColumnName, drop = TRUE]), , drop = FALSE]
  fileNames <- fileNames[order(fileNames)]
  haTable[, colToAdd] <- fileNames
  haTable
}





