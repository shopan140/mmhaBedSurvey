# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(mmhaBedSurvey)
library(dplyr)

test_check("mmhaBedSurvey")

library(readxl)


df <- Map(function(x, y) readExcelFile(paste0("data//",y,"//", x)), fileName, names(fileName))


compiledData <- mergeDataFrame(df, colName = haShortName)


excelData <- compiledData[, !names(compiledData) %in% c("ServiceType", "HealthAuthority")]


excelTableWithAtleastOneData <- compiledData[rowSums(!is.na(excelData[, c(2:10)])) > 0, ]

spfullname <- compiledData$...1[findItems("indicator", compiledData$...1, offset = -1)]

write.csv(spfullname, "spname.csv")
View(spfullname)
# sp <- makeServiceProviderTable(compiledData[compiledData$HealthAuthority == "FHA", 1, drop = T],
#                                haName = "FHA")


# make it a function
sp <- lapply(unique(compiledData$HealthAuthority),
       function(x, df) makeServiceProviderTable(df[df[, haShortName] == x, 1, drop = TRUE],
                                                haName = x), compiledData)

sp <- bind_rows(sp)
View(temp[!is.na(temp$...8), ])


indicators <- checkUniqueIndicators(excelTableWithAtleastOneData)
tickerdf <- makeTicker(indicatorDf = indicators, textList = accessAndUtilization)



writexl::write_xlsx(uniqIndicatorStr,  "indicator.xlsx")
