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


df <- Map(function(x, y) readExcelFile(paste0("data//",y,"//", x), tabReader = readExceltab0), fileName, names(fileName))


compiledData <- mergeDataFrame(df, colName = haShortName)

#
# excelData <- compiledData[, !names(compiledData) %in% c("ServiceType", "HealthAuthority")]
#
#
# excelTableWithAtleastOneData <- compiledData[rowSums(!is.na(excelData[, c(2:10)])) > 0, ]
#
# spfullname <- compiledData$...1[findItems("indicator", compiledData$...1, offset = -1)]
#
# write.csv(spfullname, "spname.csv")
# View(spfullname)
# # sp <- makeServiceProviderTable(compiledData[compiledData$HealthAuthority == "FHA", 1, drop = T],
# #                                haName = "FHA")
#
#
# # make it a function
# sp <- lapply(unique(compiledData$HealthAuthority),
#        function(x, df) makeServiceProviderTable(df[df[, haShortName] == x, 1, drop = TRUE],
#                                                 haName = x), compiledData)
#
# sp <- bind_rows(sp)
# View(temp[!is.na(temp$...8), ])
#
#
# indicators <- checkUniqueIndicators(excelTableWithAtleastOneData)
# tickerdf <- makeTicker(indicatorDf = indicators, textList = accessAndUtilization)
#
# groupByMatchingText( indicatorDf = indicators,
#                      textVec = accessAndUtilization[[34]])
#
#
# writexl::write_xlsx(uniqIndicatorStr,  "indicator.xlsx")
#
#
#
#
# excelFileName <- "data//FHA Q2 July-Sept 2023.xlsx"
# excelTabName <- "Adult SU Treatment Beds"
#
# rm(df)
# df <- readExceltab(sheetName = excelTabName, path = excelFileName)
# names(df)[[1]] <- indicatorExcelName
# rowForColumnName  <-  2
# colToStart <-  2
# for( i in colToStart:ncol(df)){
#   quarter <- timeSeries[unlist(lapply(timeSeries,
#                                       function(x) isTextMatch(df[rowForColumnName, i, drop = TRUE],
#                                                               x)))]
#   if (length(quarter) > 0){
#     columnName <- names(quarter)[[1]]
#     names(df)[[i]] <- columnName
#   }
#
# }
#
# quarter <- timeSeries[c(F, F, F, F)]
#
# rm(df)
#
#
#
# l1 <- findItems("indicator", df$IndicatorNameInExcel, offset = -1)
# l2 <- findItems(Q1FY23.24, df$...2, offset = -1)
#
#
# union(l1,l2)
#
# for ( i in 2:ncol(df)){
#   is.na(findItems(Q1FY23.24, df[, 4, drop = TRUE]))
# }
#
#
# temp <- compiledData[compiledData$...1 == "Percentage of clients on OAT at:\r\n-admission\r\n-discharge"
#  & !is.na(compiledData$...1), ]



# read a tab
# name first column excelIndicatorName
#find indicator -1, check it is equal to 1 or not
# if 1 then rund the service provider name keep in a variable
# move to second column, check date, rename column
#move to third column, check date, rename and go on
# if no date remove the column
# how to check we are not mixing up survice provider.
# check with date stuffsplit the df


servieProviderIndex <- which(isTextMatch(compiledData$IndicatorNameInExcel, "indicator") |
                         isTextMatch(compiledData$IndicatorNameInExcel, "yes"))-1


sp <- compiledData[servieProviderIndex, indicatorExcelName, drop = T]

nmbd <- sapply(sp, function(x) getServiceProviderBedNumber(x))

nmbd <- unlist(nmbd)



x = getServiceProviderBedNumber("get this new")
sp <- data.frame(spName = sp, NumOfBed = )



comp <- lapply(c(2:length(servieProviderIndex)),
       function(x, y) orgnizeSingleSP(y[servieProviderIndex[x-1]:(servieProviderIndex[x]-1), ]), compiledData)


comp <- bind_rows(comp)

comp <- makeTicker(indicatorDf = comp, textList = accessAndUtilization)


compLong <- comp %>%
  filter(!is.na(IndicatorNameInExcel)) %>%
  pivot_longer(cols = all_of(names(getQuarterList())), names_to = "Quarters", values_to = "Value")


compUniqClient <- compLong %>%
  filter(IndicatorTicker == "UniqClientServed") %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(!is.na(Value))


compUniqClient %>%
  filter(ServiceType !="Budget") %>%
  group_by(HealthAuthority, Quarters) %>%
  summarise(Value = sum(Value)) %>%
  ggplot(mapping = aes(x = HealthAuthority, y = Value, fill = Quarters))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Value),
            position = position_dodge(width = 0.9), vjust = -.5)+
  ylab("Number of Unique Client Served")


compUniqClient %>%
  filter(HealthAuthority == "NHA") %>%
  filter(Value > 1000) %>%
  View()


comp %>%
  filter(grepl("bed", IndicatorTicker))

compLong$IndicatorTicker

timeSeries <- getQuarterList()


# new way

filePath <- paste0("data//PHSA//",fileName[[5]])
tab <- tabName[[4]]

df <- readExceltab(tab, filePath)


spNameIndex <- which(isTextMatch(df$IndicatorNameInExcel, "indicator") |
                       isTextMatch(df$IndicatorNameInExcel, "yes"))-1
# split table

length(spNameIndex)
orgnizeSingleSP(df)

comp <- lapply(c(2:length(spNameIndex)),
               function(x, y) orgnizeSingleSP(y[spNameIndex[x-1]:(spNameIndex[x]-1), ]), df)

comp <- bind_rows(comp)



comp %>%
  filter(!is.na({{indicatorExcelName}})) %>%
  pivot_longer(cols = all_of(names(comp)[!names(comp) %in% c(indicatorExcelName, spName)]), names_to = "Quarter",
               values_to = "Value")


for(i in seq_along(vec)){

}


df <- readExcelFile(filePath)

df <- readExceltab0("Adult SU Treatment Beds", filePath)

df[, names(df)[!is.na(df[2, ])]]
