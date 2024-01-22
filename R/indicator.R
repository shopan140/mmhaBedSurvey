accessAndUtilization <-list(TotalClientServed = c("Total",
                                                  "client",
                                                  "served"),
                            UniqClientServed = c("unique",
                                                 "client", "served"),
                            NumRepeatedClient = c("repeat",
                                                  "client"),
                            UniqClientIndigenous = c("unique",
                                                     "client",
                                                     "indigenous"),
                            UniqClientFirstNation = c("unique",
                                                      "client",
                                                      "first",
                                                      "nation"),
                            UniqClientInuit = c("unique",
                                                "client",
                                                "inuit"),
                            UniqClinetMetis = c("unique",
                                                "client",
                                                paste0("M", "\u00E9", "tis")),
                            TotalBedDay = c("Total",
                                            "Bed",
                                            "day"),
                            TotalVacantBedDay = c("Total",
                                                  "vacant",
                                                  "Bed",
                                                  "day"),
                            OccupancyRate = c("Occupancy", "rate"),
                            DiffReferralService = c("days",
                                                    "between",
                                                    "client",
                                                    "referral",
                                                    "service"),
                            DiffReferralWaitlist = c("days",
                                                     "between",
                                                     "client",
                                                     "referral",
                                                     "waitlist"),
                            DiffWaitlistService = c("days",
                                                    "between",
                                                    "waitlist",
                                                    "service"),
                            TotalVisit = c("total",
                                           "visit"),
                            UniqClientMen = c("client",
                                              "unique",
                                              "men"),
                            TotalClientMen = c("client",
                                               "number",
                                               "men"),
                            UniqClientWomen = c("client",
                                                "unique",
                                                "women"),
                            TotalClientWomen = c("client",
                                                 "number",
                                                 "women"),
                            UniqClientNonbinary = c("client",
                                                    "unique",
                                                    "non-binary"),
                            TotalClientNonbinary = c("client",
                                                     "number",
                                                     "non-binary"),
                            ServiceLocation = c("service",
                                                "location"),
                            ServiceLocation = c("service",
                                                "loation"),
                            TotalParticipants = c("total",
                                                  "participant"),
                            TotalCaseLoadCapacity = c("Total",
                                                      "case",
                                                      "capacity"),
                            CaseLoadCapacityUtilRate = c("caseload",
                                                         "capacity"),
                            HeldBedDays = c("held",
                                            "bed",
                                            "day"),
                            TreatmentBedDays = c("treatment",
                                                 "bed",
                                                 "day"),
                            PostTreatmentBedDays = c("Post",
                                                     "treatment",
                                                     "bed",
                                                     "day"),
                            PercentClientOnOAT = c("percent",
                                                   "client",
                                                   "OAT"),
                            PercentClientRetainedService = c("Percentage",
                                                             "client",
                                                             "retained",
                                                             "service"),
                            PercentClientRetainedService = c("%",
                                                             "client",
                                                             "retained",
                                                             "service"),
                            PercentClientTransitionToCommunitySupport = c("Percentage",
                                                                          "client",
                                                                          "community",
                                                                          "transition",
                                                                          "service"),
                            PercentClientTransitionToCommunitySupport = c("%",
                                                                          "client",
                                                                          "community",
                                                                          "transition",
                                                                          "service"),
                            PercentClientTransitionToCommunitySupport = c("%",
                                                                          "client",
                                                                          "community",
                                                                          "successfully",
                                                                          "service"),
                            PercentClientTransitionToHousingSupport = c("Percentage",
                                                                        "client",
                                                                        "housing",
                                                                        "transition",
                                                                        "service"),
                            PercentClientTransitionToHousingSupport = c("%",
                                                                        "client",
                                                                        "housing",
                                                                        "transition",
                                                                        "service"),
                            PercentStaffWithSafetyTraining = c("Percentage",
                                                               "Program",
                                                               "staff",
                                                               "training"),
                            NumClientVisit = c("client",
                                               "visit"),
                            NumClientStartingProgram = c("client",
                                                         "starting",
                                                         "program"),
                            NumClientTurnAwayForLowCapacity = c("client",
                                                                "turned",
                                                                "away",
                                                                "capacity"),
                            NumContractorReceivedEducSupport =  c("contractor",
                                                                  "received",
                                                                  "support"),
                            NumContractorReceivedAppScreenSuport = c("contractor",
                                                                     "received",
                                                                     "support",
                                                                     "screening"),
                            NumNewPositionHiredSupportImplementation = c("new",
                                                                         "position",
                                                                         "hired",
                                                                         "support",
                                                                         "implementation"),
                            NumSupportiveRecoveryOp = c("supportive",
                                                        "recovery",
                                                        "operator"),
                            PercentClientStartingLate = c("%",
                                                          "client",
                                                          "attending",
                                                          "midpoint"),
                            PercentClientConnectMHSU = c("client",
                                                         "connected",
                                                         "mhsu"),
                            PercentClientReceivingSUService = c("%",
                                                                "client",
                                                                "receiving",
                                                                "substance",
                                                                "service"),
                            NumAftercareCounsellorHired = c("aftercare",
                                                            "counsellor",
                                                            "hired"),
                            PercentClientRetainedService = c("Percentage",
                                                             "client",
                                                             "continuing",
                                                             "service"))



checkUniqueIndicators <- function(df, matchKey = "indicator", off = -1){
  vec <- df[, 1, drop = TRUE]
  dd <- df[-findItems(matchKey, vec, offset = -1), ]

  dd <- dd[!duplicated(dd[, 1, drop = TRUE]), ]
  dd[, indicatorShortName] <- dd[, 1]
  dd[, 1] <- NULL

  dd[order(dd[, indicatorShortName, drop = TRUE]), c(haShortName, svcType, indicatorShortName)]
}


groupByMatchingText <- function(indicatorDf, textVec){

  # for (i in seq_along(textVec)){
  #   indicatorDf <- indicatorDf[grepl(tolower(paste0("\\b", textVec[[i]])),
  #              tolower(indicatorDf[, indicatorShortName, drop = TRUE])), ]
  # }
  items <- isTextMatch(indicatorDf[, indicatorShortName, drop = TRUE],
                       textVec)
  indicatorDf[items, ]
}


isTextMatch <- function(indicatorList, textVec){
  output <- rep(TRUE, length(indicatorList))
  for (i in seq_along(textVec)){
    temp <- grepl(tolower(paste0("\\b", textVec[[i]])),
                                     tolower(indicatorList))
    output <- output & temp
  }
  output
}

makeTicker <- function(indicatorDf, textList){
  # check indicator column exists
  if (!indicatorTicker %in% names(indicatorDf)){
    indicatorDf[, indicatorTicker] <- NA
  }
  for(i in seq_along(textList)){
    items <- isTextMatch(indicatorDf[, indicatorExcelName, drop = TRUE], textList[[i]])
    if(names(textList[i]) != ""){
      tickerName <- names(textList[i])

    }
    else{
      tickerName <- "NoNameSpecified"
    }


    for( i in seq_along(items)){
      if(items[[i]]){
        indicatorDf[i, indicatorTicker] <- tickerName
      }
    }

  }

  indicatorDf
}
