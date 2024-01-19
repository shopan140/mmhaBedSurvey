
makeServiceTypeTable <- function(vec = c("Treatment" = "Adult Substance Use Treatment Beds",
                                         "Supportive Recovery" = "Adult Substance Use Supportive Recovery ",
                                         "Withdrawal Mgmt" = "Adult Substance Use Withdrawal Mgmt. Beds",
                                         "random" = "random sheet",
                                         "Budget" = "Budget 2021",
                                         "Stabilization" = ""),
                                 columnNames = c(svcType, svcTypeDes)){
  makeTable(vec = vec, columnNames = columnNames, idColName = idCol)
}

attachTabToServiceType <- function(serviceTypeTable,
                                   tabList,
                                   colToLook = "ServiceType",
                                   colToAdd = "TabName"){


  attachcolForValue(serviceTypeTable, tabList, colToLook, colToAdd)
}


