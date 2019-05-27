tableRCI <- function(data = JTRCIdf, useGroups = NA) {
  
  require(data.table)
  
  JTRCIdf <- data.table(JTRCIdf)

  if(useGroups) {

  tableList <- NULL
  
  for(l in 1: length(levels(JTRCIdf$group))) { 
    
    DT <- as.data.table(JTRCIdf[group == levels(JTRCIdf$group)[l], table("reliable change classification" = class_RCI)])
    
    setnames(DT, "N", levels(JTRCIdf$group)[l])
    
    tableList <- c(tableList, DT)
  }
  
  
  RCItable <- as.data.table(do.call(cbind, tableList)[,c(1, seq(2,length(tableList), 2))])  
 
  } else {
    
    RCItable <-as.data.table(JTRCIdf[, table("reliable change classification" = class_RCI)])
  }
  
  print(RCItable)
}
