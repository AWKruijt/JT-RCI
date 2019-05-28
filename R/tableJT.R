#' display a table summarizing Jacobson-Truax indices
#'
#' This function creates a table summarzing the number of observations for each label in the Jacobson-Truax classification. It uses data stored in the JTRCIdf dataframe that is outputted by the JTRCI() function.
#' @param data The data to use, default is JTRCIdf. 
#' @param useGroups if true, counts are given by each level of the group variable originally provided in the call to JTRCI()
#' @return a table summarizing counts per classification label.
#' @examples
#' tableJT(useGroups = F)

tableJT <- function(data = JTRCIdf, useGroups = NA) {
  
  require(data.table)
  
  JTRCIdf <- data.table(JTRCIdf)
  
  if(useGroups) {
  
  tableList <- NULL
  
    for(l in 1: length(levels(JTRCIdf$group))) { 
      
      DT <- as.data.table(JTRCIdf[group == levels(JTRCIdf$group)[l], table("Jacobson-Truax classification" = class_JTRCI)])
      
      setnames(DT, "N", levels(JTRCIdf$group)[l])
      
      tableList <- c(tableList, DT)
    }
    

  JTtable <- as.data.table(do.call(cbind, tableList)[,c(1, seq(2,length(tableList), 2))])  
} else {
    
  JTtable <-as.data.table(JTRCIdf[, table("Jacobson-Truax classification" = class_JTRCI)])
}
  print(JTtable)
}
  