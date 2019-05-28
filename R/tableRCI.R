#' display a table summarizing reliable change indices
#'
#' This function creates a table summarzing the number of observations for each label in the reliable change classification. It uses data stored in the JTRCIdf dataframe that is outputted by the JTRCI() function.
#' @param data The data to use, default is JTRCIdf. 
#' @param useGroups if true, counts are given by each level of the group variable originally provided in the call to JTRCI()
#' @return a table summarizing counts per classification label.
#' @examples
#' tableRCI(useGroups = F)
#' #' @import data.table
#' @import ggplot2
#' @export


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
