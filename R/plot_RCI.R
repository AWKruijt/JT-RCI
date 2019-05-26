plot_RCI <- function(data = JTRCIdf, 
                     classcountsinlegend = T, 
                     useGroups = F, 
                     facetplot = F, 
                     addJitter = T, 
                     xlab = "pre", 
                     ylab = "post", 
                     plottitle = "reliable change plot") {
  
  if(!exists("JTRCIdf")) {
    stop("\nto plot RCI indices, first run the JTRCI() function with argument 'indextype = \"RCI\"'", call. = F)}
  
  if(is.null(JTRCIdf$class_RCI)) {
    stop("\nto plot RCI indices, first run the JTRCI() function with argument 'indextype = \"RCI\"'", call. = F)}
  
  if(facetplot == T) { useGroups = T}
  
  if(useGroups == T & is.null(JTRCIdf$group)) {
    stop("\nto plot by group, first run the JTRCI() function with argument 'group =' set to the variable indicating group membership", call. = F)}
  

  
  JTRCIdf$classPlot <- JTRCIdf$class_RCI
  Sdiff <- JTRCIdf$Sdiff[1]
  
  if(classcountsinlegend) {
    if(useGroups){
      
      classtab <- table(JTRCIdf$classPlot, JTRCIdf$group)
      
      for (l in 1:nrow(classtab)) {
        levels(JTRCIdf$classPlot)[l] <- paste0(levels(JTRCIdf$classPlot)[l], ": ", paste0(classtab[[l, 1]] ))
        
        if(ncol(classtab) > 1) {
          for (g in 2:ncol(classtab)) {
            levels(JTRCIdf$classPlot)[l] <-
              paste0(levels(JTRCIdf$classPlot)[l], " / ", paste0(classtab[[l, g]] ))
          }}
      }
    }
    
    if(!useGroups) {
      classtab <- table(JTRCIdf$classPlot)
      
      for (l in 1:nrow(classtab)) {
        levels(JTRCIdf$classPlot)[l] <- paste0(levels(JTRCIdf$classPlot)[l], ": ", paste0(classtab[[l]] ))
        
      }}
  }
  
  # created vector of plot colours linked to specific level names:
  cols <- c( "reliably deteriorated" = "coral1", "no reliable change" = "skyblue3", "reliably improved" = "orchid3")
  
  names(cols) <-
    c(levels(JTRCIdf$classPlot)[grep("reliably deteriorated", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("no reliable change", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("reliably improved", levels(JTRCIdf$classPlot))])
  
  datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
  plotrange <- c( c(min(c(JTRCIdf$pre, JTRCIdf$post))) - .1*datrangelength, c(max(c(JTRCIdf$pre, JTRCIdf$post))) + .1*datrangelength)
  
  require(ggplot2)

  if(facetplot == F)  {groupshapes = T
  groupfacets = F}
  
  if(facetplot == T)  {groupshapes = F
  groupfacets = T} 
  
  if(!useGroups)      {groupshapes = F 
  groupfacets = F}
  
  if(addJitter) {
    jitter <- position_jitter(width = 0.005 * (plotrange[2]- plotrange[1]), height = 0.005 * (plotrange[2]- plotrange[1]))}  else 
      { jitter <- position_jitter(width = 0, height = 0)}
  
  # a dataframe to define the lines with - this way we can add a legend for the lines to the plot:  

  linesdf <- data.frame(lineID = c("no change", "+/- 1.96 Sdiff", "+/- 1.96 Sdiff"), 
                        intercept = c(0, 1.96 * Sdiff, -1.96 * Sdiff),
                        slope = c(1, 1, 1))
  
  linesdf$lineID <- factor(linesdf$lineID, levels = c ("no change", "+/- 1.96 Sdiff"))
  
  RCI_plot <- ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)), ], aes(x = pre, y = post, colour = classPlot)) +
    geom_abline(data= linesdf, mapping=aes(slope=slope, intercept=intercept, linetype= factor(lineID)), col = "gray50") +
    {if(groupfacets) facet_wrap(.~ group , if(length(unique(JTRCIdf$group)) > 3) {2} else {1}) } +
    geom_point(size = 2.5, position = jitter) +
    {if(!groupshapes)geom_point(size = 2.5, shape = 1, colour = "gray30", position = jitter) } +
    {if(groupshapes) aes(shape = group)} +
    {if(groupshapes) geom_point(size = 2.5, position = jitter, aes(fill = classPlot))} +
    {if(groupshapes) geom_point(size = 2.5, colour = "gray30", position = jitter)} +
    theme_classic() +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          legend.background = element_blank(),
          panel.grid.major = element_line(color = "gray95"), 
          strip.background = element_blank()) +        
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values=c(21: 25)) +
    scale_linetype_manual(values=c(1,3,3)) +
    guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2), fill = F, alpha = F) + 
    xlim(plotrange) + 
    ylim(plotrange) +
    labs(
      title = plottitle,
      x = xlab,
      y = ylab,
      colour = if(classcountsinlegend & (useGroups)) { paste0("reliable change classification \n ", paste(levels(JTRCIdf$group), collapse = "/ "), ":") } 
      else { ("reliable change  \n classification")},
      linetype = element_blank()
    )
  
  print(RCI_plot)

}

