plot_JT <- function(data = JTRCIdf, 
                     classcountsinlegend = T, 
                     useGroups = F, 
                     facetplot = F, 
                     addJitter = T, 
                     xlab = "pre", 
                     ylab = "post", 
                     plottitle = "Jacobson-Truax plot") {
  
  if(!exists("JTRCIdf")) {
    stop("\nto plot JT indices, first run the JTRCI() function with argument 'indextype = \"JT\"'", call. = F)}
  
  if(is.null(JTRCIdf$class_JTRCI)) {
    stop("\nto plot JT indices, first run the JTRCI() function with argument 'indextype = \"JT\"'", call. = F)}
  
  if(facetplot == T) { useGroups = T}
  
  if(useGroups == T & is.null(JTRCIdf$group)) {
    stop("\nto plot by group, first run the JTRCI() function with argument 'group =' set to the variable indicating group membership", call. = F)}
  
  JTRCIdf$classPlot <- JTRCIdf$class_JTRCI
  critval <- JTRCIdf$critval[1]
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
  cols <- c("recovered" = "orchid3","non reliably recovered" = "palevioletred3","improved" = "olivedrab3","unchanged" = "skyblue3","deteriorated" = "coral1")
  
  names(cols) <- c(
    levels(JTRCIdf$classPlot)[grep("^recovered", levels(JTRCIdf$classPlot))],
    levels(JTRCIdf$classPlot)[grep("non reliably", levels(JTRCIdf$classPlot))],
    levels(JTRCIdf$classPlot)[grep("improved", levels(JTRCIdf$classPlot))],
    levels(JTRCIdf$classPlot)[grep("unchanged", levels(JTRCIdf$classPlot))],
    levels(JTRCIdf$classPlot)[grep("deteriorated",  levels(JTRCIdf$classPlot))]
  )
  
  datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
  plotrange <- c( c(min(c(JTRCIdf$pre, JTRCIdf$post))) - .1*datrangelength, c(max(c(JTRCIdf$pre, JTRCIdf$post))) + .1*datrangelength)
  plotrangex <- plotrange
  plotrangey <- plotrange
  
  if(critval < plotrange[1]) {plotrangey[1] <- critval}
  if(critval > plotrange[2]) {plotrangey[2] <- critval}
  
  library(ggplot2)
  
  if(facetplot == F)  {groupshapes = T
  groupfacets = F}
  if(facetplot == T)  {groupfacets = T
  groupshapes = F} 
  if(!useGroups)      {groupshapes = F 
  groupfacets = F}
  
  if(addJitter) {
    jitter <- position_jitter(width = 0.005 * (plotrange[2]- plotrange[1]), height = 0.005 * (plotrange[2]- plotrange[1]))}
  else { jitter <- position_jitter(width = 0, height = 0)}
  
  # a dataframe to define the lines with - this way we can add a legend for the lines to the plot:  
  linesdf <- data.frame(lineID = c("no change", "reliable change boundary", "reliable change boundary", paste0("JT ", JTRCIdf$crittype[1], ": ", round(critval,1))), 
                        intercept = c(0, 1.96 * Sdiff, -1.96 * Sdiff, critval),
                        slope = c(1, 1, 1, 0))
  
  linesdf$lineID <- factor(linesdf$lineID, levels = c ("no change", "reliable change boundary", paste0("JT ", JTRCIdf$crittype[1], ": ", round(critval,1))))
  
  
  JT_plot <- ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)), ], 
                                   aes(x = pre, y = post, colour = classPlot)) +
    geom_abline(data= linesdf, mapping=aes(slope=slope, intercept=intercept, linetype= factor(lineID)), col = "gray45") +
    {if(groupfacets) facet_wrap(.~ group , if(length(unique(JTRCIdf$group)) > 3) {2} else{1}) } +
    geom_point(size = 2.5, position = jitter) +
    {if(!groupshapes)geom_point(size = 2.5, shape = 1, colour = "gray30",  position = jitter) } +
    {if(groupshapes) aes(shape = group)} +
    {if(groupshapes) geom_point(size = 2.5, position = jitter, aes(fill = classPlot))} +
    {if(groupshapes) geom_point(size = 2.5, colour = "gray30",  position = jitter)} +
    theme_classic() +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          legend.background = element_blank(),
          panel.grid.major = element_line(color = "gray95"), 
          strip.background = element_blank()) +        
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values=c(21: 25)) +
    scale_linetype_manual(values=c(1,3,2)) +
    guides(colour = guide_legend(order = 1), 
           shape = guide_legend(order = 2), fill = F, 
           alpha = F, linetype = guide_legend(order = 3)) + 
    xlim(plotrangex) + 
    ylim(plotrangey) +
    labs(
      title = plottitle,
      x = xlab,
      y = ylab,
      colour = if(classcountsinlegend & (useGroups)) { paste0("Jacobson-Truax classification \n ", paste(levels(JTRCIdf$group), collapse = "/ "), ":") } 
      else  { ("Jacobson-Truax  \n classification")},
      linetype = element_blank(),
      shape = element_blank()
    )
  
 
  print(JT_plot)
  
}

