# @import data.table
# @import ggplot2

## this code relies heavily on the excellent article by Stephanie Bauer, Michael Lambert, & Steven Lars Nielsen:
## Clinical Significance Methods: A Comparison of Statistical Techniques
## J Pers Assess. 2004 Feb;82(1):60-70. DOI:10.1207/s15327752jpa8201_11
## http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.615.8373&rep=rep1&type=pdf

## and of course on:
## Clinical Significance: a Statistical Approach to Defining Meaningful Change in Psychotherapy Research
## by Neil Jacobson & Paula Truax 
## in Journal of Consulting  and Clinical Psycholoy 1991 
## https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049

JTRCI <- function(x.pre = NA,
                   x.post = NA,
                   reliability = NA,
                   ppid = NA,
                   group = NA,
                   indextype = "JT",
                   plot = T,
                   table = T,
                   higherIsBetter = F,
                   JTcrit = "auto",
                   normM = NA,
                   normSD = NA, 
                   facetplot = F) {

  ## check inputs:    
  if (!indextype %in% c("JT", "RCI")) {
    stop('\nindextype must be either "JT" or "RCI"', call. = FALSE)}
  
  if (all(is.na(x.pre))) {
    stop('\nprovide pre data in x.pre', call. = FALSE)}
  
  if (all(is.na(x.post))) {
    stop('\nprovide post data in x.post', call. = FALSE)}
  
  if (length(x.post) != length(x.pre)) {
    stop("\nx.pre and x.post differ in length")}
  
  if (!is.na(ppid) & (length(ppid) != length(x.pre))) {
    stop("\nlength of ppid does not match length of data")}
  
  if (is.na(ppid)) {
    ppid <- seq(1:length(x.pre))
    warning("\nNo participant IDs provided - substituting with values 1:n")}
  
  if (is.na(reliability)) {
    stop('\nprovide reliability estimate in reliability = ', call. = FALSE)}
  
  if (!higherIsBetter) {
    warning("\nAssumed that lower scores are better (and reduction == improvement), if that is incorrect: set higherIsBetter = T")
  }
  if (higherIsBetter) {
    warning("\nAssumed that higher scores are better (and increased scores == improvement), if that is incorrect: set higherIsBetter = F")
  }
  
  if (any(!is.na(group))) {
    useGroups = T
  } else{ useGroups = F}
  
  ## determine SEmeasurement and Sdiff for computation of reliable change index:
  
  SEmeasurement <- sd(x.pre) * sqrt(1 - reliability) # standard error of measurement based on the sd of the pre measurement/baseline
  
  Sdiff <- sqrt(2 * SEmeasurement ^ 2) # the SD of the SEm for a difference score
  
  
  ## determine cut-off critera: 
  
  if (indextype == "JT") {
    # determine the criterion to determine recovery:
    
    ### the criterion to use is determined by the user or by the following rules ifJTcrit == "auto" or norm data is missing: 
    # When pre and norm data overlap: C is preferred, if they do not overlap: B is preferred, if no norm-data is available: A has to be used. 
    # also see original Jacobson & Truax (1991) article - https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049 
    
    if (is.na(normM) | is.na(normSD) |JTcrit == "A")  {
      # if no norm data is available, only crit A is  possible, also compute crit A if requested
      
      crittype <- "crit A"
      
      # if we're here but the B or C crit was requested, either one or both of the normdata params is not provided - generate a warning:
      if (JTcrit == "B" |JTcrit == "C") {
        warning(paste0( "\ncomputing criterion A - requested JTcrit (",JTcrit, ") requires norm mean and SD"))
      }
      
    } # end of if (is.na(normM) | is.na(normSD) | JTcrit == "A")
    
    else {
      # if norm data is availabe:
      
      # first check if the norm and pre-distributions overlap: 
      if (all(c(mean(x.pre) - (3 * sd(x.pre)), mean(x.pre) + (3 * sd(x.pre))) < normM - (3 * normSD)) |
          all(c(mean(x.pre) - (3 * sd(x.pre)), mean(x.pre) + (3 * sd(x.pre))) > normM + (3 * normSD))) {
        
        # if the pre/clinical sample overlaps with the norm sample, criterion C is recommended:
        if (JTcrit == "auto" |JTcrit == "C") {
          crittype <- "crit C"}
        
        # compute B if explicity requested but add a warning message:
        if (JTcrit == "B") {
          crittype <- "crit B"
          
          warning("\nNB criterion C is recommended when the baseline distribution overlaps with the norm distribution")
        }
      } # end of 'if distributions overlap' section
      
      else {
        # if the pre/clinical sample does not overlap with the norm sample, criterion B is preferred (according to Jacobson & Truax on page 15: https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049)
        
        if (JTcrit == "auto" |JTcrit == "B") {
          crittype <- "crit B"}
        
        # compute C if explicity requested but add a warning message:
        if (JTcrit == "C") {
          crittype <- "crit C"
          
          warning( "\nNB criterion B is recommended when the baseline distribution does not overlap with the norm distribution")}
      } # end of if no overlap section
    } # end of 'if norm data is available' section
    
    
    ## compute the determined crit type: 
    
    if(crittype == "crit A") {
      # The level of functioning at post should fall outside the range of the baseline population
      # i.e. more than 1.96 standard deviation in the 'more healthy' direction - dependent on 'higherIsBetter':
      if (!higherIsBetter) {
        critval <- mean(x.pre) - 1.96 * sd(x.pre)}
      
      if (higherIsBetter) {
        critval <- mean(x.pre) + 1.96 * sd(x.pre)}
      
      warning(paste0("\nJacobson-Truax criterion A: ", round(critval, 1)))
    }
    
    if(crittype == "crit B") {
      # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data - direction dependent on 'higherisbetter'
      if (!higherIsBetter) {
        critval <- normM + 1.96 * normSD}
      
      if (higherIsBetter) {
        critval <- normM - 1.96 * normSD}
      
      warning(paste0("\nJacobson-Truax criterion B: ", round(critval, 1)))
    }
    
    if(crittype == "crit C") {
      # The level of functioning at post should place the patient closer to the mean of the comparison norm data than the mean of the clinical norm data / pre measurement - weighted by SD of both clin and norm
      
      critval <- ((sd(x.pre) * normM) + (normSD * mean(x.pre))) / (sd(x.pre) + normSD)
      
      warning(paste0("\nJacobson-Truax criterion C: ", round(critval, 1)))
    }
  } # end of if (indextype == "JT")
  
  if (indextype == "RCI") {
    crittype <- "non JT simple RCI"
    critval <- "none"
  }
  
  ## create a JTRCIdf dataframe and log things:
  
  JTRCIdf <- NULL
  JTRCIdf$ppid <- ppid
  JTRCIdf <- as.data.frame(JTRCIdf)
  if(useGroups) {JTRCIdf$group <- as.factor(group)}
  JTRCIdf$pre <- x.pre
  JTRCIdf$post <- x.post
  
  JTRCIdf$change_abs <- JTRCIdf$post - JTRCIdf$pre
  
  # write values to JTRCIdf:
  JTRCIdf$SEmeasurement <- SEmeasurement
  JTRCIdf$Sdiff <- Sdiff
  JTRCIdf$crittype <- crittype
  JTRCIdf$critval <- critval
  
  ## compute Reliable Change Indices:
  JTRCIdf$change_Sdiff <- JTRCIdf$change_abs / Sdiff
  
  ## check and warn if there are participants who were already 'recovered' at pre - their classification will be odd: 
  if (indextype == "JT") {
    
    if ( (!higherIsBetter & sum(x.pre < critval) > 0) | (higherIsBetter & sum(x.pre > critval) > 0) ) {
      warning(paste0( "\n", sum(x.pre < critval),  " participants scored below the Jacobson-Truax cut-off score at the pre-measurement - interpret Jacobson-Truax classification with caution and consider assessing 'simple' reliable change (by setting indextype = 'RCI')" ) ) }
    
    if (!higherIsBetter) {
      # determine Jacobson-Truax classification (note that these next lines only 'work' if the entire series is run in the correct order):
      JTRCIdf$class_JTRCI <- NA
      JTRCIdf$class_JTRCI [JTRCIdf$post <= critval & JTRCIdf$change_Sdiff <= -1.96] <- "recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post <= critval & JTRCIdf$change_Sdiff > -1.96]  <- "non reliably recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post > critval & JTRCIdf$change_Sdiff <= -1.96]  <- "improved"
      JTRCIdf$class_JTRCI [JTRCIdf$post > critval & JTRCIdf$change_Sdiff > -1.96]   <- "unchanged"
      JTRCIdf$class_JTRCI [JTRCIdf$change_Sdiff >= 1.96]                         <- "deteriorated"
    }
    
    if (higherIsBetter) {
      # determine Jacobson-Truax classification (note that these next lines only 'work' if the entire series is run in the correct order):
      JTRCIdf$class_JTRCI <- NA
      JTRCIdf$class_JTRCI [JTRCIdf$post >= critval & JTRCIdf$change_Sdiff >= 1.96] <- "recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post >= critval & JTRCIdf$change_Sdiff < 1.96]  <- "non reliably recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post < critval & JTRCIdf$change_Sdiff >= 1.96]  <- "improved"
      JTRCIdf$class_JTRCI [JTRCIdf$post < critval & JTRCIdf$change_Sdiff < 1.96]   <- "unchanged"
      JTRCIdf$class_JTRCI [JTRCIdf$change_Sdiff <= -1.96]                       <- "deteriorated"
    }
    
    
    #     JTRCIdf$class_JTRCI <- factor(JTRCIdf$class_JTRCI, levels = rev(  c("recovered", "non # reliably recovered", "improved", "unchanged", "deteriorated") ))
    
    JTRCIdf$class_JTRCI <- factor(JTRCIdf$class_JTRCI, 
                                  levels = rev(c("recovered",
                                                 "non reliably recovered",
                                                 "improved",
                                                 "unchanged",
                                                 "deteriorated") ))
    
    
    # pass dataframe to environment
    JTRCIdf <<- JTRCIdf
    
    ## output table if requested: 
    if (table) {
      JTtable <- data.table::setDT(JTRCIdf)[, .N, by = .("Jacobson Truax classification" = class_JTRCI)]
      JTtable <- data.table::setorder(JTtable, na.last = T)
      print(JTtable)
    }
  }
  
  if (indextype == "RCI") {
    # determine RCI classification:
    
    if (higherIsBetter == F) {
      JTRCIdf$class_RCI <- "no reliable change"
      JTRCIdf$class_RCI [JTRCIdf$change_Sdiff > 1.96]  <- "reliably deteriorated"
      JTRCIdf$class_RCI [JTRCIdf$change_Sdiff < -1.96] <- "reliably improved"
    }
    
    if (higherIsBetter == T) {
      JTRCIdf$class_RCI <- "no reliable change"
      JTRCIdf$class_RCI [JTRCIdf$change_Sdiff > 1.96]  <- "reliably improved"
      JTRCIdf$class_RCI [JTRCIdf$change_Sdiff < -1.96] <- "reliably deteriorated"
    }
    
    JTRCIdf$class_RCI <- factor(JTRCIdf$class_RCI, levels = c("reliably deteriorated",
                                                              "no reliable change",
                                                              "reliably improved"))
    
    
    # pass dataframe to environment
    JTRCIdf <<- JTRCIdf
    
    ## output table if requested: 
    if (table) { 
      
      if(useGroups) {
        
        RCItable <- htmlTable::htmlTable(table(JTRCIdf$class_RCI, JTRCIdf$group))
        
        #RCItable <- kable(table(JTRCIdf$class_RCI, JTRCIdf$group))
        print(RCItable)
      }
      else {
        RCItable <-  data.table::setDT(JTRCIdf)[, .N, by = .("reliable change classification" = class_RCI)]
        RCItable <- data.table::setorder(RCItable, na.last = T)
        print(RCItable)
      }
    }
  }
  
  
  ## plot JTRCI or RCI if requested:
  
  if (plot == T & indextype == "JT") {
    
    # add a variable classplot to the dataframe and add to each level the number of cases for each level:
    JTRCIdf$classPlot <- JTRCIdf$class_JTRCI
    
    for (l in 1:length(levels(JTRCIdf$classPlot))) {
      levels(JTRCIdf$classPlot)[l] <-  paste0(levels(JTRCIdf$classPlot)[l],": ", table(JTRCIdf$classPlot)[[l]])
    }
    
    
    # created vector of plot colours linked to specific level names:
    cols <- c(
      "recovered" = "orchid3",
      "non reliably recovered" = "palevioletred3",
      "improved" = "olivedrab3",
      "unchanged" = "skyblue3",
      "deteriorated" = "coral1"
    )
    
    # and substitute its level names with the level names with added case counts
    names(cols) <- c(
      levels(JTRCIdf$classPlot)[grep("^recovered", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("non reliably", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("improved", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("unchanged", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("deteriorated",  levels(JTRCIdf$classPlot))]
    )
    
    # determine the datarange in order to adjust the axis range:
    datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
    plotrange <- c(c(min(c( JTRCIdf$pre, JTRCIdf$post))) - .1 * datrangelength, c(max(c(JTRCIdf$pre, JTRCIdf$post))) + .1 * datrangelength)
    
    require(ggplot2)

    if(facetplot == F)  {
      groupshapes = T
      groupfacets = F}
    if(facetplot == T)  {
      groupfacets = T
      groupshapes = F} 
    if(!useGroups)      {
      groupshapes = F 
      groupfacets = F}

    JTRCIplot <- ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)), ], aes(x = pre, y = post, colour = classPlot)) +
      {if(groupfacets) facet_wrap(.~ group , if(length(unique(JTRCIdf$group)) > 3) {2} else{1}) } +
      geom_point(size = 2.5) +
      {if(!groupshapes)geom_point(size = 2.5, shape = 1, colour = "gray30") } +
      {if(groupshapes) aes(shape = group)} +
      {if(groupshapes) geom_point(size = 2.5, aes(fill = classPlot))} +
      {if(groupshapes) geom_point(size = 2.5, colour = "gray30")} +
      geom_abline(intercept = 0, slope = 1) +
      geom_abline(intercept = Sdiff * 1.96, slope = 1, linetype = 3) +
      geom_abline(intercept = Sdiff * -1.96 , slope = 1, linetype = 3) +
      geom_hline(yintercept = critval, linetype = 2) +
      theme_classic() +
      theme(panel.grid.major = element_line(color = "gray95"), 
            strip.background = element_blank()) +        
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      scale_shape_manual(values=c(21: 25)) +
      guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2), fill = F, alpha = F) + 
      xlim(plotrange) + 
      ylim(plotrange) +
      labs(
        title = "reliable change plot",
        x = "pre",
        y = "post",
        colour = "reliable change \n classification:"
      )
    
    return(JTRCIplot)
  }
  
  if (plot == T & indextype == "RCI") {
    
    # add a variable classplot to the dataframe and add to each level the number of cases for each level:
    JTRCIdf$classPlot <- as.factor(JTRCIdf$class_RCI)
    
    for (l in 1:length(levels(JTRCIdf$classPlot))) {
      levels(JTRCIdf$classPlot)[l] <-  paste0(levels(JTRCIdf$classPlot)[l], ": ", table(JTRCIdf$classPlot)[[l]])
    }
    
    # created vector of plot colours linked to specific level names:
    cols <- c(
      "reliably deteriorated" = "coral1",
      "no reliable change" = "skyblue3",
      "reliably improved" = "orchid3"
    )
    
    # and substitute its level names with the level names with added case counts
    names(cols) <- c(
      levels(JTRCIdf$classPlot)[grep("reliably deteriorated", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("no reliable change", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("reliably improved", levels(JTRCIdf$classPlot))]
    )
    
    # determine the datarange in order to adjust the axis range:
    datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
    plotrange <- c(c(min(c( JTRCIdf$pre, JTRCIdf$post ))) - .1 * datrangelength, c(max(c( JTRCIdf$pre, JTRCIdf$post))) + .1 * datrangelength)
    
    require(ggplot2)

    if(facetplot == F)  {
      groupshapes = T
      groupfacets = F}
    if(facetplot == T)  {
      groupfacets = T
      groupshapes = F} 
    if(!useGroups)      {
      groupshapes = F 
      groupfacets = F}
    
    JTRCIplot <-
      ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)), ], aes(x = pre, y = post, colour = classPlot)) +
      {if(groupfacets) facet_wrap(.~ group , if(length(unique(JTRCIdf$group)) > 3) {2} else{1}) } +
      geom_point(size = 2.5) +
      {if(!groupshapes)geom_point(size = 2.5, shape = 1, colour = "gray30") } +
      {if(groupshapes) aes(shape = group)} +
      {if(groupshapes) geom_point(size = 2.5, aes(fill = classPlot))} +
      {if(groupshapes) geom_point(size = 2.5, colour = "gray30")} +
      geom_abline(intercept = 0, slope = 1) +
      geom_abline(intercept = Sdiff * 1.96, slope = 1, linetype = 3) +
      geom_abline(intercept = Sdiff * -1.96 , slope = 1, linetype = 3) +
      theme_classic() +
      theme(panel.grid.major = element_line(color = "gray95"), 
            strip.background = element_blank()) +        
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      scale_shape_manual(values=c(21: 25)) +
      guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2), fill = F, alpha = F) + 
      xlim(plotrange) + 
      ylim(plotrange) +
      labs(
        title = "reliable change plot",
        x = "pre",
        y = "post",
        colour = "reliable change \n classification:"
      )
    
    return(JTRCIplot)
    
  }
  
}
