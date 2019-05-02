# @import data.table
# @import ggplot2

## this code relies heavily on the excellent article by Stephanie Bauer, Michael Lambert, & Steven Lars Nielsen:
## Clinical Significance Methods:A Comparison of Statistical Techniques
## J Pers Assess. 2004 Feb;82(1):60-70. DOI:10.1207/s15327752jpa8201_11
## http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.615.8373&rep=rep1&type=pdf

## and of course 
## Clinical Significance:  A Statistical Approach to DenningMeaningful  Change in  Psychotherapy Research
## by Neil Jacobson & Paula Truax 
## in Journal of Consulting  and Clinical Psycholoy 1991 
## https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049

JTRCI <- function(x.pre = NA,
                  x.post = NA,
                  reliability = NA,
                  ppid = NA,
                  type = "JT",
                  plot = T,
                  table = T,
                  higherIsBetter = F,
                  JT.crit = "auto",
                  norm.M = NA,
                  norm.SD = NA) {
  
  ## check inputs:    
  if (!type %in% c("JT", "RCI")) {
    stop('\ntype must be either "JT" or "RCI"', call. = FALSE)
  }
  
  if (all(is.na(x.pre))) {
    stop('\nprovide pre data in x.pre', call. = FALSE)
  }
  
  if (all(is.na(x.post))) {
    stop('\nprovide post data in x.post', call. = FALSE)
  }
  
  if (length(x.post) != length(x.pre)) {
    stop("\nx.pre and x.post differ in length")
  }
  
  if (length(ppid) != length(x.pre)) {
    ppid <- seq(1:length(x.pre))
    warning("\nNo participant IDs provided or length of ppid does not match length of data - substituting with values 1:n")
  }
  
  if (is.na(reliability)) {
    stop('\nprovide reliability estimate in reliability = ', call. = FALSE)
  }
  
  ## determine SEmeasurement and Sdiff for computation of reliable change index:
  
  SEmeasurement <-
    sd(x.pre) * sqrt(1 - reliability) # standard error of measurement based on the sd of the pre measurement/baseline
  
  Sdiff <-
    sqrt(2 * SEmeasurement ^ 2) # the SD of the SEm for a difference score
  
  
  ## determine cut-off critera: 
  
  if (type == "JT") {
    # determine the criterion to determine recovery:
    
    ### the criterion to use is determined by the user or by the following rules if JT.crit == "auto" or norm data is missing: 
    # When pre and norm data overlap: C is preferred, if they do not overlap: B is preferred, if no norm-data is available: A has to be used. 
    # also see original Jacobson & Truax (1991) article - https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049 
    
    if (is.na(norm.M) | is.na(norm.SD) | JT.crit == "A")  {
      # if no norm data is available, only crit A is  possible,
      # also compute crit A if requested through JT.crit
      
      crittype <- "crit A"
      
      # The level of functioning at post should fall outside the range of the baseline population
      # i.e. more than 1.96 standard deviation in the 'more healthy' direction - dependent on 'higherIsBetter':
      if (!higherIsBetter) {
        crit <- mean(x.pre) - 1.96 * sd(x.pre)
      }
      
      if (higherIsBetter) {
        crit <- mean(x.pre) + 1.96 * sd(x.pre)
      }
      
      warning(paste0("\nJacobson-Truax criterion A: ", round(crit, 1)))
      warning(paste0("\nfor crit A, 'recovered' means: 'post-score falls outside distribtution of the sample at baseline'") )
      
      if (JT.crit == "B" | JT.crit == "C") {
        
        # if we're here but a different crit was requested, either one or both of the normdata params is not provided:
        warning(paste0( "\nrequested JTcrit (", JT.crit, ") requires norm mean and SD"))
      }
    } # end of if (is.na(norm.M) | is.na(norm.SD) | JTcrit == "A")
    
    else {
      # if norm data is availabe:
      
      # first check if the norm and pre-distributions overlap: 
      
      if (all(c(mean(x.pre) - (3 * sd(x.pre)), mean(x.pre) + (3 * sd(x.pre))) < norm.M - (3 * norm.SD)) |
          all(c(mean(x.pre) - (3 * sd(x.pre)), mean(x.pre) + (3 * sd(x.pre))) > norm.M + (3 * norm.SD))) {
        # if the pre/clinical sample overlaps with the norm sample, criterion C is recommended:
        
        if (JT.crit == "auto" | JT.crit == "C") {
          crittype <- "crit C"
          
          # The level of functioning at post should place the patient closer to the mean of the comparison norm data than the mean of the clinical norm data / pre measurement - weighted by SD of both clin and norm
          
          crit <- ((sd(x.pre) * norm.M) + (norm.SD * mean(x.pre))) / (sd(x.pre) + norm.SD)
          warning(paste0("\nJacobson-Truax criterion C: ", round(crit, 1)))}
        
        if (JT.crit == "B") {
          # compute B if explicity requested but add a warning message:
          
          # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data - direction dependent on 'higherisbetter'
          
          if (!higherIsBetter) {
            crit <- norm.M + 1.96 * norm.SD
          }
          
          if (higherIsBetter) {
            crit <-
              norm.M - 1.96 * norm.SD # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data
          }
          
          warning(paste0("\nJacobson-Truax criterion B: ", round(crit, 1)))
          warning("\nNB criterion C is recommended when the baseline distribution overlaps with the norm distribution")
        }
        
      } # end of 'if distributions overlap' section
      
      else {
        # if the pre/clinical sample does not overlap with the norm sample, criterion B is preferred (according to Jacobson & Truax on page 15: https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049)
        
        if (JT.crit == "auto" | JT.crit == "B") {
          crittype <- "crit B"
          
          # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data - direction dependent on 'higherisbetter'
          
          if (!higherIsBetter) {
            crit <- norm.M + 1.96 * norm.SD
          }
          
          if (higherIsBetter) {
            crit <- norm.M - 1.96 * norm.SD
          }
          
          warning("\nJacobson-Truax criterion B: ", round(crit, 1))
        }
        
        if (JT.crit == "C") {
          # compute C if explicity requested but add a warning message:
          crittype <- "crit C"
          
          # The level of functioning at post should place the patient closer to the mean of the comparison norm data than the mean of the clinical norm data / pre measurement - weighted by SD of both clin and norm
          
          crit <-
            ((sd(x.pre) * norm.M) + (norm.SD * mean(x.pre))) / (sd(x.pre) + norm.SD)
          
          
          warning(paste0("\nJacobson-Truax criterion C: ", round(crit, 1)))
          warning( "\nNB criterion B is recommended when the baseline distribution does not overlap with the norm distribution")
        }
        
      }
    } # end of 'if norm data is available' section
  } # end of if (type == "JT")
  
  if (type == "RCI") {
    crittype <- "non JT simple RCI"
    crit <- "none"
  }
  
  ## create a JTRCIdf dataframe and log things:
  
  JTRCIdf <- NULL
  JTRCIdf$ppid <- ppid
  JTRCIdf <- as.data.frame(JTRCIdf)
  JTRCIdf$pre <- x.pre
  JTRCIdf$post <- x.post
  
  JTRCIdf$change <- JTRCIdf$post - JTRCIdf$pre
  
  # write values to JTRCIdf:
  JTRCIdf$SEmeasurement <- SEmeasurement
  JTRCIdf$Sdiff <- Sdiff
  JTRCIdf$crittype <- crittype
  JTRCIdf$crit <- crit
  
  ## compute Reliable Change Indices:
  JTRCIdf$RCI <- JTRCIdf$change / Sdiff
  
  if (type == "JT") {
    
    ## assess if there are participants who were already 'recovered' at pre - their classification will be odd: 
    if (!higherIsBetter & sum(x.pre < crit) > 0) {
      warning(paste0( "\n", sum(x.pre < crit),  " participants scored below the Jacobson-Truax cut-off score at the pre-measurement - interpret Jacobson-Truax classification with caution!" ) ) }
    
    if (higherIsBetter & sum(x.pre > crit) > 0) {
      warning( paste0("\n",sum(x.pre > crit), " participants scored above the Jacobson-Truax cut-off score at the pre-measurement - interpret Jacobson-Truax classification with caution!"))}
    
    
    if (!higherIsBetter) {
      # determine Jacobson-Truax classification (note that these next lines only work if the entire series is run in the correct order):
      JTRCIdf$class_JTRCI <- NA
      JTRCIdf$class_JTRCI [JTRCIdf$post <= crit & JTRCIdf$RCI <= -1.96] <- "recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post <= crit & JTRCIdf$RCI > -1.96]  <- "non reliably recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post > crit & JTRCIdf$RCI <= -1.96]  <- "improved"
      JTRCIdf$class_JTRCI [JTRCIdf$post > crit & JTRCIdf$RCI > -1.96]   <- "unchanged"
      JTRCIdf$class_JTRCI [JTRCIdf$RCI >= 1.96]                         <- "deteriorated"
    }
    
    if (higherIsBetter) {
      # determine Jacobson-Truax classification (note that these next lines only work if the entire series is run in the correct order):
      JTRCIdf$class_JTRCI <- NA
      JTRCIdf$class_JTRCI [JTRCIdf$post >= crit & JTRCIdf$RCI >= 1.96] <- "recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post >= crit & JTRCIdf$RCI < 1.96]  <- "non reliably recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post < crit & JTRCIdf$RCI >= 1.96]  <- "improved"
      JTRCIdf$class_JTRCI [JTRCIdf$post < crit & JTRCIdf$RCI < 1.96]   <- "unchanged"
      JTRCIdf$class_JTRCI [JTRCIdf$RCI <= -1.96]                       <- "deteriorated"
    }
    
    
    JTRCIdf$class_JTRCI <- factor(JTRCIdf$class_JTRCI, levels = rev(  c("recovered", "non reliably recovered", "improved", "unchanged", "deteriorated") ))
    
    # pass dataframe to environment
    JTRCIdf <<- JTRCIdf
    
    ## output table if requested: 
    if (table) {
      JTtable <- data.table::setDT(JTRCIdf)[, .N, by = .("Jacobson Truax classification" = class_JTRCI)]
      JTtable <- data.table::setorder(JTtable, na.last = T)
      print(JTtable)
    }
  }
  
  if (type == "RCI") {
    # determine RCI classification:
    
    if (higherIsBetter == F) {
      JTRCIdf$class_RCI <- "no reliable change"
      JTRCIdf$class_RCI [JTRCIdf$RCI > 1.96]  <- "reliably deteriorated"
      JTRCIdf$class_RCI [JTRCIdf$RCI < -1.96] <- "reliably improved"
    }
    
    if (higherIsBetter == T) {
      JTRCIdf$class_RCI <- "no reliable change"
      JTRCIdf$class_RCI [JTRCIdf$RCI > 1.96]  <- "reliably improved"
      JTRCIdf$class_RCI [JTRCIdf$RCI < -1.96] <- "reliably deteriorated"
    }
    
    # pass dataframe to environment
    JTRCIdf <<- JTRCIdf
    
    ## output table if requested: 
    if (table) { 
      RCItable <-  data.table::setDT(JTRCIdf)[, .N, by = .("reliable change classification" = class_RCI)]
      RCItable <- data.table::setorder(RCItable, na.last = T)
      print(RCItable)
    }
    
  }
  
  if (!higherIsBetter) {
    warning(
      "\nassumed that lower scores are better (and reduction == improvement), if that is incorrect: set higherIsBetter = T"
    )
  }
  if (higherIsBetter) {
    warning(
      "\nassumed that higher scores are better (and increased scores == improvement), if that is incorrect: set higherIsBetter = F"
    )
  }
  
  
  ## plot JTRCI or RCI if requested:
  
  if (plot == T & type == "JT") {
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
    
    names(cols) <- c(
      levels(JTRCIdf$classPlot)[grep("^recovered", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("non reliably", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("improved", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("unchanged", levels(JTRCIdf$classPlot))],
      levels(JTRCIdf$classPlot)[grep("deteriorated",  levels(JTRCIdf$classPlot))]
    )
    
    datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
    plotrange <- c(c(min(c( JTRCIdf$pre, JTRCIdf$post))) - .1 * datrangelength, c(max(c(JTRCIdf$pre, JTRCIdf$post))) + .1 * datrangelength)
    
    library(ggplot2)
    
    JTRCIplot <-
      ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)),], aes(x = pre, y = post, colour = classPlot)) +
      geom_point(size = 2.5) +
      geom_abline(intercept = 0, slope = 1) +
      geom_abline(intercept = Sdiff * 1.96, slope = 1, linetype = 3) +
      geom_abline(intercept = Sdiff * -1.96 , slope = 1,linetype = 3) +
      geom_hline(yintercept = crit, linetype = 2) +
      theme_classic() +
      theme(panel.grid.major = element_line(color = "gray95")) +
      scale_colour_manual(values = cols) +
      xlim(plotrange) +
      ylim(plotrange) +
      labs(
        title = "Jacobson-Truax plot",
        x = "pre",
        y = "post",
        colour = "Jacobson-Truax \n classification:"
      )
    
    return(JTRCIplot)
  }
  
  if (plot == T & type == "RCI") {
    JTRCIdf$classPlot <- JTRCIdf$class_RCI
    
    for (l in 1:length(levels(JTRCIdf$classPlot))) {
      levels(JTRCIdf$classPlot)[l] <-  paste0(levels(JTRCIdf$classPlot)[l], ": ", table(JTRCIdf$classPlot)[[l]])
    }
    
    # created vector of plot colours linked to specific level names:
    cols <- c(
      "reliably deteriorated" = "coral1",
      "no reliable change" = "skyblue3",
      "reliably improved" = "orchid3"
    )
    
    names(cols) <-
      c(
        levels(JTRCIdf$classPlot)[grep("reliably deteriorated", levels(JTRCIdf$classPlot))],
        levels(JTRCIdf$classPlot)[grep("no reliable change", levels(JTRCIdf$classPlot))],
        levels(JTRCIdf$classPlot)[grep("reliably improved", levels(JTRCIdf$classPlot))]
      )
    
    if (sum(is.na(JTRCIdf$post > 0))) { warning(sum(is.na(JTRCIdf$post)),"\ncases have missing post data - these are ommitted from the plot") }
    
    datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
    plotrange <- c(c(min(c( JTRCIdf$pre, JTRCIdf$post ))) - .1 * datrangelength, c(max(c( JTRCIdf$pre, JTRCIdf$post))) + .1 * datrangelength)
    
    JTRCIplot <-
      ggplot2::ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)),], aes(x = pre, y = post, colour = classPlot)) +
      geom_point(size = 2.5) +
      geom_abline(intercept = 0, slope = 1) +
      geom_abline(intercept = Sdiff * 1.96, slope = 1, linetype = 3) +
      geom_abline(intercept = Sdiff * -1.96, slope = 1, linetype = 3) +
      theme_classic() +
      theme(panel.grid.major = element_line(color = "gray95")) +
      scale_colour_manual(values = cols) +
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
