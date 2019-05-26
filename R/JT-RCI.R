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

JTRCI <- function(data = NA, 
                  pre = NA,
                  post = NA,
                  ppid = NA,
                  group = NA,
                  reliability = NA,
                  higherIsBetter = F,
                  indextype = "JT",
                  JTcrit = "auto",
                  normM = NA,
                  normSD = NA, 
                  dysfM = NA, 
                  dysfSD = NA, 
                  plot = T,
                  table = T, 
                  ...) {
  
  "%inrange%" <- function(x, rng) x >= rng[1] & x <= rng[2]
  
  ## check inputs:    
  if (!indextype %in% c("JT", "RCI")) {
    stop('indextype must be either "JT" or "RCI"\n', call. = FALSE)}
  
  
  if (any(rowSums(is.na(data[, c(pre, post)])) != 0)) {
    message(paste0("NB ", sum(rowSums(is.na(data[, c(pre, post)])))," cases have missing data - these are ommitted from the calculations\n"))
    
    df <- data[rowSums(is.na(data[, c(pre, post)])) == 0,]
  }
  
  x.pre <- as.numeric(df [, pre])
  x.post <- as.numeric(df [, post])
  
  if (is.na(ppid)) {
    ppid <- seq(1:length(x.pre))
    message("No participant IDs provided - substituting with values 1:n\n")} else {
      ppid <- df [, ppid]}

  if (is.na(reliability)) {
    stop("Provide reliability estimate wih argument 'reliability = '\n", call. = FALSE)}
  
  if (!higherIsBetter) {
    message("Assumed that lower scores are better (and reduction == improvement),\n if that is incorrect: set higherIsBetter = T\n")
  }
  if (higherIsBetter) {
    message("Assumed that higher scores are better (and increased scores == improvement),\n if that is incorrect: set higherIsBetter = F\n")
  }
  
  if (!is.na(group)) {
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
    
    # if either or both mean and SD for the dysfunctional is missing - the baseline distribution can be used instead:
    if (any(is.na(c(dysfM, dysfSD)))) {
      usingBLasDysf <- T
      
      dysfMtoUse <- mean(x.pre)
      dysfSDtoUse <- sd(x.pre)
      
      message("NB: using the sample baseline distribution to characterize the dysfunctional population. \n    to change: provide norms for dysfunctional population using 'dysfM =' and 'dysfSD ='\n") }
    
    else { 
      usingBLasDysf <- F 
      dysfMtoUse <- dysfM
      dysfSDtoUse <- dysfSD }
    
    if (is.na(normM) | is.na(normSD) | JTcrit == "A")  {
      # if no healthy sample norm data is available, only criterion A is  possible, also compute criterion A if requested. 
      # For its computation either the dysfunctional population norm M and SD will be used (if provided) or alternatively the mean and SD of the sample at baseline - see above. 
      
      crittype <- "criterion A"
      
      # if we're here but the B or C crit was requested, either one or both of the healthy normdata params is not provided - generate a warning:
      if (JTcrit == "B" |JTcrit == "C") {
        warning(paste0( "NB: computing criterion A - the requested JTcrit (",JTcrit, ") requires healthy norm mean and SD to be provided through arguments 'normM =' and 'normSD ='\n"))
      }
      
    } # end of if (is.na(normM) | is.na(normSD) | JTcrit == "A")
    
    else {
      # if norm data is availabe:
      
      # first check if the norm and dysfucnctional/pre-distributions overlap: 
      if ( (dysfMtoUse + (3 * dysfSDtoUse)) %inrange% c(normM - (3 * normSD), normM + (3 * normSD)) |
           (dysfMtoUse - (3 * dysfSDtoUse)) %inrange% c(normM - (3 * normSD), normM + (3 * normSD)) ) {
        
        # if the pre/dysfunctional distribution overlaps with the norm distribution, criterion C is recommended:
        if (JTcrit == "auto" |JTcrit == "C") {
          crittype <- "criterion C"}
        
        # compute B if explicity requested by the user but add a message:
        if (JTcrit == "B") {
          crittype <- "criterion B"
          
          message("NB criterion C is recommended when the baseline distribution overlaps with the norm distribution\n")
        }
      } # end of 'if distributions overlap' section
      
      else {
        # if the pre/clinical sample does not overlap with the norm sample, criterion B is preferred (according to Jacobson & Truax on page 15: https://pdfs.semanticscholar.org/03b7/3ae47ee0058af60de09ea3ef7696fc53eeb1.pdf?_ga=2.145821876.1695931614.1556802049-343840472.1556802049)
        
        if (JTcrit == "auto" |JTcrit == "B") {
          crittype <- "criterion B"}
        
        # compute C if explicity requested but add a message:
        if (JTcrit == "C") {
          crittype <- "criterion C"
          
          message("NB criterion B is recommended when the",  
                  switch(as.character(usingBLasDysf), "TRUE" = paste("baseline sample", 
                                                      "FALSE" = "dysfunctional population"), 
                         "distribution does not overlap with the norm distribution\n") )  }
      } # end of if no overlap section
    } # end of 'if norm data is available' section
    
    
    ## compute the determined crit type: 
    
    if(crittype == "criterion A") {
      # The level of functioning at post should fall outside the range of the dysfunctional population OR the baseline population 
      # i.e. more than 2 standard deviation in the 'more healthy' direction - dependent on 'higherIsBetter':
      
      if (!higherIsBetter) {
        critval <- dysfMtoUse - 2 * dysfSDtoUse
      }
      if (higherIsBetter) {
        critval <- dysfMtoUse + 2 * dysfSDtoUse
      } 
      
      message("Jacobson-Truax criterion A: ", round(critval, 1))
      message(" this value represents two sd from the ", 
                    switch(as.character(usingBLasDysf), 
                           "TRUE" = paste("baseline", if(useGroups) {"total"}, "sample mean\n"), "FALSE" = "dysfunctional population mean\n"))
      
    }
    
    if(crittype == "criterion B") {
      # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 2 standard deviation of the mean of the comparison norm data - direction dependent on 'higherisbetter'
      if (!higherIsBetter) {
        critval <- normM + 2 * normSD}
      
      if (higherIsBetter) {
        critval <- normM - 2 * normSD}
      
      message("Jacobson-Truax criterion B: ", round(critval, 1))
      message(" this value represents two sd from the functional/healthy population norm mean\n")
    }
    
    if(crittype == "criterion C") {
      # The level of functioning at post should place the patient closer to the mean of the comparison norm data than the mean of the clinical norm data / pre measurement - weighted by SD of both clin and norm
      
      critval <- ((sd(x.pre) * normM) + (normSD * mean(x.pre))) / (sd(x.pre) + normSD)
      
      message("Jacobson-Truax criterion C:", round(critval, 1))
      message(" this value represents the weighted midpoint between the ", 
                     switch(as.character(usingBLasDysf), 
                            "TRUE" = paste("baseline", if(useGroups) {"total"}, "sample mean"), "FALSE" = "dysfunctional"), 
                     " and functional norm mean, \n i.e. the value at which an individual is equally likely to belong to the functional as to the dysfunctional population\n")
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
  if(useGroups) {JTRCIdf$group <- as.factor(df[, group])}
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
    
    if (!higherIsBetter & (sum(x.pre < critval)) > 0) { 
      message(sum(x.pre < critval),  " participants scored below the Jacobson-Truax cut-off score at the pre-measurement \n interpret their Jacobson-Truax classification with caution") }
    
    if (higherIsBetter & (sum(x.pre > critval)) > 0) { 
      message(sum(x.pre > critval), " participants scored above the Jacobson-Truax cut-off score at the pre-measurement \n interpret their Jacobson-Truax classification with caution")} 
    
    if (!higherIsBetter) {
      # determine Jacobson-Truax classification (note that these next lines only 'work' if the entire series is run in the correct order):
      JTRCIdf$class_JTRCI <- NA
      JTRCIdf$class_JTRCI [JTRCIdf$post <= critval & JTRCIdf$change_Sdiff <= -1.96] <- "recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post <= critval & JTRCIdf$change_Sdiff > -1.96]  <- "non reliably recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post > critval & JTRCIdf$change_Sdiff <= -1.96]  <- "improved"
      JTRCIdf$class_JTRCI [JTRCIdf$post > critval & JTRCIdf$change_Sdiff > -1.96]   <- "unchanged"
      JTRCIdf$class_JTRCI [JTRCIdf$change_Sdiff >= 1.96]                            <- "deteriorated"
    }
    
    if (higherIsBetter) {
      # determine Jacobson-Truax classification (note that these next lines only 'work' if the entire series is run in the correct order):
      JTRCIdf$class_JTRCI <- NA
      JTRCIdf$class_JTRCI [JTRCIdf$post >= critval & JTRCIdf$change_Sdiff >= 1.96] <- "recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post >= critval & JTRCIdf$change_Sdiff < 1.96]  <- "non reliably recovered"
      JTRCIdf$class_JTRCI [JTRCIdf$post < critval & JTRCIdf$change_Sdiff >= 1.96]  <- "improved"
      JTRCIdf$class_JTRCI [JTRCIdf$post < critval & JTRCIdf$change_Sdiff < 1.96]   <- "unchanged"
      JTRCIdf$class_JTRCI [JTRCIdf$change_Sdiff <= -1.96]                          <- "deteriorated"
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
    plot_JT(...)
  }
  
  if (plot == T & indextype == "RCI") {
    plot_RCI(...)
  } 
  
}
