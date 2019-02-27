# @import data.table
# @import ggplot2

JTRCI <-
  function(x.pre = NA,
           x.post = NA,
           reliability = NA,
           ppid = NA,
           type = "JT",
           plot = T,
           table = T,
           higherIsBetter = F,
           JT.crit = "auto",
           norm.M = NA,
           norm.SD = NA)
    {
  
    
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
      warning( "\nNo participant IDs provided or length of ppid does not match lenght of data \nsubstituting with values 1:n")
    }
    
    if (is.na(reliability)) {
      stop('\nprovide reliability estimate in reliability = ', call. = FALSE)
    }
    
    # determine SEmeasurement and Sdiff for computation of reliable change index:
    
    SEmeasurement <-
      sd(x.pre) * sqrt(1 - reliability) # standard error of measurement based on the sd of the pre measurement/baseline
    
    Sdiff <-
      sqrt(2 * SEmeasurement ^ 2) # the SD of the SEm for a difference score
    
    
    if (type == "JT") {
      # determine the criterion to determine recovery:
      
      ### C is preferred, followed by B, then A..
      
      if (is.na(norm.M) | is.na(norm.SD) | JT.crit == "A")  {
        # if no norm data is available, only crit A is  possible,
        # also compute crit A if requested through JT.crit
        
        crittype <- "crit A"
        crit <- mean(x.pre) - 1.96 * sd(x.pre)  # #The level of functioning at post should fall outside the range of the clinical population (more than 1.96 standard deviation in the 'more healthy' direction).
        
        warning("\nJacobson-Truax criterion A computed")
        
        if (JT.crit == "B" | JT.crit == "C") {
          # if we're here but a different crit was requested, this means that either one or both of the normdata params is not provided:
          warning("\nrequested JT.crit requires normdata provided through parameters norm.M and norm.SD \ncomputing JT.crit A instead")
        }
      }
      else {  # if norm data is availabe:
        
        if( (mean(BIdf$BI.pre) + (3 *sd(BIdf$BI.pre))) %between% c(norm.M - (3 * norm.SD), norm.M + (3 * norm.SD)) |
          (mean(BIdf$BI.pre) - (3 *sd(BIdf$BI.pre))) %between% c(norm.M - (3 * norm.SD), norm.M + (3 * norm.SD)) ) {
          # if the pre/clinical sample does not overlap with the norm sample, criterion C is recommended:
          #The level of functioning at post should place the patient closer to the mean of the comparison norm data than the mean of the clinical norm data / pre measurement - weigted by SD of both clin and norm "
          
          if (JT.crit == "auto" | JT.crit == "C") {
            crittype <- "crit C"
            crit <- ((sd(x.pre) * norm.M) + (norm.SD * mean(x.pre))) / (sd(x.pre) + norm.SD)
            warning("\nJacobson-Truax criterion C computed")
          }
          
          if (JT.crit == "B") {
            warning("\ncomputing JT.crit B as requested but JT.crit C is recommended based on the data: x.pre does not overlap with the norm distribution")
            crittype <- "crit B"
            crit <- norm.M + 1.96 * norm.SD # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data
          }
          
        }
        
        else {
          # if the distributions of the pre/clinical sample overlaps with the norm distribution, use criterion B: # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data
          if (JT.crit == "auto" | JT.crit == "B") {
            crittype <- "crit B"
            crit <- norm.M + 1.96 * norm.SD # The level of functioning at post should fall within the range of the comparison non-clinical group,i.e. within 1.96 standard deviation of the mean of the comparison norm data
            warning("\nJacobson-Truax criterion B computed")
          }
          
          if (JT.crit == "C") {
            warning("\ncomputing JT.crit C as requested but JT.crit B is recommended based on the data: x.pre overlaps with the norm distribution")
            crittype <- "crit C"
            crit <-
              ((sd(x.pre) * norm.M) + (norm.SD * mean(x.pre))) / (sd(x.pre) + norm.SD)
          }
          
        }
      }
    }
      
      if (type == "RCI") {
        crittype <- "non JT simple RCI"
        crit <- "none"
      }
      
      # see -> http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.615.8373&rep=rep1&type=pdf
      
      # create a JTRCIdf dataframe and log things:
      JTRCIdf <- NULL
      JTRCIdf$ppid <- ppid
      JTRCIdf <- as.data.frame(JTRCIdf)
  #    x.pre <- as.numeric(x.pre)
  #    x.post <- as.numeric(x.post)
      JTRCIdf$pre <- x.pre
      JTRCIdf$post <- x.post

      
      if (higherIsBetter) {
        JTRCIdf$change <- JTRCIdf$pre - JTRCIdf$post
      } else {
        JTRCIdf$change <- JTRCIdf$post - JTRCIdf$pre
      }
      
      # write values to JTRCIdf:
      JTRCIdf$SEmeasurement <- SEmeasurement
      JTRCIdf$Sdiff <- Sdiff
      JTRCIdf$crittype <- crittype
      JTRCIdf$crit <- crit
      
      # compute Reliable Change Indices:
      JTRCIdf$RCI <- JTRCIdf$change / Sdiff
      
      if (type == "JT") {
        # determine Jacobson-Truax classification:
        JTRCIdf$JTclass_RCI <- NA
        JTRCIdf$JTclass_RCI [JTRCIdf$post <= crit & JTRCIdf$RCI <= -1.96] <- "recovered"
        JTRCIdf$JTclass_RCI [JTRCIdf$post <= crit & JTRCIdf$RCI > -1.96]  <- "non reliably recovered"
        JTRCIdf$JTclass_RCI [JTRCIdf$post > crit & JTRCIdf$RCI <= -1.96]  <- "improved"
        JTRCIdf$JTclass_RCI [JTRCIdf$post > crit & JTRCIdf$RCI > -1.96]   <- "unchanged"
        JTRCIdf$JTclass_RCI [JTRCIdf$RCI >= 1.96]                      <- "deteriorated"
        
        JTRCIdf$JTclass_RCI <-
          factor(JTRCIdf$JTclass_RCI, levels = rev(c("recovered",
                                                  "non reliably recovered",
                                                  "improved",
                                                  "unchanged",
                                                  "deteriorated") ))
        
        JTRCIdf <<- JTRCIdf
        
        if(table) {
        JTtable <- data.table::setDT(JTRCIdf)[,.N, by = .("Jacobson Truax classification" = JTclass_RCI)]
        JTtable <- data.table::setorder(JTtable, na.last=T)
        print(JTtable)
         }

        warning( "\nassumed that lower scores are better (and reduction == improvement), if that is incorrect: set higherIsBetter = T")
      }
      
      if (type == "RCI") {
        # determine RCI classification:
        JTRCIdf$class_RCI <- "no reliable change"
        JTRCIdf$class_RCI [JTRCIdf$RCI > 1.96] <- "reliably deteriorated"
        JTRCIdf$class_RCI [JTRCIdf$RCI < -1.96] <- "reliably improved"
        
        JTRCIdf$class_RCI <-factor(JTRCIdf$class_RCI, levels = c("reliably deteriorated",
                                                           "no reliable change",
                                                           "reliably improved"))

        JTRCIdf <<- JTRCIdf
  
        if(table) {
          RCItable <- data.table::setDT(JTRCIdf)[,.N, by = .("reliable change classification" = class_RCI)]
          RCItable <- data.table::setorder(RCItable, na.last=T)
          print(RCItable)
        }
        
        warning( "\nassumed that lower scores are better (and reduction == improvement), if that is incorrect: set higherIsBetter = T")
      }

      
      # previous version also computed Wise table 1 (https://pdfs.semanticscholar.org/5e34/1ae1a311602280bebd31bfe98b5d17ed3ca1.pdf) based classifications but I've cut them out for the moment.
      #
      
      if (plot == T & type == "JT") {
        JTRCIdf$classPlot <- JTRCIdf$JTclass_RCI
        
        for (l in 1:length(levels(JTRCIdf$classPlot))) {
          levels(JTRCIdf$classPlot)[l] <-
            paste0(levels(JTRCIdf$classPlot)[l], ": ", table(JTRCIdf$classPlot)[[l]])
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
          
        library(ggplot2)
        
        JTRCIplot <- ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)), ], aes(x = pre, y = post, colour = classPlot)) +
          geom_point(size = 2.5) +
          geom_abline(intercept = 0, slope = 1) +
          geom_abline(intercept = Sdiff * 1.96, slope = 1, linetype = 3) +
          geom_abline(intercept = Sdiff * -1.96 , slope = 1, linetype = 3) +
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
          levels(JTRCIdf$classPlot)[l] <-
            paste0(levels(JTRCIdf$classPlot)[l], ": ", table(JTRCIdf$classPlot)[[l]])
        }
        
        # created vector of plot colours linked to specific level names:
        cols <- c( "reliably deteriorated" = "coral1", "no reliable change" = "skyblue3", "reliably improved" = "orchid3")
        
        names(cols) <-
          c(levels(JTRCIdf$classPlot)[grep("reliably deteriorated", levels(JTRCIdf$classPlot))],
            levels(JTRCIdf$classPlot)[grep("no reliable change", levels(JTRCIdf$classPlot))],
            levels(JTRCIdf$classPlot)[grep("reliably improved", levels(JTRCIdf$classPlot))])
        
        if (sum(is.na(JTRCIdf$post > 0))) {
          warning(sum(is.na(JTRCIdf$post)),"\ncases have missing post data - these are ommitted from the plot")
        }
        
        datrangelength <- c(max(c(JTRCIdf$pre, JTRCIdf$post))) - c(min(c(JTRCIdf$pre, JTRCIdf$post)))
        plotrange <- c( c(min(c(JTRCIdf$pre, JTRCIdf$post))) - .1*datrangelength, c(max(c(JTRCIdf$pre, JTRCIdf$post))) + .1*datrangelength)
        
        JTRCIplot <-
          ggplot2::ggplot(JTRCIdf[!(is.na(JTRCIdf$post > 0)), ], aes(x = pre, y = post,colour = classPlot)) +
          geom_point(size = 2.5) +
          geom_abline(intercept = 0, slope = 1) +
          geom_abline(intercept = Sdiff * 1.96, slope = 1, linetype = 3) +
          geom_abline(intercept = Sdiff * -1.96 , slope = 1, linetype = 3) +
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
    