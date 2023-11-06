#### Libraries  ####

# check for installed libraries and install any missing ones
list.of.packages <- c(
  "tidyverse",
  "readxl",
  "ecotox",
  "caret",
  "sqldf",
  "tcplfit2",
  "drc",
  "ggplot2",
  "data.table",
  "shiny",
  "shinyscreenshot",
  "refund.shiny",
  "bslib"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load libraries
library(tidyverse)
library(readxl)
library(ecotox)
library(caret)
library(sqldf)
library(tcplfit2)
library(drc)
library(ggplot2)
library(data.table)
library(shiny)
library(shinyscreenshot)
library(refund.shiny)
library(bslib)

# normalize data ----
normalize<-function(m,pc){
  (m - pc)/(max(m)-pc)
}


# convert variable name to string ----
strVarName <- function(x) {
  deparse(substitute(x))
}

# prepare data for plotting ----
prepData <- function(xl_file) {
  
  #### Import Data ####
  rawData <- read_excel(xl_file)
  
  # Sort, and log-transform dose ####
  rawData <- rawData %>%
    arrange(experiment, dose_uM) %>% # sort by exp and dose
    mutate(logDose=if_else(dose_uM==0, -2, log10(dose_uM))) # log transform, if value = 0, set to log10(0.01))
  
  
  #### Normalization ####
  
  # Min-Max normalization function ####
  normMinMax<-function(x, minVal, maxVal){
    (x - minVal)/(maxVal-minVal)
  }
  
  # min lum
  minLum<-rawData %>%
    filter(experiment=="POS") %>%
    dplyr::select(lum) %>%
    pull() %>%
    mean()
  
  # max lum
  maxLum<-rawData %>%
    filter(dose_uM==0) %>%
    dplyr::select(lum) %>%
    pull() %>%
    mean()
  
  # Normalize
  rawData <- rawData %>%
    mutate(normLum = normMinMax(x=lum, minVal=minLum, maxVal=maxLum))
}

# generate plot and table ----
# returns list that contains plot at index 1 and table at index 2 ----
genPlot <- function(xl_file, chemVal, fixedLimit) {
  
  #### get data ####
  rawData <- prepData(xl_file)
  
  #### filter data ####
  rawData <- filter(rawData, experiment == chemVal)
  
  #### plot ####
  if (fixedLimit == TRUE) {
    curve_fit <- rawData %>%
      drm(formula = normLum ~ dose_uM, data = ., fct = LL.4(names = c('hill', 'min_value', 'max_value', 'ec_50'), fixed=c(NA, 0, 1, NA)))
  } else {
    curve_fit <- rawData %>%
      drm(formula = normLum ~ dose_uM, data = ., fct = LL.4(names = c('hill', 'min_value', 'max_value', 'ec_50')))
  }
    
  ec50 <- curve_fit$coefficients['ec_50:(Intercept)']
  
  data_predicted <- tibble(
    dose_uM = seq(min(rawData$dose_uM), max(rawData$dose_uM), 0.01)
  )
  
  data_predicted$predicted <- predict(
    curve_fit,
    newdata = as.data.frame(data_predicted)
  )
  
  lines_to_highlight_ec50 <- tribble(
    ~x,   ~xend, ~y,   ~yend,
    ec50, ec50,  -Inf, 0.5,
    0,    ec50,  0.5,  0.5
  )
  
  p <- ggplot() +
    ggtitle(chemVal) +
    geom_segment(
      data = lines_to_highlight_ec50,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = 'grey', linetype = 'dashed', size = 1
    ) +
    geom_line(data = data_predicted, aes(x = dose_uM, y = predicted), size = 1) +
    geom_point(
      data = rawData,
      aes(x = dose_uM, y = normLum, fill = experiment),
      shape = 21, size = 3, color = 'white', show.legend = FALSE
    ) +
    annotate(
      'text', x = 0.01, y = 0.5, label = paste0('EC50: ', round(ec50, digits = 3), ' uM'),
      hjust = 0, vjust = -1, size = 7
    ) +
    scale_x_log10(name = 'Dose [uM]', breaks = unique(rawData$dose_uM)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.25), name = '% Viability', labels = scales::percent)
    theme(text = element_text(size = 20))

  #### display plot ####
  plot(p)
  
  #### display result table ####
  finalResults <- ED(curve_fit, respLev = 50, interval = "delta")
  colnames(finalResults) <- c("EC50","Std. Error","Lower 95% CI","Upper 95% CI")
  
  out <- list()
  out[[1]] <- p
  out[[2]] <- finalResults

  return(out)
}


# put all chem values in df ----
getChems <- function(xl_file) {
  
   #### get data ####
   rawData <- prepData(xl_file)
   
   #### get all unique chem names ####
   chems <- (unique(rawData$experiment))
   
   #### remove POS ####
   chems <- chems[chems != "POS"]
   
   return (as.data.frame(chems))
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# generate multiple plots ----
# returns list that contains multiplot at index 1 and table at index 2 ----
genMultPlots <- function(xl_file, chems, fixedLimit) {

  chemNames <- list()
  myPlots <- list()
  myTable <- list()
  out <- list()
  i = 1
  
  # generate and save each plot/table to respective lists
  for(c in chems[[1]]) {
    temp <- genPlot(xl_file,c,fixedLimit)
    myPlots[[i]] <- temp[[1]]
    myTable <- rbind(myTable,temp[[2]])
    i = i + 1
  }
  
  myTable <- cbind(chems[[1]],myTable) # add chem names column
  colnames(myTable)[1] <- "Chem"
  out[[1]] <- multiplot(plotlist = myPlots, cols = 2)
  out[[2]] <- myTable 
  return (out)
}
