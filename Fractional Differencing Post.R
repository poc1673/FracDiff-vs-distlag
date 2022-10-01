


# [1] Load the data that's being used including macroeconomic data and the national level default data.
# [2] Create a chart showing how the normalized values vary
# [3] Perform fractional differencing on the macro-economic data to arrive at the processed data series
# [4] Perform the distributed lag transformations on the macro-economic data series
# [5] Define the procedure that's going to be used to compare the models.
# [6] Run the procedure to review the fractional differencing models and the distributed lag models.
# [7] Perform in-sample and out-of-sample results between different models.
# [8] Create chart comparing the results.
# [9] 
# [10] 
# [11] 
# [12] 

#################################################################################################### 
# Function Name: centerData 
# Description: Function to center the values of a data-series
# Inputs 
# [1] dataValues
# Outputs:
# [1] centeredData 
centerData <- function(dataValues) { 
   dataMin <- min(dataValues, na.rm =T)
   dataMax <- max(dataValues, na.rm =T)
   centeredData <- (dataValues-dataMin)/(dataMax - dataMin)
   return(centeredData) } 
#####################################################################################################

# [1] ---------------------------------------------------------------------


## ------------------------------------------------------------------------------------------------------------------
library(pacman)
p_load(data.table,magrittr,ggplot2,tseries)
source("functions for blog post.R")

formattingDetails <- function(){return( theme_bw()+theme(axis.text = element_text(size = 13),
                                                         legend.position = "bottom",
                                                         axis.title = element_text(size = 16 ),
                                                         text = element_text(family = "sans"), 
                                                         title = element_text(size = 20),
                                                         legend.text = element_text(size = 13),
                                                         legend.title = element_blank())      )}

stateFileData <- list.files("ID_files/",pattern = "RDS$",full.names = T)
stateFileData <- lapply(X = stateFileData,FUN = function(x){as.data.table(readRDS(x))} ) %>% rbindlist
macroData <- readRDS("data_for_all_variables.rds")

pdData <- stateFileData[(Field == "90 days past due")|Field == "Total loans" , ][Type == "Credit Card"  ][`Financial Institution State`!=0][,c("Date","Financial Institution Name","Financial Institution State","Field","value"),with = F]
pdData <- dcast(data = pdData,formula = Date + `Financial Institution Name` + `Financial Institution State`~Field,fun.aggregate = function(x){sum(x,na.rm = T)},value.var = "value") %>% as.data.table

dataforTrainingNational <- pdData[,lapply(X = .SD, FUN = function(x){sum(x,na.rm = T)}),.SDcols = c("90 days past due","Total loans"),by = c("Date" ) ]
dataforTrainingNational <- dataforTrainingNational[,"90+ DPD":= `90 days past due`/`Total loans`]
nationalDPD <- ggplot(data = dataforTrainingNational) + geom_line(aes(x=Date,`90+ DPD`)) + formattingDetails() + ggtitle("National 90+ DPD")
ggsave(plot = nationalDPD,filename = "Figures/National DPD Values.png", height = 5, width = 9)

# Collect macro-economic data
macroVars <- readRDS("untransformedMacro.RDS")
names(macroVars) <- c("Date","Consumer Debt Service Payments","Consumer Price Index", "HPI","10 Year Treasury Yields","GDP","SP 500","Unemployment Rate")
macroVars <- as.data.table(macroVars)


# [2] ---------------------------------------------------------------------
centeredData <- macroVars[,lapply(X = .SD,function(x){centerData(x)}  ),.SDcols = c( "Consumer Debt Service Payments","Consumer Price Index", "HPI","10 Year Treasury Yields","GDP","SP 500","Unemployment Rate")]
centeredData$Date <- macroVars$Date
centeredData

dataforTrainingNational$Date <- dataforTrainingNational$Date +1
results <- merge(x =dataforTrainingNational[,c("Date","90+ DPD"),with = F] , y=centeredData , by = "Date") 
centeredData <- results[,lapply(X = .SD,function(x){centerData(x)}  ),.SDcols = c( "90+ DPD","Consumer Debt Service Payments","Consumer Price Index", "HPI","10 Year Treasury Yields","GDP","SP 500","Unemployment Rate")]
centeredData$Date <- results$Date

centeredDataforPlot <- melt(centeredData,id.vars = "Date")
centeredPlot <- ggplot(centeredDataforPlot)+geom_line(aes(x = Date , y = value , color = variable)) + formattingDetails()+  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
ggsave(plot = centeredPlot,filename = "Figures/Centered Variables.png", height = 5, width = 9)



# [3] ---------------------------------------------------------------------
# For each variable use the following fractional differencing:
# d values from 0 (no change) to 2
# All values will be considered. Nonstationarity and noncorrelation will be prioritized in model selection.
transformedData <- apply(X = centeredData[,c(-1,-9)],2,
                         FUN = function(x){
                           identifyDifferencing(x,dLow = .01,dHigh = 2,size = 10,steps = 10,outputTransformed = TRUE    )}     )
# Create and format names:
a = lapply(1:length(transformedData), function(x){ 
  curName <- names(transformedData)[x]
  newNames <- paste(curName,"_Frac_d_",names(transformedData[[x]]),sep = "" )
  names(transformedData[[x]]) <- newNames})

names(a) <- names(transformedData)
for(i in names(transformedData)    ){  names(transformedData[[i]]) <- a[[i]]}
fracDiffData <- Reduce(f = cbind  , x = transformedData, accumulate = FALSE)
## ------------------------------------------------------------------------------------------------------------------


# [4] ---------------------------------------------------------------------





distLagData <- 













