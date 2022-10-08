


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
rm(list = ls())

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
                           identifyDifferencing(x,dLow = .01,dHigh = 2,size = 6,steps = 10,outputTransformed = TRUE    )}     )
# Create and format names:
a = lapply(1:length(transformedData), function(x){ 
  curName <- names(transformedData)[x]
  newNames <- paste(curName,"_Frac_d_",names(transformedData[[x]]),sep = "" )
  names(transformedData[[x]]) <- newNames})

names(a) <- names(transformedData)
for(i in names(transformedData)    ){  names(transformedData[[i]]) <- a[[i]]}
fracDiffData <- Reduce(f = cbind  , x = transformedData, accumulate = FALSE)
names(fracDiffData) <- sapply(X = names(fracDiffData),FUN = function(x){gsub(pattern = " ",replacement = ".",x = x)} )
names(fracDiffData) <- sapply(X = names(fracDiffData),FUN = function(x){gsub(pattern = "10",replacement = "Ten",x = x)} )
fracDiffData$Date <- centeredData$Date

# [4] ---------------------------------------------------------------------
# These results are loaded from the Data Transformation.R script.
distLagData <- readRDS("Processed Data.rds")

# [5] ---------------------------------------------------------------------

# We will use the following procedure for selecting the best models:
# i. Regression equations for all underlying variables (unemployment, GDP, et cetera) will be created with no variable overlap (IE no cases where there is more than on unemployment rate)
# ii. Regression will be run for all equations for both the distributed lag and the fractional differencing models.
# iii. A set of criteria will be set for expected coefficients. Result with insignificant result wil be removed.
# iv. The adjusted R^2 will be compared across results; the result with the highest R^2 will be selected.

fracDiffCombos <- expand.grid(names(fracDiffData)[-78] ,names(fracDiffData)[-78] ) %>% as.data.table
fracDiffCombos$Var1 %>% gsub(fracDiffCombos$Var1,"_.*$","")
fracDiffCombos <- fracDiffCombos[!(gsub( Var1,pattern = "_.*$",replacement = "")==gsub( Var2,pattern = "_.*$",replacement = ""))]
fracDiffFormulas <- paste("Default~",fracDiffCombos$Var1,"+",fracDiffCombos$Var2,sep = "")
fracDiffFormulas <- sapply(fracDiffFormulas,FUN = as.formula)

fracDiffData$Default <- centeredData$`90+ DPD`
fracDiffData <- fracDiffData %>% as.data.table


distLagCombos <- expand.grid(names(distLagData)[-1] ,names(distLagData)[-1] ) %>% as.data.table
distLagCombos <- distLagCombos[!(gsub( Var1,pattern = "_.*$",replacement = "")==gsub( Var2,pattern = "_.*$",replacement = ""))]
distLagFormulas <- paste("Default~",distLagCombos$Var1,"+",distLagCombos$Var2,sep = "")

# Put together full data-set and specify the training data end date.
fullData <- merge(fracDiffData,distLagData,by.x = "Date",by.y = "out") %>% as.data.table
trainingStart <- as.Date("2001-04-01")
trainingEnd <- as.Date("2015-12-31")
trainingData <- fullData[Date <= trainingEnd,]
testingData <-  fullData[Date > trainingEnd,]

fullData <- as.data.table(fullData, centeredData$`90+ DPD` )
regressionResults <- lapply(X = c(fracDiffFormulas,distLagFormulas),
                            FUN = function(x){return(lm(formula = x,data = trainingData))})
names(regressionResults) <- c(fracDiffFormulas,distLagFormulas)

# Collect all of the regression models and summarize them.
source("functions for post.R")
collectLmMetrics(regressionResults[[1]])
lmResults <- lapply(1:length(regressionResults),
                    FUN = function(x){
                      collectedResults <- collectLmMetrics(regressionResults[[x]])
                      collectedResults$Name <- names(regressionResults)[x]
                      return(as.data.table(melt(collectedResults,id.vars = "Name")))}   )  


lmResults <- rbindlist(lmResults)[!(variable==".")]

# Divide into distributed lag and fractional differencing models.
fracDiffModels <- lmResults[grep(pattern = "Frac_d",x = Name),]
distLagModels <- lmResults[!grep(pattern = "Frac_d",x = Name),]
distLagModels$Name <- distLagFormulas
# Pick the best results based on R^2
fracDiffFormula <- fracDiffModels[(variable=="R2")][(value == max(value))][1]
distLagModels$value <- as.numeric(distLagModels$value)
distLagFormula <- distLagModels[(variable=="R2")][ (value == max(value))&(value>.1)][1]
 
fracDiffforTesting <- regressionResults[[fracDiffFormula$Name]]
distLagforTesting <- regressionResults[[distLagFormula$Name]]
# [6] ---------------------------------------------------------------------

# Generate the in-sample results

#################################################################################################### 
# Function Name: generateTesting 
# Description: Generate the in-sample and out-of-sample back-testing.
# Inputs 
# [1] modelObject
# [2] IS
# [3] OOS
# Outputs:
# [1] modelResults 
generateTesting <- function(modelObject,IS,OOS) { 
  depVar <- names(modelObject$model)[1]
  predictIS <- predict(modelObject,IS)
  predictOOS<- predict(modelObject,OOS)
  
  absErrorIS <- abs( IS[[depVar]] - predictIS )
  absErrorOOS<- abs( OOS[[depVar]] - predictOOS)
  
  ISResults <- data.frame("Date" = IS$Date, Actual = IS[[depVar]],"Prediction" = predictIS, "Error" = absErrorIS)
  OOSResults <-data.frame("Date" =  OOS$Date,Actual =  OOS[[depVar]],"Prediction" = predictOOS, "Error" = absErrorOOS)
  modelResults <- list( "IS"= ISResults,
                        "OOS"= OOSResults  )
  return(modelResults) } 

#####################################################################################################

fracDiffResults <- generateTesting(modelObject = fracDiffforTesting,IS = trainingData,OOS = testingData)
fracDiffResults$IS$`Model Type` <- "Fractional Differencing"
fracDiffResults$OOS$`Model Type` <- "Fractional Differencing"
distLagResults <- generateTesting(modelObject = distLagforTesting,IS = trainingData,OOS = testingData)
distLagResults$IS$`Model Type` <- "Distributed Lag"
distLagResults$OOS$`Model Type` <- "Distributed Lag"

#OOSResults <-data.frame("Date" =  distLagResults$OOS$Date,Actual =  distLagResults$OOS[[depVar]],"Prediction" = predictOOS, "Error" = absErrorOOS)
OOSResults <- data.frame(list("Date" = fracDiffResults$OOS$Date , "Actual" = fracDiffResults$OOS$Actual,"Fractional Differencing" = fracDiffResults$OOS$Prediction,"Distributed Lag" =distLagResults$OOS$Prediction))
OOSResults <- melt(OOSResults,id.vars =c( "Date" ),value.name = "90+ DPD as Percent of Balance") %>% as.data.table
# 
#
ISResults <- data.frame(list("Date" = fracDiffResults$IS$Date , "Actual" = fracDiffResults$IS$Actual,"Fractional Differencing" = fracDiffResults$IS$Prediction,"Distributed Lag" =distLagResults$IS$Prediction))
ISResults <- melt(ISResults,id.vars =c( "Date" ),value.name = "90+ DPD as Percent of Balance") %>% as.data.table
ISResults <-ISResults[Date >=as.Date("2008-10-01")]

ISChart <- ggplot(data = ISResults[variable!= "Error",] ) +geom_line(aes(x = Date,y = `90+ DPD as Percent of Balance`,color = variable))+ formattingDetails()  +  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+ ggtitle("In-Sample Back-testing Results")
OOSChart <- ggplot(data = OOSResults[variable!= "Error",] ) +geom_line(aes(x = Date,y = `90+ DPD as Percent of Balance`,color = variable))+ formattingDetails()  +  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+ ggtitle("Out of Sample Back-testing Results")

ggsave(plot = ISChart, "Figures/ISChart_outputPlot.png",height= 5,width = 9)
ggsave(plot = OOSChart, "Figures/OOSChart_outputPlot.png",height= 5,width = 9)



# [6] ---------------------------------------------------------------------

# Generate some supplementary results:


# Generate boxcar kernel alongside fractional differencing weights

steps <-  1:10  
boxcar <- c( rep(.25,4),rep(0,6))
fracdiffWeights <- c(fracWeights(d = .5,size = 4),rep(0,5))
weightComparisonResults <- melt( data.table("Lag" =steps , "Boxcar Kernel Weights" =boxcar , "Fractional Differencing Weights"=fracdiffWeights),id.vars = "Lag",value.name = "Weight",variable.name = "Method") %>% as.data.table
weightComparisonResults
 

weightPlot <- ggplot(weightComparisonResults) + 
  geom_line(aes(x = Lag, y = Weight, color = Method))  + theme(axis.text.y = element_blank())+
  formattingDetails() +  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  ggtitle("Fractional Differencing and Boxcar Weights")

ggsave(plot = weightPlot, "Figures/boxcar and frac weights.png",height= 5,width = 9)




