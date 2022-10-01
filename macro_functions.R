diff <- function(dataSeries,window=0,lag){
  diff.val <-  shift(dataSeries,n = lag) - dataSeries
  return(diff.val)
}


#################################################################################################### 
# Function Name: mavg 
# Description: Calculate the moving average using the boxcar transform, if specified.
# Inputs 
# [1] dataSeries
# [2] lag
# [3] window
# Outputs:
# [1] processedData 
#  
#  
mavg <- function(dataSeries,lag,window) { 
  require(zoo)
  require(data.table)
  mavg.values <- rollmean(x = dataSeries, k = window)
  mavg.w.lag <- shift(mavg.values,n = lag)
  return(mavg.w.lag) 
} 
#  
#####################################################################################################




#mavg  <- function(dataSeries,window=1,lag=1){
#  require(zoo)
#  require(data.table)
#  mavg.values <- rollmean(x = dataSeries, k = window)
#  mavg.w.lag <- shift(mavg.values,n = lag)
#  return(mavg.w.lag) 
#  }

#pct.chg <- function(macro.var,lag,window=0){
#  require(zoo)
#  require(data.table)
#  lag.vars <- shift(macro.var,n = lag)  
#  return(macro.var/lag.vars-1)
#}


#################################################################################################### 
# Function Name: pctChange 
# Description: Uses the percentage change of the variable with windows and lags.
# Inputs 
# [1] lag
# [2] dataSeries
# [3] window
# Outputs:
# [1] processedData 
#  
#  
pctChange <- function(dataSeries,lag,window) { 
  require(zoo)
  require(data.table)
  lagData <- shift(dataSeries,lag)
  kernelData <- boxCarKernel(dataSeries,lag,window)
  processedData <- ((lagData - kernelData     )/kernelData)  
  return(processedData) 
  } 
#  
#####################################################################################################


#################################################################################################### 
# Function Name: convVarStr 
# Description: Turn the name of a function into a string.
# Inputs 
# [1] fun - the name of the function.
# Outputs:
# [1] deparsedVar - The name of the function turned into a string for identification.
#  
#  
convVarStr <- function(fun){ 
  deparsedVar <- deparse(quote(fun))
   return(deparsedVar) 
} 
#  
#####################################################################################################


#################################################################################################### 
# Function Name: boxCarKernel 
# Description: Runs the boxcar kernel (moving average) on a date-series. Allows the user to incorporate a lag.
# Inputs 
# [1] df - the data to be used in the model. Should be a vector
# [2] window - the window to be used in averaging.
# [3] lag - The lag to be used when returning the data-series. Defaults to zero.
# Outputs:
# [1] smoothedOutput - The data series after the boxcar kernel has been run. 
boxCarKernel <- function(dataVal,window ,lag =0 ) { 
  require(zoo)
  smoothedOutput <- rollapply(data = dataVal,FUN = mean, width = window)
  naPadding <- rep(NA,(window -1) + lag  )
  smoothedOutput <- c(naPadding,smoothedOutput[1:(length(smoothedOutput)-lag)])
  return(smoothedOutput ) } 
#  
#####################################################################################################
 

#################################################################################################### 
# Function Name: logOdds 
# Description: Calculates the log odds using the data, specified lags, and a window by the user.
# Based on Breeden's method and distributed lag models.
# Inputs 
# [1] dataSeries
# [2] lag
# [3] window
# Outputs:
# [1] processedData 
#  
#  
logOdds <- function(dataSeries,lag,window) { 
  require(zoo)
  require(data.table)
  LagVars <- shift(dataSeries,n = lag) 
  processedData <- log(LagVars/(1-LagVars)) 
  return(processedData) 
} 
#  
#####################################################################################################

#################################################################################################### 
# Function Name: logRatio 
# Description: Calculates the log ratio using the data, specified lags, and a window by the user. Based on Breeden's method and distributed lag models.
# Inputs 
# [1] dataSeries
# [2] lag
# [3] window
# Outputs:
# [1] processedData 

logRatio <- function(dataSeries,lag,window) { 
  require(zoo)
  require(data.table)
  lagData <- shift(dataSeries,lag)
  kernelData <- boxCarKernel(dataSeries,lag,window)
  processedData <- log(lagData / kernelData     )
  return(processedData) 
} 
#  
#####################################################################################################

# gen.transformation
# Inputs:
# [1] macro.var - The macro economic variable being used.
# [2] macro.name - The name of the macro-economic variable to use for labeling.
# [3] transformation - The type of transformation to use. This is a function that's passed to the gen.transformation model.
# [4] transformation.name - The name of the transformation.
# [5] lag.inds - The lag
# [6] window.inds - The window to use.

# Outputs:
# Provide the transformed results
gen.transformation <- function(macro.var,macro.name, transformation,transformation.name,lag.inds, window.inds){
  lag.wind.combo <- expand.grid(lag.inds,window.inds )
  transformed.results <- lapply(X = 1:nrow(lag.wind.combo),
                                FUN = function(x){transformation(dataSeries =   macro.var,
                                                                 window = lag.wind.combo[[1]][x],
                                                                 lag = lag.wind.combo[[2]][x] ) }) %>% as.data.table
  result.names <- sapply(X = 1:nrow(lag.wind.combo),function(x){paste( macro.name ,
                                                                       transformation.name,
                                                                       "L_",
                                                                       lag.wind.combo[[1]][x] ,
                                                                       "__W_",lag.wind.combo[[2]][x],
                                                                       sep = "")})
  names(transformed.results) <- result.names
  
  return(transformed.results)}