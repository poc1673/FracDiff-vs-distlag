

#################################################################################################### 
# Function Name: collectLmMetrics 
# Description: Collect the relevant linear regression data from the linear regression results.
# Inputs 
# [1] lmObject
# Outputs:
# [1] lmMetrics 

collectLmMetrics <- function(lmObject){ 
  require(data.table)
  require(reshape2)
  require(magrittr)
  regSummary <- summary(lmObject)
  coefVals<- regSummary$coefficients[,4] %>% as.data.table
  coefVals$variable <- regSummary$coefficients  %>% rownames
  lmMetrics <- dcast(coefVals,.~variable,fun.aggregate = function(x){sum(x,na.rm=T)},value.var = ".")
  r2Val <- regSummary$adj.r.squared
  lmMetrics$R2 <- r2Val
  return(lmMetrics) } 
#####################################################################################################