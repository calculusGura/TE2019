evaluatePathRisk <- function(path, productData){
  
  usedDPs   <-  path@dps
  dpList    <-  productData@dpList
  compList  <-  productData@compList;
  compDPMatrix <- productData@compDPMatrix;
  
  qualityRisk <- length(grep("R", dpList[usedDPs, "type"]));
  solutionArea <- calculateSolutionArea(path, productData);
  costRisk <- evaluateCostRisk(path, productData);
  deliveryRisk <- evaluateDeliveryRisk(path, productData);
  
  #input the result into the path instance
  path@qualityScore   <- qualityRisk;
  path@solutionArea   <- solutionArea;
  path@costScore      <- costRisk;
  path@deliveryScore  <- deliveryRisk;
  
  return(path);
}


#solutionArea
calculateSolutionArea <- function(path, productData){

  solutionArea <- colSums(path@constDPMatrix)/colSums(productData@constDPMatrix);
  solutionArea <- subset(solutionArea, solutionArea>0);
  solutionArea <- geoMeans(solutionArea);
  
  return(solutionArea)
}

##geometric mean for solutionArea 
geoMeans= function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


#cost risk
evaluateCostRisk <- function(path, productData){
  usedDPs <- path@dps
  
  #preparation of cost evaluation
  costData <- merge(productData@compDPMatrix[,], productData@dpList[,],
                    by.x="row.names", by.y="row.names");
  costData <- merge(costData, productData@compStatusWeight, 
                    by.x="status", by.y="compStatus");
  
  firstIndex <- grep(rownames(head(productData@compList,1)), colnames(costData))[1];
  lastIndex <- grep(rownames(tail(productData@compList,1)), colnames(costData));
  
  row.names(costData) <- costData$Row.names;
  costData <- cbind(costData[c("type", "importance", "costSens", "status", "statusWeight")],
                    costData[firstIndex:lastIndex]) 
  
  ##equation of cost risk evaluation
  baseCost <- productData@compList[,2];
  
  costData[,6:(6+lastIndex-firstIndex)] <- (costData[,6:(6+lastIndex-firstIndex)]
                                            * costData[,"costSens"]
                                            * costData[,"statusWeight"]);
  
  costRisk <- (colSums(costData[usedDPs,6:(6+lastIndex-firstIndex)]) 
               / colSums(costData[,6:(6+lastIndex-firstIndex)]));
  
  costRisk[is.nan(costRisk)] <- 0
  costRisk <- sum(costRisk);
  
  return(costRisk);
}


#deliver risk
evaluateDeliveryRisk <- function(path, productData){
  
  usedDPs <- path@dps
  dpList <- productData@dpList
  taskDPMatrix <- productData@taskDPMatrix;
  
  deliveryData <- as.matrix(colSums(taskDPMatrix[usedDPs,])/colSums(taskDPMatrix))
  colnames(deliveryData) <- "usedRatio";
  
  deliveryData <- merge(productData@taskList, deliveryData, 
                        by.x="row.names", by.y="row.names");
  deliveryData <- merge(productData@taskStatusWeight, deliveryData, 
                        by.x="taskStatus", by.y="status");
  
  browser();
  row.names(deliveryData) <- deliveryData$Row.names;
  deliveryData <- deliveryData[c("name", "duration", "type", "taskStatus", "statusWeight", "usedRatio")];
  
  #dequation of elivery evaluation
  ##statusWeight * taskDuration * DPs, then narrow down to used DPs
  
  deliveryRisk <- sum(as.numeric(deliveryData[,"duration"]) 
                      * deliveryData[,"statusWeight"]
                      * deliveryData[,"usedRatio"]);
  
  return(deliveryRisk);
}























#######################################
#not usded"

evaluateDeliveryRisk1 <- function(path, productData){

  usedDPs <- path@dps
  dpList <- productData@dpList
  
  #preparation of delivery evaluation
  deliveryData <- t(productData@taskDPMatrix);
  deliveryData <- merge(productData@taskList, deliveryData, 
                        by.x="row.names", by.y="row.names");
  deliveryData <- merge(taskStatus, deliveryData, 
                        by.x="status", by.y="status");
  
  firstIndex <- grep(rownames(head(dpList,1)), colnames(deliveryData))[1];
  lastIndex <- grep(rownames(tail(dpList,1)), colnames(deliveryData));
  
  row.names(deliveryData) <- deliveryData$Row.names;
  deliveryData <- cbind(deliveryData[c("name", "duration", "type", "status", "statusWeight")],
                   deliveryData[firstIndex:lastIndex]);
  
  #dequation of elivery evaluation
  ##statusWeight * taskDuration * DPs, then narrow down to used DPs
  browser();
  deliveryData[,firstIndex:lastIndex] <- (deliveryData[,"duration"] 
                                          * deliveryData[,"statusWeight"]
                                          * deliveryData[,firstIndex:lastIndex]);
  deliveryRisk <- sum(deliveryData[,usedDPs]);
  browser();

  return(deliveryRisk);
  
}




