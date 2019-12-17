#importing csv data of Component - DP relationships
generateCompDPModel <- function(csvData){
  dataLength <- nrow(csvData);
  compList <- list();
  dpList <- list();
  compCodeList <- list();
  dpCodeList <- list();
  
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){
      compList <- c(compList, c(as.character(csvData[i,1]), csvData[i,2], NA))};
    dpList <- c(dpList, 
                c(as.character(csvData[i,3]), csvData[i,4], csvData[i,5],as.character(csvData[i,6]),NA));
  }  
  
  #component list
  compList <- matrix(unlist(compList), ncol=3, byrow=TRUE);
  colnames(compList) <- c("name", "cost", "status");
  compCodeList <- paste0("C",1:nrow(compList));
  rownames(compList) <- compCodeList;
  compList <- data.frame(compList);
  compList$name <- as.character(compList$name);
  compList$cost <- as.numeric(as.character(compList$cost));
  
  #designParameter list
  dpList <- matrix(unlist(dpList), ncol=5, byrow=TRUE);
  colnames(dpList) <- c("name", "importance", "costSens", "type", "status");
  dpCodeList <- paste0("D",1:nrow(dpList));
  rownames(dpList) <- dpCodeList;
  dpList <- data.frame(dpList);
  dpList$name <- as.character(dpList$name);
  dpList$importance <- as.numeric(as.character(dpList$importance));
  dpList$costSens <- as.numeric(as.character(dpList$costSens));
  dpList$type <- as.character(dpList$type);

  #matrix of comp-dp relationships
  compDPMatrix <- matrix(0,nrow = nrow(dpList), ncol = nrow(compList));
  rownames(compDPMatrix) <- dpCodeList;
  colnames(compDPMatrix) <- compCodeList;

  count <- 0;
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){count <- count + 1;}
    compDPMatrix[i, count] <- 1;
  }  
  
  #return generated model
  productData <- new("ProductData", 
                     compList = compList, 
                     dpList = dpList, 
                     compDPMatrix= compDPMatrix);
  return(productData);
}



#importing csv data of Constraint - DP relationships
generateConstDPModel <- function(csvData, productData){
  dataLength <- nrow(csvData);
  constList <- list();
  
  for(i in csvData[which(csvData[,1] != ""),1]){
    constList <- c(constList, as.character(i));
  }

  #constraint list
  constList <- matrix(unlist(constList), ncol=1, byrow=TRUE);
  colnames(constList) <- c("name");
  constCodeList <- paste0("Cst",1:nrow(constList));
  rownames(constList) <- constCodeList;
  constList <- data.frame(constList);
  constList$name <- as.character(constList$name);
  
  #matrix of constraint-dp relationships
  constDPMatrix <- matrix(0,nrow = nrow(productData@dpList), ncol = nrow(constList));
  rownames(constDPMatrix) <- rownames(productData@dpList);
  colnames(constDPMatrix) <- rownames(constList);

  count <- 0;
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){count <- count + 1;}
    dpId <- which((productData@dpList)[,1] == csvData[i,2]);
    constDPMatrix[dpId, count] <- 1;
  }
  
  productData@constList <- constList;
  productData@constDPMatrix <- constDPMatrix; 
  productData@dpdpMatrix <- transformToDPDPmatrix(constDPMatrix); #matrix of dp-dp relationships
  
  return(productData);
}


#Transform from DP-constraint matrix to DP-DP matrix  
transformToDPDPmatrix <-function(dpConstMatrix){
  numberOfConst <- ncol(dpConstMatrix);
  numberOfDP <- nrow(dpConstMatrix);
  
  # generate 0 matrix with DP names 
  dpdpMatrix <- matrix(nrow = numberOfDP, ncol = numberOfDP, 0);
  rownames(dpdpMatrix) <- rownames(dpConstMatrix);
  colnames(dpdpMatrix) <- rownames(dpConstMatrix);
  
  for(i in 1:numberOfConst) {
    #Deduce DPs from each constraint
    relatedDPs <- which(dpConstMatrix[,i] ==1);
    #Put the constraint number to the cells of the related DPs
    dpdpMatrix[relatedDPs, relatedDPs] <- i; 
  }
  
  # put 0 on the diag cells
  diag(dpdpMatrix) <-0;
  return(dpdpMatrix);
}


#importing csv data of Task - DP relationships
generateTaskDPModel <- function(csvData, productData){
  dataLength <- nrow(csvData);
  taskList <- list();
  
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){
      taskList <- c(taskList, 
                    c(as.character(csvData[i,1]), csvData[i,2], 
                      as.character(csvData[i,3]), NA))};
  }  

  taskList <- matrix(unlist(taskList), ncol=4, byrow=TRUE);
  colnames(taskList) <- c("name", "duration", "type", "status");
  taskCodeList <- paste0("T",1:nrow(taskList));
  rownames(taskList) <- taskCodeList;
  taskList <- data.frame(taskList);
  taskList$name <- as.character(taskList$name);
  
  taskDPMatrix <- matrix(0,nrow = nrow(productData@dpList), ncol = nrow(taskList));
  rownames(taskDPMatrix) <- rownames(productData@dpList);
  colnames(taskDPMatrix) <- rownames(taskList);
  
  count <- 0;
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){count <- count + 1;}
    dpId <- which((productData@dpList)[,1] == csvData[i,4]);
    taskDPMatrix[dpId, count] <- 1;
  }
  
  productData@taskList <- taskList;
  productData@taskDPMatrix <- taskDPMatrix;

  return(productData);
}



#importing csv data of Constraint - DP relationships
generateWeightModel <- function(csvData, productData){
  
  taskStatusWeight <- csvData[complete.cases(csvData[,1:2]),1:2];
  compStatusWeight <- csvData[complete.cases(csvData[,3:4]),3:4];

  colnames(compStatusWeight)[2] <- "statusWeight";
  
  productData@taskStatusWeight <- taskStatusWeight;
  productData@compStatusWeight <- compStatusWeight; 

  return(productData);
}


#????
generateTaskTaskModel <- function(csvData, productData){
  dataLength <- nrow(csvData);
  size <- nrow(productData@taskList);
  taskTaskMatrix <- matrix(0,nrow=size, ncol=size)
  rownames(taskTaskMatrix) <- rownames(productData@taskList);
  colnames(taskTaskMatrix) <- rownames(productData@taskList);
  
  for(i in 1: dataLength){
    
    col <- rownames(productData@taskList)[which(productData@taskList["name"]==as.character(csvData[i,1]))];
    row <- rownames(productData@taskList)[which(productData@taskList["name"]==as.character(csvData[i,2]))];
    taskTaskMatrix[row, col] <- 1;
  }

  productData@taskTaskMatrix <- taskTaskMatrix;
  return(productData)
}
  


