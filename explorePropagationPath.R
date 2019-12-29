checkPathFeasibility <- function(path, productData){}


#exploring propagation path
exploringPropagationPath <- function(startDPs, productData){
  
  propagationPath <- new("PropagationPath",
                         path = list(startDPs),
                         dps = startDPs,
                         constDPMatrix = productData@constDPMatrix);

  #iterating the engineering decision
  check <- ncol(propagationPath@constDPMatrix);  
  while(check !=0){
    propagationPath <- engineeringDecision(propagationPath);
    check <- ncol(propagationPath@constDPMatrix); 
  }
  
  #write down the DPs and constraints in the propagation path
  #with elimination of duplication
  propagationPath@dps <- unique(propagationPath@dps); 
  propagationPath@constraints <- unique(propagationPath@constraints);

  #express the path as a matrix format
  temp <- productData@constDPMatrix;
  temp[propagationPath@dps, ] <- temp[propagationPath@dps, ] + 1;
  temp[which(temp>0)] <- ifelse(temp[which(temp>0)]==1, 0, 1); 
  propagationPath@constDPMatrix <- temp;
   
  return(propagationPath);
}

#decide the direction of propagation on a constraint
engineeringDecision <- function(propagationPath){
  
  candidateDPs <- NULL;
  dps <- unlist(tail(propagationPath@path,1));
  constDPMatrix <- propagationPath@constDPMatrix;

  #deduce constraints connected to DPs
  relatedConstraints <- constDPMatrix[dps,,drop=FALSE];
  relatedConstraints <- constDPMatrix[,which(colSums(relatedConstraints) > 0), drop=FALSE];
  
  usedConstraints <- colnames(relatedConstraints);
  constDPMatrix   <- constDPMatrix[, !colnames(constDPMatrix) %in% usedConstraints, drop=FALSE];
  
  #
  candidateDPs <- rownames(relatedConstraints[which(rowSums(relatedConstraints) > 0), ,drop=FALSE]);
  candidateDPs <- setdiff(candidateDPs, propagationPath@dps);
    
    #finished the iteration in case there is no candidate 
    if(length(candidateDPs) ==0){
      propagationPath@constDPMatrix <- matrix(ncol=0, nrow=0); #check <- 0
      return(propagationPath)};
  
  selectedDPs     <- sample(candidateDPs, sample(length(candidateDPs),1));

  propagationPath@path <- append(propagationPath@path, list(usedConstraints)); 
  propagationPath@path <- append(propagationPath@path, list(selectedDPs)); 
  propagationPath@dps  <- append(propagationPath@dps, selectedDPs); 
  propagationPath@constraints   <- append(propagationPath@constraints, usedConstraints); 
  propagationPath@constDPMatrix <- constDPMatrix;
  
  return(propagationPath);
}
