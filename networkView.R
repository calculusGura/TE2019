showProductNetwork <- function(productData){
  
  #matrix DP X {constraint, compoent}
  constDPMatrix <- productData@constDPMatrix;
  compDPMatrix <- productData@compDPMatrix;
  productMatrix <-deliveryData <- merge(constDPMatrix, compDPMatrix, by.x="row.names", by.y="row.names");
  row.names(productMatrix) <- productMatrix[,1];
  productMatrix <- productMatrix[,-1];
  
  #generata graph from product matrix
  network <- graph_from_incidence_matrix(productMatrix);
  
  #define type of node from attribute DP, requirement DP, constratint, component  
  dpTypes <- V(network)$name;
  dpTypes <- dpTypes[1:nrow(productData@dpList)];
  dpTypes <- matrix(dpTypes);
  rownames(dpTypes) <- dpTypes[,1];
  dpTypes <- merge(dpTypes, productData@dpList, by.x="row.names", by.y="row.names");
  dpTypes <- dpTypes[,6];
  dpTypes <- ifelse(dpTypes=="R", 1, 2); ## 1: requirement DP, 2: attribute DP
  
  ##3: constraint. 4: component
  constCompTypes <- as.numeric(c(paste(rep(3,nrow(productData@constList))), 
                                 paste(rep(4,nrow(productData@compList)))));

  types <- c(dpTypes,constCompTypes);
  
  #define the node color
  V(network)$color <- c("palegreen3", "steelblue","slategray", "tomato")[types];
  
  #define the node shape
  ## myXX <- user defined shape
  V(network)$shape <- c("mytriangle", "circle", "myrectangle","mydiamond")[types];
 
  #defined the node label
  V(network)$label <- as.character(types);
  ##component label
  V(network)$label[V(network)$label=="4"] <- 
    productData@compList[V(network)$name[V(network)$label=="4"] ,1];
  ##requirement DP label
  V(network)$label[V(network)$label=="1"] <- 
    productData@dpList[V(network)$name[V(network)$label=="1"] ,1];
  ##attribute DP label
  V(network)$label[V(network)$label=="2"] <- 
    productData@dpList[V(network)$name[V(network)$label=="2"] ,1];
  ##constraint label
  V(network)$label[V(network)$label=="3"] <- 
    productData@constList[V(network)$name[V(network)$label=="3"] ,1];
  
  V(network)$label.cex=.6
  V(network)$label.font=2
   
  result<- plot(network,vertex.label.color="white", vertex.frame.color=0,vertex.size=10);

  #show group  
  ##plot(network, mark.groups=list(c(1,4,5,8), c(15:17)),mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)
  

  return(result);  
}


## definition of node shapes 
myDiamond <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], fg=vertex.color, bg=vertex.color,
          stars = cbind(vertex.size+0.1, vertex.size+0.03,vertex.size+0.1, vertex.size+0.03),
          xlim=c(vertex.size+1,2), ylim=c(vertex.size+2,1),
          add=TRUE, inches=FALSE)
}

# clips as a circle
add_shape("mydiamond", clip=shapes("circle")$clip,
          plot=myDiamond)





myRectangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], fg=vertex.color, bg=vertex.color,
          rectangles = cbind(vertex.size+0.1, vertex.size+0.03),
          add=TRUE, inches=FALSE)
}

# clips as a circle
add_shape("myrectangle", clip=shapes("circle")$clip,
          plot=myRectangle)



mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/150 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], fg=vertex.color, bg=vertex.color,
          stars = cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}

# clips as a circle
add_shape("mytriangle", clip=shapes("circle")$clip,
          plot=mytriangle)





# generic star vertex shape, with a parameter for number of rays
mystar <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
         FUN=function(x, y, bg, size, nor) {
           symbols(x=x, y=y, bg=bg,
                   stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
                   add=TRUE, inches=FALSE)
         })
}
# no clipping, edges will be below the vertices anyway
add_shape("star", clip=shape_noclip,
          plot=mystar, parameters=list(vertex.norays=5))



