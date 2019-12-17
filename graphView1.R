img1 <- readPNG("./image/const.png")


#const-dp-component network
showConstDPNetwork <- function(productData){
  net <- network(productData@constDPMatrix, directed = FALSE, 
                 ignore.eval = FALSE, names.eval = "weights");
  types <- c(productData@dpList[grep("D", network.vertex.names(net), value = TRUE), "type"],
             rep("C", length(grep("C", network.vertex.names(net)))));
  
  browser();
  net %v% "type" <- types;
  net %e% "colors" <- ifelse(net %e% "weights" > 1, "red", "grey75");
  net %e% "lty" <- ifelse(net %e% "weights" > 1, 2, 1);
  
  set.seed(1);
  netGraph <- ggnet2(net, 
                     mode = "kamadakawai",
                     color = "type", color.palette = c("R" = "palegreen3", "A"= "steelblue", "C" = "tomato"),
                     shape = "type", shape.palette = c("R" = 17, "A"= 16,  "C" = 15),
                     label=TRUE,
                     label.size = 3,
                     legend.position ="right",
                     legend.size = 12);
  return(netGraph);
}

showConstDPNetwork(productData)




#const-dp network
showConstDPNetwork <- function(productData){
  net <- network(productData@constDPMatrix, directed = FALSE, 
                 ignore.eval = FALSE, names.eval = "weights");
  types <- c(productData@dpList[grep("D", network.vertex.names(net), value = TRUE), "type"],
             rep("C", length(grep("C", network.vertex.names(net)))));

  net %v% "type" <- types;
  net %e% "colors" <- ifelse(net %e% "weights" > 1, "red", "grey75");
  net %e% "lty" <- ifelse(net %e% "weights" > 1, 2, 1);
  
  set.seed(1);
  netGraph <- ggnet2(net, 
                     mode = "kamadakawai",
                     color = "type", color.palette = c("R" = "palegreen3", "A"= "steelblue", "C" = "tomato"),
                     shape = "type", shape.palette = c("R" = 17, "A"= 16,  "C" = 15),
                     label=TRUE,
                     label.size = 3,
                     legend.position ="right",
                     legend.size = 12);
  return(netGraph);
}

showConstDPNetwork(productData)




showPropagationPath <- function(productData){
  net <- network(productData@constDPMatrix, directed = FALSE, 
                 ignore.eval = FALSE, names.eval = "weights");

  net %v% "type" <- c(productData@dpList[grep("D", network.vertex.names(net), value = TRUE), "type"],
                      rep("C", length(grep("C", network.vertex.names(net)))));
  net %e% "colors" <- ifelse(net %e% "weights" > 1, "red", "grey75");
  net %e% "lty" <- ifelse(net %e% "weights" > 1, 2, 1);
  
  set.seed(1);
  netGraph <- ggnet2(net, 
                     mode = "kamadakawai",
                     color = "type", color.palette = c("R" = "palegreen3", "A"= "steelblue", "C" = "tomato"),
                     shape = "type", shape.palette = c("R" = 1, "A"= 2,  "C" = 3),
                     edge.label = "weights",
                     edge.color = "colors",
                     edge.lty = "lty",
                     edge.size = "weights",
                     label=TRUE,
                     label.size = 3,
                     legend.position ="right",
                     legend.size = 12);
  return(netGraph);
}

showPropagationPath(productData)
