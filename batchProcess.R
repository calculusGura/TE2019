#setwd("C:/Users/buzzs/Dropbox/TE2019");
#setwd("/home/ubuntu/test");

setwd("/Users/Jeong/Dropbox/TE2019");



rm(list = ls());

source("./setEnvirionment.R");
source("./dataClasses.R");
csvData1 <- read.csv("./productData/compDPstructure.csv");
csvData2 <- read.csv("./productData/constDP.csv");
csvData3 <- read.csv("./productData/taskDP.csv");
#csvData4 <- read.csv("./productData/taskTask.csv");
csvData5 <- read.csv("./productData/statusWeight.csv");


source("./generateModel.R");
productData <- generateCompDPModel(csvData1);
productData <- generateConstDPModel(csvData2, productData);
productData <- generateTaskDPModel(csvData3, productData);
#productData <- generateTaskTaskModel(csvData4, productData);
productData <- generateWeightModel(csvData5, productData);


source("./networkView.R");
showProductNetwork(productData);
#png("./test.png", width = 2000, height = 2000);
#par(mfrow=c(1,1));
#showProductNetwork(productData);
#dev.off();


source("./explorePropagationPath.R");
path <- exploringPropagationPath(c("D1"), productData);


source("./pathEvaluation.R");
path <- evaluatePathRisk(path, productData);



