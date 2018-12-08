# Set working directory
user <- Sys.getenv("USER")

if (user == "rutgerpoldermans"){
  setwd("/Users/rutgerpoldermans/Projects/digits")
} else if (user == "Milan"){
  setwd("/Users/milanpleus/Google Drive/Projects/digits")
} else {
  break
}

# Import data
df <- read.csv("train.csv")
compdata <- read.csv("test.csv")


# Take trainingset and testset
sample_size <- 10000
df <- df[sample(1:dim(df)[1],sample_size,replace=F), ]
index <- sample(1:sample_size,floor(0.8*sample_size),replace=F)
testdata <- df[-index,]
traindata <- df[index,]

traindata$zero <- (traindata$label == 0)
namesX <- colnames(traindata)
namesX <-namesX[grep("^[pP].*", namesX)]  
namesX <- paste(namesX,collapse="+")

# Estimate logit for zero's
fit <- glm(as.formula(paste("zero ~ ", namesX, sep = "")), data = traindata, family = "binomial")
summary(fit)
fitted <-predict(fit)
probs <- (exp(fitted)/(1+exp(fitted))>0.5)
traindata$probs <- probs

# Plot histogram 
if (user == "rutgerpoldermans"){
hist((exp(fitted)/(1+exp(fitted))))
}

# Logit requires 10 different models
# Alternative: neural network
library(neuralnet)
set.seed(99999999)
NN = neuralnet(as.formula(paste("label ~ ", namesX, sep = "")), data = traindata, hidden = 3 , linear.output = T )
traindata$NN <- NN$response
head(traindata[,c("label","NN")])
