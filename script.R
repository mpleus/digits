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
traindata <- read.csv("train.csv")
testdata <- read.csv("test.csv")

traindata$zero <- (traindata$label == 0)
namesX <- colnames(traindata)
namesX <-namesX[grep("^[pP].*", namesX)]  
namesX <- paste(namesX,collapse="+")

# Estimate logit for zero's
fit <- glm(as.formula(paste("zero ~ ", namesX, sep = "")), data = traindata, family = "binomial")
summary(fit)
predict(fit)
