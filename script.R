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


# Take sample
sample_size <- 1000
traindata <- traindata[sample(1:dim(traindata)[1],sample_size,replace=F), ]

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
