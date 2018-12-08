user <- Sys.getenv("USER")

if (user == "rutgerpoldermans"){
  setwd("/Users/rutgerpoldermans/Projects/digits")
} else if (user == "Milan"){
  setwd("/Users/milanpleus/Google Drive/Projects/digits")
} else {
  break
}

traindata <- read.csv("train.csv")
testdata <- read.csv("test.csv")