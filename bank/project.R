## Checking for missing data
library(Amelia)
library(mlbench)
bank = read.csv("bank.csv", header=TRUE)
missmap(bank, col=c("blue", "red"), legend=FALSE)
