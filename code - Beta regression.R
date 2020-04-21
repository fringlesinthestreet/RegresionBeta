# PACKAGES
# install.packages("betareg")
# install.packages("DirichletReg")
# install.packages("zoib")
# install.packages("ggplot2")
# install.packages("ggcorrplot")
# install.packages("rjags")
# install.packages("rgl")
library(betareg)
library(rgl)
library(DirichletReg)
library(rjags)
library(zoib)
library(ggplot2)
library(ggcorrplot)

setwd("PATH/TO/FOLDER")

# LOADING DATA: data.txt
data <- read.table("data.txt", header=TRUE)
data$x6 <- as.factor(data$x6)

# DATA VISUALIZATION
head(data)
summary(data[,-1])
ggcorrplot(cor(data[,-c(1,8)]),lab=TRUE)

# TRANSFORMATION FORCED SO THAT ALL VALUES ARE BETWEEN 0 AND 1
data$y2 <- (data$y-1)/(max(data$y)-1)
data$y2 <- DR_data(data$y2)[,2]

# FREQUENTIST APPROACH
# Beta regression model
# beta.freq <- betareg(y2 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data=data)
# summary(beta.freq)

# BAYESIAN APPROACH
# https://www3.nd.edu/~fliu2/Rjournal_zoib.pdf
beta.bayes <- zoib(y2 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9| 1, data=data,
                  joint=FALSE, random=0, zero.inflation=FALSE, one.inflation=FALSE,
                  n.iter=11000, n.thin=10, n.burn=1000, n.chain=3)
summary(beta.bayes$coeff)

# Posterior samples
beta1 <- as.numeric(c(beta.bayes$coeff[[1]][,2],beta.bayes$coeff[[2]][,2],beta.bayes$coeff[[3]][,2]))
beta2 <- as.numeric(c(beta.bayes$coeff[[1]][,3],beta.bayes$coeff[[2]][,3],beta.bayes$coeff[[3]][,3]))
beta3 <- as.numeric(c(beta.bayes$coeff[[1]][,4],beta.bayes$coeff[[2]][,4],beta.bayes$coeff[[3]][,4]))
beta4 <- as.numeric(c(beta.bayes$coeff[[1]][,5],beta.bayes$coeff[[2]][,5],beta.bayes$coeff[[3]][,5]))
beta5 <- as.numeric(c(beta.bayes$coeff[[1]][,6],beta.bayes$coeff[[2]][,6],beta.bayes$coeff[[3]][,6]))
beta6 <- as.numeric(c(beta.bayes$coeff[[1]][,7],beta.bayes$coeff[[2]][,7],beta.bayes$coeff[[3]][,7]))
beta7 <- as.numeric(c(beta.bayes$coeff[[1]][,8],beta.bayes$coeff[[2]][,8],beta.bayes$coeff[[3]][,8]))
beta8 <- as.numeric(c(beta.bayes$coeff[[1]][,9],beta.bayes$coeff[[2]][,9],beta.bayes$coeff[[3]][,9]))
beta9 <- as.numeric(c(beta.bayes$coeff[[1]][,10],beta.bayes$coeff[[2]][,10],beta.bayes$coeff[[3]][,10]))

# P(beta > 0)
prob <- c(sum(beta1>0)/length(beta1), sum(beta2>0)/length(beta2), sum(beta3>0)/length(beta3),
          sum(beta4>0)/length(beta4), sum(beta5>0)/length(beta5), sum(beta6>0)/length(beta6),
          sum(beta7>0)/length(beta7), sum(beta8>0)/length(beta8), sum(beta9>0)/length(beta9))
names(prob) <- paste0("beta",1:9)
round(prob,2)
