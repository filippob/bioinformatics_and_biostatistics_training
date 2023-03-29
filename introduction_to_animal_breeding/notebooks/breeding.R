################################################
##Esercizi corso animal breeding di base   ##
################################################

library("tidyverse")
library("data.table")

## Combinazioni ##

n = 10
k = (n/2)

factorial(n)/(factorial(k)*factorial(n-k))

(10*9*8*7*6)/factorial(5)

choose(n,k)

###################
## Single record ##
###################

## phenotypes (e.g. dog weights in hectograms)
y <- c(320,250,380,200,210,240,280,330,325,290);

## genetic std dev
sdg <- 36; #hg

var_a <- sdg^2	#additive genetic variance
var_p <- var(y)	#phenotypic variance

## HERITABILITY
h2 = var_a/var_p;	#h2

## EBV of one animal
ebv1 <- (y[1]-mean(y))*h2

## EBVs of all animals
ebv <- (y-mean(y))*h2;

## The EBVs sum to 0: why?
sum(ebv)

## Accuracy of EBVs
r_ay <- cov(ebv,y)/(sdg*sd(y))
sqrt(h2)
##########################################


##########################################
## PARENT - OFFSPRING REGRESSION	    ##	 
##########################################

### PARENT MIDPOINT (example from the slides)

weight <- fread("data/weights.csv")

x = weight$`parent mid-point`
y = weight$kid_kg

sum((x - mean(x)) * (y - mean(y)))
sum((x-mean(x))^2)

cov_xy = cov(x,y)
var_x = var(x)

h2 = cov_xy/var_x

g <- lm(kid_kg ~ `parent mid-point`, data=weight)
summary(g)

plot(x,y,type="p",main="Parent-offspring regression",xlab="Average weight of parents",ylab="Weight of offspring")
abline(g)


# standard error
# sqrt(SSE/(n-(1+k)))
# k = number of coefficients estimated (except the intercept)
# divided by the square root of the sum of the square of that particular x variable deviates from the mean
n = length(g$residuals)
k = 1 ## parent-offspring regression coefficient
num = sqrt(sum(g$residuals^2))
common_denom = sqrt(n-(1+k))
specific_denom = sqrt(sum((x-mean(x))^2))

se <- num/(common_denom*specific_denom)
se


### Another dataset #####
## height
height <- fread(file="data/height.csv")

# midParent #
x = NULL
y = NULL

## 1. calculate the heritability
## 2. plot the data and the fitted curve from linear regression
## 3. obtain the standard error of the heritability








