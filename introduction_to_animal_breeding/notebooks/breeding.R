####################################################
## Practicals: general intro to animal breeding   ##
####################################################

library("tidyverse")
library("data.table")

## Combinations ##
## Mendelian sampling: each descendant receives a different 50% of the parental (sire/dam) genes (random samples)
## brain teaser: how many possible 50% combinations with 10 genes?

n = 10
k = (n/2) ## 50%

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
base_folder = "/home/filippo/Documents/ciampolini/unipisa_2023/bioinformatics_and_biostatistics_training/introduction_to_animal_breeding"
weight <- fread(file.path(base_folder, "data/weights.csv"))

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


####################################
### standard error of coefficients
### from: https://www.learnbymarketing.com/tutorials/explaining-the-lm-summary-in-r
####################################
    # is the Residual Standard Error divided by the square root of the sum of 
    # the square of that particular x variable.
## Residual Standard Error: sqrt(SSE/(n-(1+k)))
    # n = sample size
    # k = number of coefficients estimated (except the intercept)
# square root of the sum of the square of that particular x variable deviates 
    # from the mean: sqrt(sum((x-mean(x))^2))

n = length(g$residuals)
k = 1 ## parent-offspring regression coefficient
num = sqrt(sum(g$residuals^2))
common_denom = sqrt(n-(1+k))
specific_denom = sqrt(sum((x-mean(x))^2))

se <- num/(common_denom*specific_denom)
se


#########################
### Another dataset #####
## height
height <- fread(file=file.path(base_folder, "data/height.csv"))

# midParent #
x = NULL
y = NULL

## 1. calculate the heritability
## 2. plot the data and the fitted curve from linear regression
## 3. obtain the standard error of the heritability

## ù############################ ##
## VARIANCE COMPONENTS FROM ANOVA #
## ############################# ##

## half-sib model (example with natural antibody titres)
titres = fread(file.path(base_folder, "data/antibody_titres.csv"))
titres <- titres |> rename(antibodies = `natural antibody titre (u/ml)`)

g <- lm(antibodies ~ sire, data = titres)
anova(g)

ms <- anova(g)
ms_sire = ms$`Mean Sq`[1] ## sigma_e + J*sigma_s
ms_residual = ms$`Mean Sq`[2] ## sigma_e
J = group_by(titres, sire) |> summarise(N = n()) |> summarise(avg_n = mean(N)) |> pull(avg_n)

sigma_e = ms_residual
sigma_s = (ms_sire - sigma_e) / J

## we now have the sire and residual variances

h2 = (4*sigma_s)/(sigma_s + sigma_e)
print(h2)

## EXERCISE ###
## with the following data, estimate h2 from the analysis of variance

#### dog breeds: seconds/400 meters
breed1 <- c(58,56,52,55,62)
breed2 <- c(60,62,68,70,68)
breed3 <- c(61,58,55,64,70)

k = 3
n = 4

breeds <- as.data.frame(
  cbind(
    seq(length(breed1)*k),
    c(rep("B1",times=n),rep("B2",times=n),rep("B3",times=n)),
    c(breed1, breed2, breed3))
)

names(breeds) <- c("dog", "breed", "speed")

## your code here
