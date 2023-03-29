################################################
## Practicals: R base + Stats base            ##
################################################
## SOME PLOTS ##

library("ggplot2")
library("tidyverse")

# simple plots with base R
data("ChickWeight")
head(ChickWeight)

plot(ChickWeight$Time, ChickWeight$weight)

with(ChickWeight,
	plot(Time, weight, xlab="days", ylab="body weight", main="Chick weight over time", pch=15, col="blue")
);

# add interpolation
chick.fit <- lm(weight ~ Time, data = ChickWeight)
abline(chick.fit)

hist(ChickWeight$weight, breaks = 10) 	# function hist for numeric data

diets <- table(ChickWeight$Diet)
barplot(diets)		# barplot for factors/characters/table output
colors = c("red", "yellow", "violet", "orange") 
barplot(diets,    # apply the hist function 
   horiz=TRUE,    # barplot verticale o orizzontale
   col=colors,     # set the color palette 
   main="Distribution of diets in the ChickWeight dataset", # the main title 
   xlab="Frequency")

# plots in ggplot2
data("esoph")
head(esoph)

ggplot(esoph, aes(x = alcgp)) + geom_bar(aes(fill=tobgp), position = "stack")
ggplot(esoph, aes(x = alcgp)) + geom_bar(aes(fill=tobgp), position = "dodge")

ggplot(esoph, aes(x = alcgp, y = ncases)) + geom_jitter(aes(color=alcgp))

dd <- ChickWeight |> 
  group_by(Time) |>
  summarise(avg = mean(weight))

p <- ggplot(dd, aes(x = Time, y = avg, group=1)) + geom_point()
p <- p + geom_line()
p <- p + ylab("average weight")
p

d1 <- esoph |> 
  group_by(alcgp) |>
  summarise(avg = mean(ncases)) |>
  rename(consumption = alcgp) |>
  mutate(risk_factor = "alcohol", consumption = as.character(consumption))

d2 <- esoph |> 
  group_by(tobgp) |>
  summarise(avg = mean(ncases)) |>
  rename(consumption = tobgp) |>
  mutate(risk_factor = "tobacco", consumption = as.character(consumption))

dd <- d1 |> bind_rows(d2)

p <- ggplot(dd, aes(x = consumption, y = avg, group=1)) + geom_point()
p <- p + geom_line()
p <- p + facet_wrap(~risk_factor)
p <- p + ylab("n. of cases")
p <- p + theme(axis.text.x = element_text(angle = 90))
p

## Let's look at ratios !!

d1 <- esoph |> 
  group_by(alcgp) |>
  summarise(avg = mean(ncases/(ncases+ncontrols))) |>
  rename(consumption = alcgp) |>
  mutate(risk_factor = "alcohol", consumption = as.character(consumption))

d2 <- esoph |> 
  group_by(tobgp) |>
  summarise(avg = mean(ncases/(ncases+ncontrols))) |>
  rename(consumption = tobgp) |>
  mutate(risk_factor = "tobacco", consumption = as.character(consumption))

dd <- d1 |> bind_rows(d2)

p <- ggplot(dd, aes(x = consumption, y = avg, group=1)) + geom_point()
p <- p + geom_line()
p <- p + facet_wrap(~risk_factor)
p <- p + ylab("n. of cases")
p <- p + theme(axis.text.x = element_text(angle = 90))
p

## heatmap
dd <- esoph |> 
  group_by(alcgp,tobgp) |>
  summarise(avg = mean(ncases/(ncases+ncontrols)))

ggplot(dd, aes(x = alcgp, y = tobgp, fill=avg)) + geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red")

##########################################
## MEASURES OF LOCATION AND VARIABILITY ##

# arithmetic mean
mean(ChickWeight$weight)

ChickWeight |>
  group_by(Diet) |>
  summarise(avg = mean(weight))


## write your own function
media <- function(x) {
  sumx = sum(x)
  n = length(x)
  ## mean = sum(x)/n
	media = sumx/n
  return(media)
}

mean(ChickWeight$weight)
media(ChickWeight$weight)

ChickWeight |>
  group_by(Diet) |>
  summarise(avg = media(weight))

# problemchen
# mu <- 40;
# 
# sum(x) <- 40*n;
# 
# 42 <- (40*n + 50 + 64)/(n+2);
# 
# 42*n + 42*2 = 40*n + 50 + 64
# 
# 2*n = 30
# n = 15

#weighted average (price/kg and fish kg)
pA <- 5.5;  ##
pM <- 7.5;  ##
pS <- 9.25; ##
pT <- 10.75; 

kgA <- 250;
kgM <- 210;
kgS <- 125;
kgT <- 60;

wt <- c(kgA,kgM,kgS,kgT)
x <- c(pA,pM,pS,pT);

(pA*kgA+pM*kgM+pS*kgS+pT*kgT)/(kgA+kgM+kgS+kgT)
weighted.mean(x,wt);

### EXERCISE FOR YOU
## try to write your own custom function to calculate the weighted average


###########################################################################

# geometric mean
 
gmean <- function(v) {

  v = v[!is.na(v)];
  
  if (!is.numeric(v)) {
	print("Argument must be numeric"); return(NULL)
  }
  else if (any(v <= 0)) {
	print("All elements must be positive"); return(NULL)
  }
  
  #else return(prod(v)^(1/length(v)))
  else return(exp(mean(log(v))))
}

# median
bw = ChickWeight$weight
ordered_weight = sort(bw)
median(ordered_weight)

### #odd sequences
temp = sample(x = bw, size = 333)
temp = sort(temp)
temp[(length(temp)+1)/2] 				

#### even sequences
temp = sample(x = bw, size = 300)
temp = sort(temp)
(temp[length(temp)/2]+temp[(length(temp)/2)+1])/2 #sequenze pari

# quantiles
quantile(ChickWeight$weight,0.5)	#median
quantile(ChickWeight$weight,0.25)	#first quartile
quantile(ChickWeight$weight,0.10)	#first decile
quantile(ChickWeight$weight,0.01)	#first percentile
quantile(ChickWeight$weight,0.05)	#fifth percentile

par()->OriginalParameters;
par(mfrow=c(2,1))
hist(ChickWeight$weight,col="51")
boxplot(ChickWeight$weight,col="51",horizontal=T)
par(OriginalParameters);	#restore initial graphical settings

boxplot(
  ChickWeight$weight,
	col = "orange", 
	pch = 1,			#empty circles
	horizontal=TRUE,
 	main = "Chick weight",
	xlab="Body weight in grams",
	outline = TRUE,
	border = "red",
	range = 1.5,		#whiskers' length (0 gives max length)
	boxwex = 0.25,		#box dimension
	plot = TRUE
)

# mode
abs(sort(-table(ChickWeight$weight)))	#not mode(), but table()
abs(sort(-table(ChickWeight$weight)))[1]	

## READ EXTERNAL FILES 
library("here")
library("data.table")

base_folder = "/home/filippo/Documents/ciampolini/unipisa_2023/bioinformatics_and_biostatistics_training/introduction_to_animal_breeding"
data_url = "https://zenodo.org/record/7056828/files/milk_records.gz" 
outfile = file.path(base_folder,"data/milk_records.gz")
system2("wget", paste(data_url, "-O", outfile))
milk = fread(outfile)

head(milk)

### EXERCISE
### Calculate mean, median, mode for yield_tot, SCC and DSCC


#############################################################

#trend

as.Date(milk$calving_date, format = "%Y-%m-%d")
milk$year <- format(milk$calving_date, format = "%Y")

dd <- milk |>
  mutate(year = factor(year)) |>
  filter(!is.na(year)) |>
  group_by(year, breed) |>
  summarise(avg_scc = mean(SCC))

ggplot(dd, aes(year, avg_scc, group=breed)) + geom_line(aes(color = breed)) + xlab("Year") + ylab("SCC")


##############################
# MEASURES OF VARIABILITY   #
##############################

# same mean, different range
bw1 = c(72,76,74)
bw2 = c(59,92,71)

#same range, different dispersion
ab1 <- c(1.5,7.0,7.0,7.0,7.0,7.0,7.0,7.0)
ab2 <- c(1.5,4.0,4.8,5.5,6.0,6.5,6.6,7.0)
ab3 <- c(1.5,2.3,2.2,1.7,3.5,3.0,3.1,7.0)


# variance and standard dev
var(ChickWeight$weight)
sd(ChickWeight$weight)

#draw gaussians with different sigmas
plot(density(rnorm(50000,0,10)),type="l",ylim = c(0,0.5))
lines(density(rnorm(50000,0,8)))
lines(density(rnorm(50000,0,5)))
lines(density(rnorm(50000,0,2)))
lines(density(rnorm(50000,0,1)))


#standardization
round(rnorm(1,6,3),1) # R course at UniPi
round(rnorm(1,8.5,1.5),1) # R course at Stanford University

# is 7 at UinPi more or less worth than 8.5 at Stanford?
zunip = (7-6)/3;	
zstanf = (8-8.5)/1.5;

#changing scale
summary(milk$SCC)
massimo = max(milk$SCC)
minimo = min(min$SCC)

a = 1
b = 100

scala <- function(x,a,b) { 

	massimo<-max(x);
	minimo<-min(x);

	return((((b-a)*(x-minimo))/(massimo-minimo))+a)
}

rescaled_scc = scala(milk$SCC, a, b)
summary(rescaled_scc)

### the scale function
scale(x = milk$SCC, center = FALSE, scale = TRUE)

##########################
# coefficient of variation

sd(ChickWeight$weight)/mean(ChickWeight$weight)
sd(milk$SCC)/mean(milk$SCC)

cv <- function(vect) {

	cv = sd(vect)/mean(vect)
	return(cv)
}

# covariance
years = c(3,4,4,2,5,3,4,5,3,2)
grade = c(57,78,72,58,89,63,73,84,75,48)
plot(years, grade, pch=16, col="red", main="Relationship between years of study and grades", xlab="Study years", ylab="Grade");

devx = years-mean(years)
devy = grade-mean(grade)
n = length(years)-1
sum(devx*devy)/n

cov(years,grade)

# correlation
cov(years,grade)/(sd(years)*sd(grade))
cor(years,grade)

cor(ChickWeight$weight,ChickWeight$Time)
cor(milk$SCC, milk$DSCC)


#### read dog data
dogs <- fread(file.path(base_folder, "data/pheno.dat"))
head(dogs)

long_dogs <- dogs |>
  gather(key = "trait", value = "score", -c(id,sex))

long_dogs |>
  group_by(sex, trait) |>
  summarise(avg = mean(score)) |>
  spread(key = sex, value = avg)

dd <- select(dogs, -c(id,sex)) |> cor()

library("corrplot")
corrplot(dd)

############################
# 	ANOVA		   #
############################
Fert1 <- c(71,75,65,69)
Fert2 <- c(90,80,86,84)
Fert3 <- c(72,77,76,79)

# overall mean
GrandMean = (length(Fert1)*mean(Fert1)+length(Fert2)*mean(Fert2)+length(Fert2)*mean(Fert3))/(length(Fert1)+length(Fert2)+length(Fert3))

(mean(Fert1)+mean(Fert2)+mean(Fert3))/3

k = 3
n = 4

#total sum of squares
sst = sum((Fert1-GrandMean)^2,(Fert2-GrandMean)^2,(Fert3-GrandMean)^2);

sst/(k*n-1)
var(c(Fert1,Fert2,Fert3))

sstr = n*(
	(mean(Fert1)-GrandMean)^2+
	(mean(Fert2)-GrandMean)^2+
	(mean(Fert3)-GrandMean)^2
);

sse = sum((Fert1-mean(Fert1))^2,(Fert2-mean(Fert2))^2,(Fert3-mean(Fert3))^2);

sst == sstr+sse

df1 = (k-1);
df2 = k*(n-1);

mstr = sstr/df1
mse = sse/df2

Fstat = mstr/mse

qf(0.99,df1,df2)	#quantile for alpha=0.01
1-pf(Fstat,df1,df2)	#probability


#prepare data for lm()

fertilizers <- as.data.frame(
cbind(
 seq(length(Fert1)*k),
 c(rep("F1",times=n),rep("F2",times=n),rep("F3",times=n)),
 c(Fert1, Fert2, Fert3))
);

names(fertilizers)<-c("ID","FERT","YIELD");
fertilizers <- mutate(fertilizers, YIELD = as.numeric(YIELD))

g <- lm(YIELD ~ FERT, data=fertilizers);
anova(g);


################################
# LINEAR  REGRESSION      		 #
################################

# 32275*b = 4055 - 35*a
# b = (4055 - 35*a) / 32275
# 
# 4617544 = 32275*a + 37987805*((4055 - 35*a) / 32275)
# 
# 32275*a  + ((37987805*35)/32275)*a = (37987805*4055)/32275 - 4617544
# 
# 73470.14*a = 155207.3
# a = 155207.3/73470.14
# b = (4055 - 35*a) / 32275

data(Orange)
head(Orange)

plot(Orange$age, Orange$circumference)

mean(Orange$circumference)
mean(Orange$age)

############################
## circumference = a + b*age
############################
lm(circumference ~ age, data=Orange)


X <- cbind(rep(1, nrow(Orange)), Orange$age)
y = Orange$circumference

XX <- t(X) %*% X
Xy = t(X) %*% y

b = solve(XX) %*% Xy
b
