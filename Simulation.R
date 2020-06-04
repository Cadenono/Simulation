##Q1 
set.seed(1)
n = 120; # n participants 
lambda0_f = 5; 
lambda1_f = 10;
lambda0_m = 6;
lambda0_m = 6;
gender = rbinom(120,1,0.5) #Let females = 1 and males = 0 
print(sum(gender)) #tells us number of females 
## [1] 57
gender <- ifelse(gender==1, 'F','M')
age = sample(20:60, 120, replace=TRUE)
table <- data.frame(cbind(gender,age))
tablef <- subset(table,gender == 'F') #table with females only 
tablem <- subset(table,gender == 'M') #table with males only
Y0_f = rpois(57,5)
Y1_f = rpois(57,10)
Y0_m = rpois(63,6)
Y1_m = rpois(63,6)
tablef <- cbind(tablef,Y0_f,Y1_f)
colnames(tablef) <-  c('gender','age','Y(0)','Y(1)')
tablem <- cbind(tablem,Y0_m,Y1_m)
colnames(tablem) <- c('gender','age','Y(0)','Y(1)')
table <- rbind(tablef,tablem)
table$index <- as.numeric(row.names(table))
table <- table[order(table$index), ]
table[1:10,] #first 10 rows of PO matrix
summary(table)
print( mean(Y1_f)-mean(Y0_f) ) #tau for females
print( mean(Y1_m)-mean(Y0_m) ) #tau for males

##Q2i
set.seed(1)
n = 120; # n participants 
lambda0_f = 5; 
lambda1_f = 10;
lambda0_m = 6;
lambda0_m = 6;
gender = rbinom(120,1,0.5) #Let females = 1 and males = 0 
print(sum(gender))
## [1] 57
gender <- ifelse(gender==1, 'F','M')
age = sample(20:60, 120, replace=TRUE)
table <- data.frame(cbind(gender,age))
p1=2/3
n1=n*p1
n0=n-n1
labels= c(rep(1,n1), rep(0,n0)) # cards indicating treatments
Z = sample(labels, n, replace=FALSE) #shuffle the cards
table <- cbind(table,Z) #we get a table with gender,age,Z

tablef <- subset(table,gender == 'F') #subset to females only
tablem <- subset(table,gender == 'M') #subset to males only
Y0_f = rpois(57,5)
Y1_f = rpois(57,10)
Y0_m = rpois(63,6)
Y1_m = rpois(63,6)


tablef <- cbind(tablef,Y0_f,Y1_f)
colnames(tablef) <-  c('gender','age','Z','Y(0)','Y(1)')
tablem <- cbind(tablem,Y0_m,Y1_m)
colnames(tablem) <- c('gender','age','Z', 'Y(0)','Y(1)')
Y.obs.female = rep(NA,57);
Y.obs.male = rep(NA,63)

Y.obs.female[Z==1] = Y1_f[Z==1]
Y.obs.female[Z==0] = Y0_f[Z==0]
Y.obs.male[Z==1] = Y1_m[Z==1]
Y.obs.male[Z==0] = Y0_m[Z==0]

Y.obs.female <- Y.obs.female[!is.na(Y.obs.female)]
Y.obs.male <- Y.obs.male[!is.na(Y.obs.male)]
tablef <- cbind(tablef, Y.obs.female)
colnames(tablef) <- c('gender','age','Z','Y(0)','Y(1)','Y.obs')
tablem <- cbind(tablem,Y.obs.male)
colnames(tablem) <- c('gender','age','Z','Y(0)','Y(1)','Y.obs')
table <- rbind(tablef,tablem)
table$index <- as.numeric(row.names(table))
table <- table[order(table$index), ]
sampleZ <- function(i){
  set.seed(i)
  Z. = sample(Z, n, replace=FALSE)
  return(Z.)
}

gender <- table$gender 
computeB <- function(gender,Z.){
  prob = sum(gender=='F' & Z.==1)/ sum(Z.==1)
  return(prob)
}

computeC <- function(Z.){
  expect = mean(age[Z.==1])
  return(expect)
}

Y0 <- table[,4] #assign Y0 to Y(0) in the table
Y1 <- table[,5] #assign Y0 to Y(1) in the table
computeD <- function(Y0,Y1, Z.){
  Ybar.obs.0 = sum((1-Z.)*Y0)/n0;
  Ybar.obs.1 = sum(Z.*Y1)/n1;
  d. =  Ybar.obs.1 -  Ybar.obs.0; 
  #print(d.)
  return(d.)
}


computeMed <- function(Y0,Y1, Z.){
  d. =  median(Y1[Z.==1]) -  median(Y0[Z.==0]);
  return(d.)
}
nRep = 10000

Z.mat = sapply(1:nRep, sampleZ); 
str(Z.mat); 
##  num [1:120, 1:10000] 1 1 0 0 0 1 1 1 0 0 ...
T.apply1 = apply(Z.mat,2,function(x) computeB(gender,x));
summary(T.apply1);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.3500  0.4500  0.4750  0.4748  0.5000  0.5875
var(T.apply1);
## [1] 0.00105225
hist(T.apply1, main="Distribution of P(Female|Z=1)");
T.apply2 = apply(Z.mat, 2, function(x) computeC(x)); 
summary(T.apply2);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   39.14   41.52   42.08   42.08   42.64   45.24
var(T.apply2);
## [1] 0.6659786
hist(T.apply2, main="Distribution of E(Age|Z=1)");
T.apply3 = apply(Z.mat, 2, function(x) computeD(Y0,Y1,x)); 
summary(T.apply3);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.900   1.925   2.163   2.163   2.413   3.513
var(T.apply3);
## [1] 0.1228639
hist(T.apply3, main="Distribution of mean(Yobs(1)) - mean(Yobs(0))");
T.apply4 = apply(Z.mat,2, function(x) computeMed(Y0,Y1,x));
summary(T.apply4);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   1.000   1.500   1.606   2.000   3.500
var(T.apply4);
## [1] 0.2890131
hist(T.apply4, main="Distribution of med(Yobs(1)) - med(Yobs(0))");


##Q2ii Blocked randomization with n=12, trt assignment 2:1 
labels = c(rep(1,8),rep(0,4))
temp = sapply(1:10, function(x) sample(labels,12,replace=FALSE))
Z = matrix(temp, ncol=1)
table <- cbind(table,Z)
Y.obs.male = rep(NA,63)
Y0_f = rpois(57,5)
Y1_f = rpois(57,10)
Y0_m = rpois(63,6)
Y1_m = rpois(63,6)
tablef <- cbind(tablef,Y0_f,Y1_f)
Y.obs.female = rep(NA,57)
Y.obs.female[tablef$Z==0] = Y0_f[tablef$Z==0]
Y.obs.female[tablef$Z==1] = Y1_f[tablef$Z==1]
tablef <- cbind(tablef,Y.obs.female)
colnames(tablef) <- c('gender','age','Z','Y(0)','Y(1)','Y.obs')
tablem <- cbind(tablem,Y0_m,Y1_m)
Y.obs.male = rep(NA,57)
Y.obs.male[tablem$Z==0] = Y0_m[tablem$Z==0]
Y.obs.male[tablem$Z==1] = Y1_m[tablem$Z==1]
tablem <- cbind(tablem,Y.obs.male)
colnames(tablem) <- c('gender','age','Z','Y(0)','Y(1)','Y.obs')
table <- rbind(tablef,tablem)
table$index <- as.numeric(row.names(table))
table <- table[order(table$index), ]
#table[1:10,]
sampleZ <- function(i){
  set.seed(i)
  Z. = sample(Z, n, replace=FALSE)
  return(Z.)
}

gender <- table$gender 
computeB <- function(gender,Z.){
  prob = sum(gender=='F' & Z.==1)/ sum(Z.==1)
  return(prob)
}

computeC <- function(Z.){
  expect = mean(age[Z.==1])
  return(expect)
}
Y0 <- table[,4] #assign Y0 to Y(0) in the table
Y1 <- table[,5] #assign Y0 to Y(1) in the table
n1 = sum(Z==1)
n0 = n - n1 
computeD <- function(Y0,Y1, Z.){
  Ybar.obs.0 = sum((1-Z.)*Y0)/n0;
  Ybar.obs.1 = sum(Z.*Y1)/n1;
  d. =  Ybar.obs.1 -  Ybar.obs.0; 
  #print(d.)
  return(d.)
}


computeMed <- function(Y0,Y1, Z.){
  d. =  median(Y1[Z.==1]) -  median(Y0[Z.==0]);
  return(d.)
  
} 

nRep = 10000

Z.mat = sapply(1:nRep, sampleZ); 
str(Z.mat); 
##  num [1:120, 1:10000] 1 0 1 1 1 1 0 1 0 1 ...
T.apply1 = apply(Z.mat,2,function(x) computeB(gender,x));
summary(T.apply1);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.3625  0.4500  0.4750  0.4753  0.5000  0.5875
var(T.apply1);
## [1] 0.001065963
hist(T.apply1, main="Distribution of P(Female|Z=1)");
T.apply2 = apply(Z.mat, 2, function(x) computeC(x)); 
summary(T.apply2);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   38.35   41.55   42.09   42.10   42.65   45.14
var(T.apply2);
## [1] 0.6752807
hist(T.apply2, main="Distribution of E(Age|Z=1)");
T.apply3 = apply(Z.mat, 2, function(x) computeD(Y0,Y1,x)); 
summary(T.apply3);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.650   1.562   1.750   1.757   1.950   2.763
var(T.apply3);
## [1] 0.08170382
hist(T.apply3, main="Distribution of mean(Yobs(1)) - mean(Yobs(0))");
T.apply4 = apply(Z.mat,2, function(x) computeMed(Y0,Y1,x));
summary(T.apply4);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   1.000   1.000   1.298   1.500   3.000
var(T.apply4);
## [1] 0.2593506
hist(T.apply4, main="Distribution of med(Yobs(1)) - med(Yobs(0))");

##Q3i Coin toss with P(treatment) = 2/3
#Selection criterion: accept Z if Pr(Female | Z=1) > 0.45

set.seed(1)
n = 120; # n participants 
lambda0_f = 5; 
lambda1_f = 10;
lambda0_m = 6;
lambda0_m = 6;
gender = rbinom(120,1,0.5) #Let females = 1 and males = 0 
print(sum(gender))
## [1] 57
gender <- ifelse(gender==1, 'F','M')
age = sample(20:60, 120, replace=TRUE)
table <- data.frame(cbind(gender,age))
p1=2/3
n1=n*p1
n0=n-n1
labels= c(rep(1,n1), rep(0,n0)) # cards indicating treatments 
Z = sample(labels,size=n,replace=FALSE) #shuffle the cards since we want the assignment to be random
table <- cbind(table,Z) #we get a table with gender,age,Z

tablef <- subset(table,gender == 'F') #subset to females only
tablem <- subset(table,gender == 'M') #subset to males only
Y0_f = rpois(57,5)
Y1_f = rpois(57,10)
Y0_m = rpois(63,6)
Y1_m = rpois(63,6)


tablef <- cbind(tablef,Y0_f,Y1_f)
colnames(tablef) <-  c('gender','age','Z','Y(0)','Y(1)')
tablem <- cbind(tablem,Y0_m,Y1_m)
colnames(tablem) <- c('gender','age','Z', 'Y(0)','Y(1)')
Y.obs.female = rep(NA,57);
Y.obs.male = rep(NA,63)

Y.obs.female[Z==1] = Y1_f[Z==1]
Y.obs.female[Z==0] = Y0_f[Z==0]
Y.obs.male[Z==1] = Y1_m[Z==1]
Y.obs.male[Z==0] = Y0_m[Z==0]

Y.obs.female <- Y.obs.female[!is.na(Y.obs.female)]
Y.obs.male <- Y.obs.male[!is.na(Y.obs.male)]
tablef <- cbind(tablef, Y.obs.female)
colnames(tablef) <- c('gender','age','Z','Y(0)','Y(1)','Y.obs')
tablem <- cbind(tablem,Y.obs.male)
colnames(tablem) <- c('gender','age','Z','Y(0)','Y(1)','Y.obs')
table <- rbind(tablef,tablem)
table$index <- as.numeric(row.names(table))
table <- table[order(table$index), ]
Z <- table$Z 
sampleZ <- function(i){
  set.seed(i)
  Z. = sample(Z, n, replace=FALSE)
  return(Z.)
}

gender <- table$gender 
computeB <- function(gender,Z.){
  prob = sum(gender=='F' & Z.==1)/ sum(Z.==1)
  return(prob)
}

computeC <- function(Z.){
  expect = mean(age[Z.==1])
  return(expect)
}

Y0 <- table[,4] #assign Y0 to Y(0) in the table
Y1 <- table[,5] #assign Y0 to Y(1) in the table
n1 <- sum(Z==1)
n0 <- n - n1

computeD <- function(Y0,Y1, Z.){
  Ybar.obs.0 = sum((1-Z.)*Y0)/n0;
  Ybar.obs.1 = sum(Z.*Y1)/n1;
  d. =  Ybar.obs.1 -  Ybar.obs.0; 
  #print(d.)
  return(d.)
}


nRep = 10000

Z.mat = sapply(1:nRep, sampleZ); 
str(Z.mat); 
##  num [1:120, 1:10000] 1 1 0 0 0 1 1 1 0 0 ...
T.apply1 = apply(Z.mat,2,function(x) computeB(gender,x))
vec <- c() 
for (i in indexes){
  vec <- c(vec,Z.mat[,i])
}
vec <- matrix(vec, nrow=120, byrow=FALSE)

T.apply1 = apply(vec,2,function(x) computeB(gender,x))
summary(T.apply1)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.4625  0.4750  0.4875  0.4903  0.5000  0.5875
var(T.apply1)
## [1] 0.0005192288
hist(T.apply1, main="Distribution of P(Female|Z=1)")
T.apply2 = apply(vec,2,function(x) computeD(Y0,Y1,x))
summary(T.apply2)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.900   1.962   2.200   2.193   2.438   3.513
var(T.apply2)
## [1] 0.1193444
hist(T.apply2, main="Distribution of mean(Yobs(1))-mean(Yobs(0))")


library("purrr")
names(T.apply1) <- seq_along(T.apply1)
T.apply1 <- keep(T.apply1, T.apply1 > 0.45)
indexes = as.numeric(names(T.apply1))
cat('The acceptance ratio is: ', length(indexes) / 10000)


