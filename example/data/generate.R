##############
#  SIMULATION  #
##############

#Start R code preparation
#First maximise memory allocation to ensure that you do not run out of space in R
#memory.limit(4095)

#For convenience, start by setting up file names ahead of time
#Note that by convention slashes in path statements in R are forward not backward
DC1.data.file<-"Study.1.csv"
DC2.data.file<-"Study.2.csv"
DC3.data.file<-"Study.3.csv"
DC4.data.file<-"Study.4.csv"
DC5.data.file<-"Study.5.csv"
DC6.data.file<-"Study.6.csv"
AC.beta.vector<-"beta.vector.csv"
ALL.data.file<-"Study.ALL.csv"

#SET UP DATA STRUCTURE
#Random number seed so others can precisely repeat analysis on their own implementation of R
set.seed(1028)
#Specify study sizes and generate IDs for studies and individuals
numsubs.study<-c(2000,3000,1500,300,2000,700)
numsubs<-sum(numsubs.study)
numstudies<-length(numsubs.study)
study.id<-rep(1:numstudies,numsubs.study)
id<-c(1:numsubs.study[1], 1:numsubs.study[2], 1:numsubs.study[3],
           1:numsubs.study[4], 1:numsubs.study[5], 1:numsubs.study[6])

#SET UP MODEL STRUCTURE AND PARAMETERS
#Number of and values of regression coefficients
numpara<-4
beta0<--0.3
beta.bmi<-0.02
beta.bmi456<-0.04
beta.snp<-0.5
#Minor allele frequency
MAF<-0.3

#SIMULATE DATA
#Generate covariates
bmi<- rnorm(numsubs,mean=23,sd=4)-23
bmi456<-c(rep(0,6500),bmi[6501:9500])
snp<-rbinom(numsubs,2,MAF)
#Generate linear predictor and equivalent probabilities of response
lp<-beta0 + beta.bmi*bmi  +beta.bmi456*bmi456 + beta.snp*snp
probresp<-exp(lp)/(1+exp(lp))
#Randomly sample case control status
CC<-rbinom(numsubs,1,probresp)

#ASSEMBLE AND WRITE OUT COMPLETE DATA SET
all.data<-data.frame(study.id,id,CC,bmi,snp,bmi456)
write.csv(all.data,file=ALL.data.file,row.names=FALSE)

#PREPARE AND WRITE OUT DATA FILES FOR EACH STUDY INDIVIDUALLY
Study<-list()
Study[[1]]<-all.data[study.id==1,]
write.csv(Study[[1]],file=DC1.data.file,row.names=FALSE)
Study[[2]]<-all.data[study.id==2,]
write.csv(Study[[2]],file=DC2.data.file,row.names=FALSE)
Study[[3]]<-all.data[study.id==3,]
write.csv(Study[[3]],file=DC3.data.file,row.names=FALSE)
Study[[4]]<-all.data[study.id==4,]
write.csv(Study[[4]],file=DC4.data.file,row.names=FALSE)
Study[[5]]<-all.data[study.id==5,]
write.csv(Study[[5]],file=DC5.data.file,row.names=FALSE)
Study[[6]]<-all.data[study.id==6,]
write.csv(Study[[6]],file=DC6.data.file,row.names=FALSE)

