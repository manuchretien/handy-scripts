# Script - simulation of data 
# number of replicates necessary to detect effects in LMM
# model here includes two fixed effects (with two levels) and one random effect
# can be customized for other contexts
# by Emmanuelle Chrétien
# May 7, 2018


rm(list=ls())

# Set parameter values
eff.v1<-0.3 # would indicate a 30% increase between level 1 and level 2
eff.v2<-0.05 
nb.replicates<-c(8,10,12,14,16)
var.replicates<-0.1 
y.mean<-4 
y.sd<-1.2 


require(lme4) # for lmer function
require(car) # for Anova function (robust to order of variables, generates p-values)

# source function to generate R2 for mixed effects models
# The function generates from a linear model a table including marginal and conditional R2.
# marginal R2: variance explained by fixed effects only
# conditional R2: variance explained by fixed + random effects
# https://github.com/jslefche/rsquared.glmm/blob/master/rsquaredglmm.R
# Download function and save in personal folder
source(file.choose())

# Create empty matrix for results of simulations
n=NULL
simul=NULL
p.v1=NULL
p.v2=NULL
r2.m=NULL
r2.c=NULL

Results<-c(n,simul,p.v1,p.v2,r2.m,r2.c)

# Loop to generate 100 simulations of data for increasing sample size (nb.replicates). 
for (n in nb.replicates){
  
  for (i in 1:100){
    
    simul<-i
    
    # Calculate variance associated to individual replicate
    replicates<-rnorm(n,0,1)
    col.var.replicates<-rep(var.replicates*replicates,2) # calculated this way as otherwise with rnorm() the variance value too close to zero generated "NaN" in data
    
    # Calculate response values based on estimated effect sizes
    
    # v1 level 1 + v2 level 1:
    x1<-rnorm(n/2,y.mean+(y.mean*eff.v2),y.sd)
    # v1 level 1 + v2 level 2:
    x2<-rnorm(n/2,y.mean,y.sd)
    # v1 level 2 + v2 level 1:
    x3<-rnorm(n/2,y.mean+(y.mean*eff.v1)+(y.mean*eff.v2),y.sd)
    # v1 level 2 + v2 level 2:
    x4<-rnorm(n/2,y.mean+(y.mean*eff.v1),y.sd)
    
    # Create one vector for response variable
    x.all<-c(x1,x2,x3,x4)
    
    # create matrix of data to run model
    mat=matrix(0,nrow=2*n,ncol=4)
    mat[,1]<-x.all+col.var.replicates # response variable
    mat[,2]<-as.factor(c(rep("level1",n),rep("level2",n))) # effect (v1)
    mat[,3]<-as.factor(c(rep("1st",n/2),rep("2nd",n/2),rep("1st",n/2),rep("2nd",n/2))) # effect (v2)
    mat[,4]<-as.factor(rep(seq(1:n),2)) # replicate ID
    
    # run linear mixed model
    M1<-lmer(mat[,1]~mat[,2]+mat[,3]+(1|mat[,4]))
    
    # get p-value for each effect
    p.M1<-Anova(M1)
    
    p.v1<-p.M1[1,3]
    p.v2<-p.M1[2,3]
    
    # get R2
    r2.M1<-rsquared.glmm(M1)
    
    r2.m<-r2.M1[1,4]
    r2.c<-r2.M1[1,5]
    
    # Gather results in table
    Results<-rbind(Results,c(n,simul,p.v1,p.v2,r2.m,r2.c))
    
  }
  
}

# add column names to the data
colnames(Results)<-c("Nb.replicates","simul","p.v1","p.v2","R2.m","R2.c")

# Convert to dataframe format
simul.data<-as.data.frame(Results)
simul.data$Nb.replicates<-as.factor(simul.data$Nb.replicates)

# Compare histograms of p-values for each sample size simulated

for (n in nb.replicates){
  
  title<-paste("n=",n,sep="")
  
  hist(simul.data$p.v1[simul.data$Nb.replicates==n],main=title,breaks=20,xlab="p-value v1")
  
  # Add a vertical line showing the 0.05 threshold 
  abline(v=0.05,col="red",lwd=2)
  
}
