rm(list=ls(all=T))
library(rstan)
library(dplyr)

toydata <- read.table("mixed-logit/data/toydata.csv", header=T, sep=",")

R <- length(unique(toydata$id)) #Respondents
X <- list()
X2 <- list()
Z <- list()
choice <- list()
n <- 1
for (r in unique(toydata$id)) { # respondents
  scenario <- toydata[toydata$id==r,]
  X[[n]] <- scenario %>% select(A, B, E, D) %>% data.matrix
  X2[[n]] <- scenario %>% select(choice1, choice2) %>% data.matrix
  
  choice[[n]] <- scenario$choice
  Z[[n]] <- scenario$viable
  n <- n + 1
}

datlist <- list(N=length(X),      #number of individuals
                C=nrow(X[[1]]),   #number of alternatives 
                K=ncol(X[[1]]),   #number of covariates per individual
                KC=ncol(X2[[1]]), #number of covariates per alternative
                X=X,              #matrix of individual covariates
                X2=X2,            #matrix of alternative covariates
                choice=choice,    #final choice
                Z=Z)              #indicator of consideration

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
neth.stan <- stan(file="mixed-logit/stan/mnl_cl.stan", data=datlist,
                  control = list(stepsize=0.01, 
                                 adapt_delta=0.99)) 


















