rm(list=ls(all=T))
library(rstan)
library(dplyr)
library(mlogit)

#read in data
toydata <- readRDS("data/toydata.Rds")

#get data into correct format
TM1 = mlogit.data(toydata, choice = "lastvote", shape = "long",  alt.var = "mode")

f = mFormula(lastvote ~ choice1 + choice2 | Age + Dist)

M = model.matrix(f, TM1)

#create index vector of viable alternatives
n=1
Z <- list()
for (r in unique(toydata$id)) { # respondents
  scenario <- toydata[toydata$id==r,]
  #Z[[n]] <- scenario$viable
  Z[[n]] <- as.numeric(scenario$viable!=1)
  n <- n + 1
}



#set up data for stan
y = as.numeric(TM1[TM1$lastvote, "alt"])

datlist <- list(N=length(y),    
                J=max(y), 
                K=ncol(M),
                x=M,
                y=y,
                Z=Z)              

#fit model
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
out.stan <- stan(file="stan/mnl_cl_v2.stan", data=datlist,
                 control = list(stepsize=0.01, 
                                adapt_delta=0.99)) 

#print(out.stan, digits = 3)

fit1 <- out.stan


extracter <- function(x){
  summary_all <- summary(x)$summary %>% as.data.frame
  summary_all$Parameter <- as.factor(gsub("\\[.*]", "", rownames(summary_all)))
  
  betas <- summary_all[grep("beta", row.names(summary_all), fix=T),] %>%
    select(mean, sd, `2.5%`, `97.5%`, n_eff)%>%
    mutate(effekt = colnames(M))
  return(betas)
}

a <- extracter(out.stan)
b <- extracter(fit1)
pos <- grep("Dist",a$effekt)
a[pos,]
b[pos,]









