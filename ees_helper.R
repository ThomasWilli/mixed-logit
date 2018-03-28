labs <- read.table("https://raw.githubusercontent.com/ThomasWilli/mixed-logit/master/data/qpp8_14.tsv", header=T, sep="\t")
parties <- read.table("https://raw.githubusercontent.com/ThomasWilli/mixed-logit/master/data/parties.tsv", header=T, sep="\t")



label_changer <- function(ees_raw, cntrynr){
  
  labs <- labs %>%
    dplyr::filter(b==cntrynr)
  
 
  ees_temp_pos <- grep(paste0(labs$var_old, collapse="|"), names(ees_raw))
  ees_temp <- ees_raw[,ees_temp_pos]
  
  #Change Variable labels
  for(i in 1:nrow(labs)){
    
    nn <- as.character(labs[i, "var_old"])
    
    pos <- grep(nn, names(ees_temp))
    if(grepl("qpp8", nn)){
      new <- paste0("PTV_", as.character(labs[i, "var_new"]))
    }else{
      new <- paste0("LR_", as.character(labs[i, "var_new"]))
    }
    
    names(ees_temp)[pos] <- new
    
    cat(nn, " --> ", new, "\n")
    
    
  }
  return(ees_temp)
  
  
}

qpp5_changer <- function(ees, country){
  qpp5.df <- parties %>%
    dplyr::filter(b==country)%>%
    dplyr::filter(var=="qpp5")
  
  ees$qpp5 <- as.character(ees$qpp5)
  ees$qpp5 <- as.numeric(ees$qpp5)
  ees$qpp5[ees$qpp5==-9] <- NA
  ees$qpp5[ees$qpp5==-8] <- NA
  ees$qpp5[ees$qpp5==-7] <- NA
  ees$qpp5[ees$qpp5==34] <- NA
  ees$qpp5[ees$qpp5==35] <- NA
  ees$qpp5 <- paste0("QUARK",ees$qpp5)
  
  for(i in nrow(qpp5.df):1){
    
    new <- as.character(qpp5.df[i,"value_new"])
    
    lal <- paste0("QUARK", as.character(qpp5.df[i,"value_old"]))
    ees$qpp5 <- gsub(lal, new, ees$qpp5, fixed=T)
    
    unique(ees$qpp5)
    
  }
  
  ees$qpp5 <- gsub("QUARKNA", NA, ees$qpp5, fixed=T)
  
  return(ees)
}

qpp6_changer <- function(ees, country){
  qpp6.df <- parties %>%
    dplyr::filter(b==country)%>%
    dplyr::filter(var=="qpp6")
  
  ees$qpp6 <- as.character(ees$qpp6)
  ees$qpp6 <- as.numeric(ees$qpp6)
  ees$qpp6[ees$qpp6==-9] <- NA
  ees$qpp6[ees$qpp6==-8] <- NA
  ees$qpp6[ees$qpp6==-7] <- NA
  ees$qpp6[ees$qpp6==34] <- NA
  ees$qpp6[ees$qpp6==35] <- NA
  ees$qpp6 <- paste0("QUARK",ees$qpp6)
  
  for(i in nrow(qpp6.df):1){
    
    new <- as.character(qpp6.df[i,"value_new"])
    
    lal <- paste0("QUARK", as.character(qpp6.df[i,"value_old"]))
    ees$qpp6 <- gsub(lal, new, ees$qpp6, fixed=T)
    
    #unique(ees$qpp6)
    
  }
  
  ees$qpp6 <- gsub("QUARKNA", NA, ees$qpp6, fixed=T)
  
  return(ees)
}

#function to convert PTV into consideration 1/0
ptv_consideration <- function(input) ifelse(input==-99, 0, 
                                  ifelse(input==-8|input==-7|input==-9, NA, input))


issue_cleaner <- function(input) ifelse(input==-8|input==-7|input==-9, NA, input-1)



label_changer_belgium <- function(ees_raw, cntrynr, nuts1){
  
  labs <- labs %>%
    dplyr::filter(b==cntrynr&p7_region_nuts1==nuts1)
  
  ees_raw <- ees_raw %>% filter(p7_region_nuts1==nuts1)
  
  ees_temp_pos <- grep(paste0(labs$var_old, collapse="|"), names(ees_raw))
  ees_temp <- ees_raw[,ees_temp_pos]
  
  #Change Variable labels
  for(i in 1:nrow(labs)){
    
    nn <- as.character(labs[i, "var_old"])
    
    pos <- grep(nn, names(ees_temp))
    if(grepl("qpp8", nn)){
      new <- paste0("PTV_", as.character(labs[i, "var_new"]))
    }else{
      new <- paste0("LR_", as.character(labs[i, "var_new"]))
    }
    
    names(ees_temp)[pos] <- new
    
    cat(nn, " --> ", new, "\n")
    
    
  }
  return(ees_temp)
  
  
}
