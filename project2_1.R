library(tidyr)
library(dummies)
library(lubridate)
library(zoo)

#library(tidyverse)
setwd("Projects/Project2")
options(scipen = 999)


conversion_df = read.csv("project2.conversion.11.csv")
incident_df = read.csv("project2.incident.11.csv")


View(conversion_df)
View(incident_df)

######Cleaning Conversion File###########
date = as.Date(conversion_df[,1], "%Y%m%d")
conversion_df$renewal_date <- as.Date( as.character( conversion_df$renewal_date ), format = "%Y%m%d" )

# # of rows in co
nrow(conversion_df) #16066
View(conversion_df)

head(conversion_df,5)

names(conversion_df)[names(conversion_df) == 'Service.Agreement.Id'] <- 'SAID'
conversion_df = na.omit(conversion_df)

class(conversion_df[1,1])

require(data.table)
DT <- data.table(conversion_df)
NDT = unique(DT[order(renewal_date)], by="SAID", fromLast=TRUE)
View(NDT)
nrow(NDT) # 15765
length(unique(NDT$SAID))

# Make dataframe of table
c = data.frame(rbind(NDT))
conversion_df = c
View(conversion_df)

# c = separate(c, "renewal_date", c("Year", "Month", "Day"), sep = "-")

#############Cleaning Inc File############

incident_df$Day_Name <- rep(1,nrow(incident_df))

incident_df$calendar_yearmonth = as.Date(paste(incident_df$Year_Name, incident_df$Month_Name, incident_df$Day_Name, sep='-'))


merged_df = merge(x = incident_df, y = conversion_df, by = "SAID", all = TRUE)

merged_df$tcp_t = (as.yearmon(strptime(merged_df$renewal_date, format="%Y-%m-%d"))-as.yearmon(strptime(merged_df$calendar_yearmonth,format="%Y-%m-%d")))*12
View(merged_df)
nrow(merged_df) #54448


####### REmove NA and 0 
merged_df  = merged_df[merged_df$tcp_t>0,] # 48753 rows

# Remove NAs
merged_df = na.omit(merged_df)

# Convert loss to 0 & convert as 1
merged_df$status = dummy(merged_df$status)[,1]
View(merged_df)




###########
rows = nrow(merged_df)


############ ###############


hyptest<- function(urModel, rModel){
  lrt = 2*(-(urModel$objective)-(-rModel$objective)) ; lrt
  
  pval = 1- pchisq(lrt,df = 1); pval
  return(pval)
}


############################
# Unrestricted Model
modelUR <- function(par,data){
  a=par[1]
  b=par[2]
  beta = par[3]
  ge = par[4]
  gv = par[5]
  gm = par[6]
  gp=par[7]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)

    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                  log(1+ tmp$number_of_Cases)*
                 (1+ge*log(1+ tmp$number_of_escalation) +
                  gv * log(1+ tmp$number_of_single_visit_missed) + 
                    gm * log(1+tmp$number_of_response_missed)+
                    gp * log(1+tmp$number_of_parts_used))
                 )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
srt = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
#srt = c(-1.0, 0.07,0.23,  1.2, 3.3 , 5.2) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
urMod =nlminb(start = srt, objective = modelUR, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
urMod

###################################

# a Restricted Model
modelRa <- function(par,data){
  #a=par[1]
  
  b=par[1]
  beta = par[2]
  ge = par[3]
  gv = par[4]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  #z = a+b*ex
  z = b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.9, 0.024,0.32,  -6.2, 12.1, 3.2) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
raMod =nlminb(start = srt, objective = modelRa, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
raMod


print(paste("UR  and restircted a", hyptest(urMod, raMod)))
##############################

# beta restricted Model
modelRbeta <- function(par,data){
  a=par[1]
  
  b=par[2]
  #beta = par[3]
  ge = par[3]
  gv = par[4]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(    (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,  1.2, 3.3 , 5.2,1.0) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rbetaMod =nlminb(start = srt, objective = modelRbeta, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
rbetaMod

# ge Restricted Model
modelRge <- function(par,data){
  a=par[1]
  
  b=par[2]
  beta = par[3]
  #ge = par[4]
  gv = par[4]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,0.23, 3.3 , 5.2,1.0) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgeMod =nlminb(start = srt, objective = modelRge, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
rgeMod

###################################

# gm restricted Model
modelRgm <- function(par,data){
  a=par[1]
  
  b=par[2]
  beta = par[3]
  ge = par[4]
  gv = par[5]
  #gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed)+
                      gp * log(1+ tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,0.23,  1.2, 5.2,1.2) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgmMod =nlminb(start = srt, objective = modelRgm, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
rgmMod

###################################

# gv restricted Model
modelRgv <- function(par,data){
  
  a=par[1]
  b=par[2]
  beta = par[3]
  ge = par[4]
  #gv = par[6]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}


#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,0.23,  1.2, 5.2,2.1) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgvMod =nlminb(start = srt, objective = modelRgv, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
rgvMod

###################################

# gp Restricted
modelRgp <- function(par,data){
  a=par[1]
  b=par[2]
  beta = par[3]
  ge = par[4]
  gv = par[5]
  gm = par[6]
  #gp=par[7]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
srt = c(0.1,0.1,0.1,0.1,0.1,0.1)
#srt = c(-1.0, 0.07,0.23,  1.2, 3.3 , 5.2) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgpMod =nlminb(start = srt, objective = modelRgp, data = merged_df[1:rows,], control = list(eval.max=5000, iter.max=5000, trace=2))
rgpMod



print(paste("UR  and restircted a", hyptest(urMod, raMod)))
print(paste("UR  and restircted beta",hyptest(urMod, rbetaMod)))
print(paste("UR  and restircted ge",hyptest(urMod, rgeMod)))
print(paste("UR  and restircted gv",hyptest(urMod, rgvMod)))
print(paste("UR  and restircted gm",hyptest(urMod, rgmMod)))
print(paste("UR  and restircted gp",hyptest(urMod, rgpMod)))

urMod

