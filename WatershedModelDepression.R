###################################
## Watershed Depression Project  ##
## Version 1                     ##  
## Gaby Lunansky, 20-05-2017     ##
###################################

require("dplyr")

load("esmdata.RData")

## SELECT & RECODE AFFECT-STATES ## 
data.af<- esm[,c(1,13:43)]                             # Subjnr & affect-state variables     

for (i in 2:ncol(data.af)){                            # Recode data. Character factors to numeric 
  data.af[,i] <- as.integer(data.af[,i])
}


## SELECT SUBJECTNRS ## 

length(unique(data.af[,1]))                            # 12011 unique subject numbers...

data.af<- data.af %>% group_by(subjnr) %>% filter(n() > 1)
data.af <- as.data.frame(data.af)

length(unique(data.af$subjnr))                         # Selecting only subjecnrs with > 1 observation, we now have 603 unique subjectnrs


## MISSINGS ##

data.af[,2:32] %>% summarise_each(funs(sum(is.na(.)))) # Check NA's per variable

missings <- data.af[,2:32] %>% summarise_each(funs(sum(is.na(.))))

# make vector with % missings per column
v<- matrix(NA,1,31)
colnames(v) <- colnames(data.af[,2:32])

for (i in 1:length(missings)){
  v[i]<- missings[[i]] / length(data.af[,i+1]) * 100
}

v                                                  # Check % of missings per variable

which(v > 95)                                      # gewoon, droevig, erbij globalql & gespanne all have > 95% missings

## CHECK VARIANCE PER VARIABLE ##

sort(apply(data.af[,2:ncol(data.af)],2,var, na.rm=T))    # Sort variance per variable, low to high
                                                   # Schuldig, Wantrou, droevig, angstig, eenzaam & sober have variance < 1

datadef<- data.af[, - c(4,8,10,12,14,15, 19,28,29,32)] # Proposal: removing variables with > 95% missings and variables with < 1 variance
ncol(datadef) - 1                                  # Left with 21 affect-state variables
names(datadef)

##### Definitieve affect metingen:

data <- datadef[,-c(2,3,10,11,12,13,14,15,16,21)]
View(esm)
names(data)


#### depressie variabelen toevoegen

datadep <- esm[,c(1,118:142)]

for (i in 2:ncol(datadep)){                            # Recode data. Character factors to numeric 
  datadep[,i] <- as.integer(datadep[,i])
}

View(datadep)


## sem modellen
library("lavaan")

#Reflective one-factor model for affect states: Fits poorly
ASonefactormodel <-
  '
aslv=~ piekerde + opgewkt + onzeker + ontspann + boosgei + tevreden + energiek + nlekker
        + voegejaa + rustig + enthous
'
fitASonefactormodel <- cfa(ASonefactormodel, data=data,estimator='MLM',se='robust',std.lv=T,std.ov=TRUE)
summary(fitASonefactormodel,fit.measures=TRUE,standardized=T)









