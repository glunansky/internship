###################################
## Watershed Depression Project  ##
## Version 1                     ##  
## Gaby Lunansky                ##
###################################

require("dplyr")
require("lavaan")
require("semPlot")

load("esmdata.RData")

## SELECT & RECODE AFFECT-STATES ## 
data.af<- esm[,c(1,13:43)]                             # Subjnr & affect-state variables     

for (i in 2:ncol(data.af)){                            # Recode data. Character factors to numeric 
  data.af[,i] <- as.integer(data.af[,i])
}


## SELECT SUBJECTNRS ## 

length(unique(data.af[,1]))                            # 12011 unique subject numbers

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

##### Definitieve data 
#"subjnr"   "piekerde" "opgewkt"  "onzeker"  "ontspann" "boosgei"  "tevreden" "energiek" "nlekker" 
# "voegejaa" "rustig"   "enthous" "dep1" "psy1" "par1" "ang1" "dep1now" "dep1past" 

data <- esm[,c(1, 16, 17, 18, 20, 22, 24, 35, 36, 37, 38, 42, 118, 119, 120, 121, 165, 166)]
dim(data)

for (i in 2:ncol(data)){                            # Recode data. Character factors to numeric 
  data[,i] <- as.integer(data[,i])
}

data<- data %>% group_by(subjnr) %>% filter(n() > 1) # Selecting only subjecnrs with > 1 observation, we now have 603 unique subjectnrs
data <- as.data.frame(data)

length(unique(data$subjnr))    




## BUILD SEM MODELS
## Level 1: Measurement model for depression (fits reasonably)



DPmodel <-
  '
deplv =~ dep1 + psy1 +  par1 + ang1 
'

fitDPmodel <- cfa(DPmodel, data=data, estimator="MLM", se='robust', std.lv = TRUE, std.ov = TRUE)

summary(fitDPmodel, fit.measures=TRUE, standardized = TRUE)


## one factor model for affect-state variables (fits poorly)
ASonefactormodel <-
  '
aslv=~ piekerde + opgewkt + onzeker + ontspann + boosgei + tevreden + energiek + nlekker
+ voegejaa + rustig + enthous
'
fitASonefactormodel <- cfa(ASonefactormodel, data=data,estimator='MLM',se='robust',std.lv=T,std.ov=TRUE)
summary(fitASonefactormodel,fit.measures=TRUE,standardized=T)


# Watershed / MIMIC model
MIMIC <-
'
depression =~ dep1 + psy1 +  par1 + ang1 
depression ~ piekerde + opgewkt + onzeker + ontspann + boosgei + tevreden + energiek + nlekker
+ voegejaa + rustig + enthous
'
fitMIMIC <- cfa(MIMIC, data=data, estimator="MLM", se='robust', std.lv=T, std.ov=T)
summary(fitMIMIC, fit.measures=T, standardized=T, rsquare=T)

# Diagram of the model
semPaths(fitMIMIC,what="diagram",rotation=3,edge.label.cex = 0.5, edge.color="black", curvePivot=T, exoCov=F,nCharNodes=0, intercepts=F, residuals=F)

# The fitted model
semPaths(fitMIMIC,what="std",rotation=3,edge.label.cex = 0.5, curvePivot=T, exoCov=F,nCharNodes=0, intercepts=F, residuals=F)

