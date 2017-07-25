###################################
## Watershed Depression Project  ##
## Version 3                     ##  
## Gaby Lunansky                 ##
###################################

require("dplyr")
require("lavaan")
require("semPlot")

load("esmdata.RData")

# First, prepare dataset
## RECODE AFFECT-STATES ## 
data <- esm[,c(1, 16, 17, 18, 20, 22, 24,35, 36, 37, 38, 42, 118:141,165, 166)]
names(data)                      

for (i in 2:22){                                      # Recode data. Character factors to numeric 
  data[,i] <- as.integer(data[,i])
}


## SELECT SUBJECTNRS ## 

length(unique(data[,1]))                            # 12011 unique subject numbers

data<- data %>% group_by(subjnr) %>% filter(n() > 1)
data<- as.data.frame(data)

length(unique(data$subjnr))                         # Selecting only subjecnrs with > 1 observation, we now have 603 unique subjectnrs


# Take mean measurements per subject number (one row for each subject)

data <- aggregate(data[, 2:ncol(data)], list(data$subjnr), mean, na.rm=T)
View(data)

## MISSINGS ##

data %>% summarise_each(funs(sum(is.na(.)))) # Check NA's per variable

missings <- data %>% summarise_each(funs(sum(is.na(.))))
missings/603 * 100

# Translate the affect-state variabeles to english
colnames(data)[2:12] <- c("worrying","cheerful","unsure", "relaxed","angirr","satisfied","energy","nwell","agitated","quiet","enthus")

## BUILD SEM MODELS
## Check 1: Measurement model for depression 
## Check which measurement moment of depression symptoms fits data best (Dep2)

DPmodel2 <-
  '
depression =~ dep2 + psy2 + par2 + ang2
'

fitDP2 <- cfa(DPmodel2, data=data, estimator="MLM", se='robust', std.lv = TRUE, std.ov = TRUE)
summary(fitDP2, fit.measures=TRUE, standardized = TRUE) # Chi-sq < .01, CFI = .977, RMSEA = .133

DPmodel3 <-
  '
depression =~ dep3 + psy3 + par3 + ang3
'

fitDP3 <- cfa(DPmodel3, data=data, estimator="MLM", se='robust', std.lv = TRUE, std.ov = TRUE)
summary(fitDP3, fit.measures=TRUE, standardized = TRUE) # Chi-sq = 0.004 CFI = .989, RMSEA = .092

DPmodel4 <-
  '
depression =~ dep4 + psy4 + par4 + ang4
'

fitDP4 <- cfa(DPmodel4, data=data, estimator="MLM", se='robust', std.lv = TRUE, std.ov = TRUE)
summary(fitDP4, fit.measures=TRUE, standardized = TRUE) # Chi-sq < .001, CFI = .986, RMSEA = .137

DPmodel5 <-
  '
depression =~ dep5 + psy5 + par5 + ang5
'

fitDP5 <- cfa(DPmodel5, data=data, estimator="MLM", se='robust', std.lv = TRUE, std.ov = TRUE)
summary(fitDP5, fit.measures=TRUE, standardized = TRUE) # Chi-sq < .001, CFI = .979, RMSEA = .173


# Check 2: one factor model for affect-state variables 
# Should fit poorly, and fits poorly (p < .001, CFI = .74, RMSEA = .251)
ASonefactormodel <-
  '
aslv=~  worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus
'
fitASonefactormodel <- cfa(ASonefactormodel, data=data,estimator='MLM',se='robust',std.lv=T,std.ov=TRUE)
summary(fitASonefactormodel,fit.measures=TRUE,standardized=T)

# Check 3: Reflective one factor model for depression 1
# Should fit poorly but fits reasonably (CFI = .992, RMSEA =.094 )

DepOnly1 <-
  '
deplv =~ dep1 + psy1 +  par1 + ang1 
'

fitDPmodel <- cfa(DepOnly1, data=data, estimator="MLM", se='robust', std.lv = TRUE, std.ov = TRUE)

summary(fitDPmodel, fit.measures=TRUE, standardized = TRUE)

# Level 1 MIMIC model

MIMIC1 <- '
depression =~ dep2 + psy2 + par2 + ang2
depression ~ dep1 + psy1 + par1 + ang1
'
fitMIMIC1 <- cfa(MIMIC1, data=data,estimator='MLM',se='robust',std.lv=T,std.ov=TRUE)
summary(fitMIMIC1,fit.measures=TRUE,standardized=T)

# Watershed model

Watershed <- 
  '
depression =~ dep3 + psy3 + par3 + ang3
depression ~ dep1 + psy1 + par1 + ang1

dep1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

psy1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

par1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

ang1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

'

fitWS <- cfa(Watershed, data=data,estimator='MLM',se='robust',std.lv=T,std.ov=TRUE)
summary(fitWS,fit.measures=TRUE,standardized=T)

## Watershed post-hoc: add residual covariances for depression symptoms and affect-states

WatershedRV <- 
  '
depression =~ dep3 + psy3 + par3 + ang3
depression ~ dep1 + psy1 + par1 + ang1

dep1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

psy1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

par1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

ang1 ~ worrying + cheerful + unsure + relaxed + angirr + satisfied + energy + nwell + agitated + quiet +  enthus

dep1~~psy1
dep1~~par1
dep1~~ang1

psy1~~par1
psy1~~ang1

par1~~ang1

worrying ~~ cheerful
worrying ~~ unsure
worrying ~~ relaxed
worrying ~~ angirr
worrying ~~ satisfied 
worrying ~~ energy
worrying ~~  nwell
worrying ~~  agitated
worrying ~~  quiet
worrying ~~ enthus

cheerful ~~ unsure
cheerful ~~ relaxed
cheerful ~~ angirr
cheerful ~~ satisfied 
cheerful ~~ energy
cheerful ~~  nwell
cheerful ~~  agitated
cheerful ~~  quiet
cheerful ~~ enthus

unsure ~~ relaxed
unsure ~~ angirr
unsure ~~ satisfied 
unsure ~~ energy
unsure ~~  nwell
unsure ~~  agitated
unsure ~~  quiet
unsure ~~ enthus

relaxed ~~ angirr
relaxed ~~ satisfied 
relaxed ~~ energy
relaxed ~~  nwell
relaxed ~~  agitated
relaxed ~~  quiet
relaxed ~~ enthus

angirr ~~ satisfied 
angirr ~~ energy
angirr ~~  nwell
angirr ~~  agitated
angirr ~~  quiet
angirr ~~ enthus

satisfied ~~ energy
satisfied ~~  nwell
satisfied ~~  agitated
satisfied ~~  quiet
satisfied ~~ enthus

energy ~~  nwell
energy ~~  agitated
energy ~~  quiet
energy ~~ enthus

nwell ~~  agitated
nwell ~~  quiet
nwell ~~ enthus

agitated ~~  quiet
agitated ~~ enthus

quiet ~~ enthus

'

fitWSRV <- cfa(WatershedRV, data=data,estimator='MLM',se='robust',std.lv=T,std.ov=TRUE)
summary(fitWSRV,fit.measures=TRUE,standardized=T)

#plot

m = matrix(nrow = 5, ncol = 11)
m[1, ] = c(1,rep(0,2),2,rep(0,3),3,rep(0,2),4)
m[2, ] = c(rep(0,5),20,rep(0,5))
m[3, ] = c(5,rep(0,2),6,rep(0,3),7,rep(0,2),8)
m[4, ] = rep(0,11)
m[5, ] = 9:19


semPaths(fitWSRV,
         what="std", 
         whatLabels = "invisible", 
         layout=m, 
         edge.label.cex = 0.5,
         intercepts=F, 
         residuals=F,
         sizeMan = 6,
         sizeLat = 10,
         nCharNodes = 10)

