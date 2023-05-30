library(tidyverse)
library(readxl)
library(metafor)
library(multcomp)
library(dplyr)
library(readxl)
getwd()
setwd()
rm(list=ls())


### Importing and cleaning data 
olddata <- read_excel("SCED-1994-2014.xlsx")
newdata <- read_excel("SCED_2014-2019.xlsx")


### Making sure dataframes match before combining them
setdiff(newdata,olddata)
olddata$time = olddata$time_
olddata$time_ = NULL
olddata$MT = 0

### Combining datasets
alldata = rbind(olddata, newdata)


### Removing cases that do not have baseline phases
netdata = subset(alldata, idstudy != "1" & idstudy != "16" & idstudy != "20" & idstudy != "33")


### Running an OLS regression to calculate effect sizes for each series (using non-standardised scores)
out = with(netdata,
           by(netdata, idseries, function(x) lm(scores ~ 1 + condition, data=x)))

### Extracting the coefficients from the regression output
outsum = lapply(out, summary)
coefmat = sapply(outsum, coef)
coeffs = t(coefmat)


### Extracting the residual standard deviation from the output
sigma = sapply(outsum, `[`, 'sigma')
sigma = as.data.frame(sigma)
sigma = t(sigma)  #corces the element into a matrix, so needs to be transformed into dataframe again
sigma = as.data.frame(sigma)

install.packages("tibble")
library(tibble)
### Joining both coefficients and sigma into one dataframe
coeffs = as_tibble(coeffs, rownames = "idseries", .name_repair = "unique")
coeffs
coeffs$sigma = sigma$V1
View(coeffs)
### Standardising the coefficients by dividing them by the corresponding residual standard deviations
coeffs$stdES = coeffs$...2/coeffs$sigma

### Adding the effect sizes (b1)  to the main dataframe
metadat = netdata %>%
  distinct(idseries, .keep_all = TRUE)
?installed.packages()
metadat$stdES = coeffs$stdES


#Adding the comparator for each series. Since all included studies only compare treatments with the baseline, this is what we get:
metadat$comparator = rep("baseline", each=259)

### Calculating standard errors based on Van den Noortgate and Onghena (2008)

library(dplyr)
data_sample_size<-netdata %>% 
  group_by(idstudy,idcase,idseries,condition) %>%
  summarise(n = n())

data_sample_size$condition[data_sample_size$condition == 0]<- "baseline"
data_sample_size$condition[data_sample_size$condition == 1]<- "treat"
data_sample_size$condition[data_sample_size$condition == 2]<- "treat"
data_sample_size$condition[data_sample_size$condition == 3]<- "treat"
data_sample_size$condition[data_sample_size$condition == 9]<- "treat"

c<-data_sample_size %>% 
  group_by(idstudy,idcase,idseries,condition) %>%
  summarise(num = sum(n))
c <- c[!(is.na(c$condition)),]
data_wide <- spread(c, condition,num)
metadat<-merge(data_wide, metadat, by= c("idstudy", "idcase", "idseries"))

metadat$var<-(metadat$baseline + metadat$treat)/(metadat$baseline * metadat$treat) + (metadat$stdES^2/ (2*(metadat$baseline + metadat$treat)))
metadat$stder<-sqrt(metadat$var)

#######      Performing the Network Meta-Analysis  #######

### Creating dummies for types of intervention#
metadat$ABA = ifelse(metadat$typeint == "ABA", 1, 0)
metadat$IMI = ifelse(metadat$typeint == "IMI", 1, 0)
metadat$SRI = ifelse(metadat$typeint == "SRI", 1, 0)
metadat$DEV = ifelse(metadat$typeint == "DEV", 1, 0)


###  Subseting the dataset so that only the dependent variable attention is included
metadat_att<-metadat[which(metadat$dependvar=="joint attention" | metadat$dependvar=="attentiveness" | metadat$dependvar== "focused attention"),]


#For some studies, more than one effect size could be extracted. To avoid data dependency,
#only the first observation was selected per case

mydata <- metadat_att[with(metadat_att, do.call(order, list(idstudy,idcase))), ]

metadat_att<-do.call(rbind, by(mydata, list(mydata$idstudy, mydata$idcase), 
                               FUN=function(x) head(x, 1)))


###Centering continuous variables
metadat_att$c_weeks<-scale(metadat_att$weeks, center = TRUE, scale = FALSE)
metadat_att$c_age<-scale(metadat_att$age, center = TRUE, scale = FALSE)
metadat_att$c_sessions<-scale(metadat_att$sessions, center = TRUE, scale = FALSE)

#In two observations, the intervention took place both at home and at a clinic
#To simplifythis situation, we just chose one setting per participant (in this case, the clinic)
metadat_att$settinghome[c(11,12)]<-0

#Creating two variables (number of settings - 1) using effect coding to analyze "setting" variable
metadat_att$c1_class = ifelse(metadat_att$settingclass == 1, 1, 
                         ifelse(metadat_att$settinghome == 1, 0, 
                                ifelse(metadat_att$settingclinic == 1, -1 ,0)))


metadat_att$c2_home = ifelse(metadat_att$settingclass == 1, 0, 
                              ifelse(metadat_att$settinghome == 1, 1, 
                                     ifelse(metadat_att$settingclinic == 1, -1,0)))


#Some interventions were carried out by two agents (Parents AND reseracher, and parents AND professor).
#To simplify, we just chose one agent per participant (in this case, we chose researcher and proffesor)

metadat_att$actorparent[c(11:13)]<-0
metadat_att$actorparent[c(17:21)]<-0

#Creating three variables (number of agents - 1) using effect coding to analyze "agent"variable
metadat_att$c1_parent = ifelse(metadat_att$actorparent == 1, 1, 
                              ifelse(metadat_att$actorresearch == 1, 0, 
                                     ifelse(metadat_att$actorteach == 1, 0 ,
                                            ifelse(metadat_att$actorprof == 1, -1 ,0))))
metadat_att$c2_research = ifelse(metadat_att$actorparent == 1, 0, 
                               ifelse(metadat_att$actorresearch == 1, 1, 
                                      ifelse(metadat_att$actorteach == 1, 0 ,
                                             ifelse(metadat_att$actorprof == 1, -1 ,0))))

metadat_att$c3_actorteach = ifelse(metadat_att$actorparent == 1, 0, 
                                 ifelse(metadat_att$actorresearch == 1, 0, 
                                        ifelse(metadat_att$actorteach == 1,1 ,
                                               ifelse(metadat_att$actorprof == 1, -1 ,0))))


#Null model: model without moderator varibles

netma_null = rma.mv(stdES, var, 
               mods= ~ -1 + ABA + DEV + IMI + SRI,
               random = list(~ 1 | idstudy/idcase),
               data=metadat_att,
               rho = 1/2,
               method = "ML")

netma_null

#Model including age as covariate

netma_age = rma.mv(stdES, var, 
                    mods= ~ -1 +ABA + DEV + IMI + SRI + c_age,
                    random = list(~ 1 | idstudy/idcase),
                    data=metadat_att,
                    rho = 1/2,
                    method = "ML")

netma_age 
# Log-likelihood ratio test
anova(netma_null, netma_age)

#Model including weeks as covariate
netma_weeks = rma.mv(stdES, var, 
                    mods= ~ ABA + DEV + IMI + SRI + c_weeks -1,
                    random = list(~ 1 | idstudy/idcase),
                    data=metadat_att,
                    rho = 1/2,
                    method = "ML")

netma_weeks
# Log-likelihood ratio test
anova(netma_null, netma_weeks)

#Model including sessions as covariate
netma_sessions = rma.mv(stdES, var, 
                     mods= ~ ABA + DEV + IMI + SRI + c_sessions -1,
                     random = list(~ 1 | idstudy/idcase),
                     data=metadat_att,
                     rho = 1/2,
                     method = "ML")

netma_sessions
# Log-likelihood ratio test
anova(netma_null, netma_sessions)

#Model including settings as covariate

netma_setting = rma.mv(stdES, var, 
                    mods= ~ -1 +ABA + DEV + IMI + SRI + c1_class +c2_home  ,
                    random = list( ~ 1 | idstudy/idcase),
                    data=metadat_att,
                    rho = 1/2,
                    method = "ML")

netma_setting 
# Log-likelihood ratio test
anova(netma_null, netma_setting)

#Model including agent as covariate  
netma_agent = rma.mv(stdES, var, 
                       mods= ~ -1 +ABA + DEV + IMI + SRI + c1_parent +c2_research + c3_actorteach ,
                       random = list(~ 1 | idstudy/idcase),
                       data=metadat_att,
                       rho = 1/2,
                       method = "ML")
netma_agent

# Log-likelihood ratio test
anova(netma_null, netma_agent)
#All covariates *except for agent* in the model

netma_all = rma.mv(stdES, var, 
                     mods= ~ -1 +ABA + DEV + IMI + SRI +c_age + c_weeks + c_sessions + c1_parent +c2_research + c3_actorteach,
                     random = list(~ 1 | idstudy/idcase),
                     data=metadat_att,
                     rho = 1/2,
                     method = "ML")
netma_all
# Log-likelihood ratio test
anova(netma_null, netma_all)


###   Ranking the treatments   ###

#Performing Pairwise Comparisons First#
contr = contrMat(setNames(rep(1,(netma_null$p)), colnames(netma_null$X)), type="Tukey")
sav = predict(netma_null, newmods=contr)
sav$pval = anova(netma_null, L=contr)$pval
sav<-as.data.frame(sav)
sav$name<-rownames(contr)
sav<-sav[!grepl("c1|c2|c3|weeks|sessions|age", sav$name),]
    
#Indicating which treatments are being compared
  sav$i =  c("DEV", "IMI", "SRI",  "IMI", "SRI","SRI")
  sav$j = c("ABA", "ABA", "ABA",  "DEV", "DEV", "IMI")
    
  sav$beta_i = ifelse(sav$i == "ABA", netma_null$b["ABA",], 
                    ifelse(sav$i == "DEV", netma_null$b["DEV",],
                          ifelse(sav$i == "IMI", netma_null$b["IMI", ],
                                ifelse(sav$i == "SRI", netma_null$b["SRI",], ""))))
  
  sav$beta_j = ifelse(sav$j == "ABA", netma_null$b["ABA",], 
                      ifelse(sav$j == "DEV", netma_null$b["DEV",],
                            ifelse(sav$j == "IMI", netma_null$b["IMI", ],
                                  ifelse(sav$j == "SRI", netma_null$b["SRI",], ""))))
   
#calculating the one-sided pvalue
  sav$pij = ifelse(sav$beta_i <= sav$beta_j, sav$pval/2, 1-sav$pval/2)

#saving the pvalue according to intervention
  p_ABA = ifelse(sav$i == "ABA", sav$pij,
               ifelse(sav$j == "ABA", 1-sav$pij, ""))
  p_DEV = ifelse(sav$i == "DEV", sav$pij,
               ifelse(sav$j == "DEV", 1-sav$pij, ""))
  p_IMI = ifelse(sav$i == "IMI", sav$pij,
               ifelse(sav$j == "IMI", 1-sav$pij, ""))  
  p_SRI = ifelse(sav$i == "SRI", sav$pij,
               ifelse(sav$j == "SRI", 1-sav$pij, ""))
    
  pscore = cbind(as.numeric(p_ABA),
                   as.numeric(p_DEV),
                   as.numeric(p_IMI),
                   as.numeric(p_SRI))
    
  #calculating the pscore
  pavg_ABA = mean(pscore[,1], na.rm = TRUE)
  pavg_DEV = mean(pscore[,2], na.rm = TRUE)
  pavg_IMI = mean(pscore[,3], na.rm = TRUE)
  pavg_SRI = mean(pscore[,4], na.rm = TRUE)   
  
  pavg = rbind(pavg_ABA, pavg_DEV, pavg_IMI, pavg_SRI)   
 
    
    

#Plotting the network  
  
  
 library(igraph)
    cases = c(36,9, 16, 3, 8) # Corresponds to following interventons: c("BASELINE", "ABA", "IMI", "SRI", "DEV")
    studies = c(9, 16, 3,8) #No baseline because it's an attribute of comparisons (edges of network)
    V(net)$cases = cases
    E(net)$studies = studies
    net = graph (edges = c("BASELINE","ABA", 
                           "BASELINE","DEV", 
                           "BASELINE","IMI", 
                           "BASELINE","SRI"), 
                 directed = FALSE)
    
    E(net)$width = studies
    V(net)$size = cases
  
#plot the network - distance between nodes will be manually adjusted for asthetic purposes
  tkplot(net, layout = layout.fruchterman.reingold(net),
           label.cex = 2, vertex.label.dist = 0, label.degree = "2pi",
           vertex.label.color = "black", vertex.color = "#4796ca", edge.color = "lightgray", 
           vertex.frame.color = "#4796ca",
           frame.color = "purple", edge.label = studies, edge.label.color = "#666666",
           main = "Nertwork of Early Interventions for Autism Spectrum Disorder")
  
  
#Making the forest plot#
    cf<-coef(summary(netma_null))
    cf<-cf[c(1:4), ]
    library(forestplot)
    for.net = cf
    for.data = cbind(
      mean = c(NA, 0.1749, 0.0745 , 0.0429,-0.0702),
      lower = c(NA, -0.0332 ,-0.1967, -0.2431, -0.4756),
      upper = c(NA,0.3830, 0.3456 , 0.3289 , 0.3352))
    
    tabletext = cbind(
      c("", "ABA", "DEV", "IMI", "SRI"),
      c("Total cases", "9", "16", "3", "8"),
      c("Effect & 95% CI", "0.17 [-0.03, 0.38]", "0.07 [-0.20, 0.35]", "0.04 [-0.24, 0.33]", "-0.07 [-0.48, 0.34]")
    )

    forestplot(tabletext,graph.pos = 4,
               boxsize = 0.2,
               for.data, new_page = TRUE, txt_gp = fpTxtGp(cex=1),  xticks = c(-.48, -0.20, 0, 0.20, 0.40), title="Relative Intervention Effects")
    