############################################
### NMA of REPLICATED AB PHASE DESIGN ######
############################################

#------------------------------------------------------------------
#meta-data
  #Authors: Le Grange JJP & Van den Noortgate Wim 
  #version 1 
  #last updated: 
    (last_update <- Sys.time()) #"2023-05-09 16:46 CEST"
  #who updated: Le Grange, J
#------------------------------------------------------------------



# clear workspace and install packages--------- 
rm(list=ls())

inst_packages<-c("tidyverse","readxl","metafor","multcomp","dplyr","here")

for(i in inst_packages){
  if(!i %in% rownames(installed.packages())){
    install.packages(i)
  }
  library(i,character.only=T)
}


# ii. set working directory.

  #Note: if working directory is not set and the functions file 
  #      "230416_thesis_functions_jlg.R" in line 37



setwd("C:/Users/Student/OneDrive/Knowledge Management/Projects/thesis-I-PCKS-L-4306/chapter 2/Data analysis/Thesis Simulation")




# iii. load functions-------

functions_jlg<-"C:/Users/Student/OneDrive/Knowledge Management/Projects/thesis-I-PCKS-L-4306/chapter 2/Data analysis/Thesis Simulation/230416_thesis_functions_jlg.R"

thesis_function <- source(functions_jlg)

# 2. A PRIORI DEFINED SIMULATION CONDITion

smd1<-.2 # smd between treatment a and b

smd2<-.5 # smd between treatmetn b and c

smd3<-.7 # smd between treatment a and c 



#let's assume that we have an equal number of studies that compares 
#conditions a with b, b with c, and a with c 

n_ab<-5 #number of studies a and b
n_ac<-5 #number of studies a and c
n_bc<-5 #number of studies b and c

n_studies<-n_ab+n_ac+n_bc

np_1<-5 #PHASE 1:number of measurement occasions
np_2<-15#PHASE 2:number of measurement occasions

n_cases <- 3


#autocor + mpn (number of timepoints) predefined. 

mpn=20 #NB think of ways in which I can be able to change of these values to be realistic in accordance to previous studies, meta-analysis, and/or simulations. Decide on at least 3. 
a=.2




# 1. SIMULATING 1 DATA SET------ 
## 1.1 studies a vs b-----------

  #define the number of cases (participants)
  # Create a list to store the data of Study 1 which compares study A with Study B. 
  l_ab<-list()
    
  # Loop through the number of cases and generate a time series and corresponding dataset for each case
  for (study_ab in 1:n_ab){
      ab_dat_list <- list()
      for (i in 1:n_cases) {
        timeseries<-create_ts(mpn,a)
        ab_dat <- create_dat_ab(timeseries) # Generate a dataset for
        #a single case by using the function 
        # (create_dat_ab)
        # which take the timeseries for a single
        # participant and assigns the strict
        # phase lengths to it. 
        
        # Add the dataset to the list
        ab_dat_list[[i]] <- ab_dat
      }
      l_ab [[study_ab]]  <- ab_dat_list
  }

print(l_ab)
  
  
# Create an empty dataframe 

  df_ab <- data.frame() 
  case_no <- 1 #case counter for the for loop
  study_no<-1  #study counter for the for loop

set.seed (12)  

# for loop

for (i in 1:length(l_ab)){
    study <- l_ab[[i]]
    # Loop through each case in the study
    for (c in seq_along(study)){
      case <- study[[c]]
      # Create a data frame with the measurements and scores
      df_case <- data.frame(study_id = study_no,
                            case_id = case_no,
                            condition = case[[1]],
                            score = case[[2]])
      # Append the case data frame to the main data frame
      df_ab <- bind_rows(df_ab, df_case)
      case_no <- case_no + 1
      if(case_no %% n_cases == 1){
        study_no=study_no+1
      } else {
        study_no=study_no
      }
    }
  }


study_no
colnames(df_ab)=c("study_id","case_id", "condition_id","score")

#some checks for myself:
  #head(df_ab)
  #View(df_ab)
  unique(df_ab$study_id)
df_ab  



## 1.2. studies b vs c ------

#create an emptly list wherein data will be stored 
l_bc<-list()


set.seed (13)  

#generate the data of studies comparing conditions b and c 
for (study_bc in 1:n_bc){
    bc_dat_list <- list()
    for (i in 1:n_cases) {
      timeseries<-create_ts(mpn,a)
      bc_dat <- create_dat_bc(timeseries) # Generate a dataset for
                                          #a single case by using the function 
                                          # (create_dat_ab)
                                          # which take the timeseries for a single
                                          # participant and assigns the strict
                                          # phase lengths to it. 
                                          # Add the dataset to the list
      bc_dat_list[[i]] <- bc_dat
    }
    l_bc[[study_bc]]  <- bc_dat_list
}


l_bc
  
# create data frame from this list 
  
# Create an empty dataframe 
df_bc <- data.frame() 
case_no      #case counter for the for loop
             # study number need not be initialised again, we continue with the previous 
             # number 


# for loop creating a dataframe from the list above

for (i in 1:length(l_bc)){
  study <- l_bc[[i]]
  # Loop through each case in the study
  for (c in seq_along(study)){
    case <- study[[c]]
    # Create a data frame with the measurements and scores
    df_case <- data.frame(study_id = study_no,
                          case_id = case_no,
                          condition = case[[1]],
                          score = case[[2]])
    # Append the case data frame to the main data frame
    df_bc <- bind_rows(df_bc, df_case)
    case_no <- case_no + 1
    if(case_no %% n_cases == 1){
      study_no=study_no+1
    } else {
      study_no=study_no
    }
  }
}
colnames(df_bc)=c("study_id","case_id", "condition_id","score")
df_bc

#some checks for myself:
  head(df_bc)
  unique(df_bc$study_id)

## 1.3. studies a vs c-----

l_ac<-list()

#generate the data of studies comparing conditions b and c 

for (study_ac in 1:n_ac){
  ac_dat_list <- list()
  for (i in 1:n_cases) {
    timeseries<-create_ts(mpn,a)
    ac_dat <- create_dat_ac(timeseries) # Generate a dataset for
                                        # a single case by using the function 
                                        # (create_dat_ab)
                                        # which take the timeseries for a single
                                        # participant and assigns the strict
                                        # phase lengths to it. 
                                        # Add the dataset to the list
    ac_dat_list[[i]] <-ac_dat
  }
l_ac[[study_ac]]  <- ac_dat_list
}

# create data frame from this list 

# Create an empty dataframe 
df_ac <- data.frame() 


set.seed (1)  

# for loop

for (i in 1:length(l_ac)){
  study <- l_ac[[i]]
  # Loop through each case in the study
  for (c in seq_along(study)){
    case <- study[[c]]
    # Create a data frame with the measurements and scores
    df_case <- data.frame(study_id = study_no,
                          case_id = case_no,
                          condition = case[[1]],
                          score = case[[2]])
    # Append the case data frame to the main data frame
    df_ac <- bind_rows(df_ac, df_case)
    case_no <- case_no + 1
    if(case_no %% n_cases == 1){
      study_no=study_no+1
    } else {
      study_no=study_no
    }
  }
}
colnames(df_ac)=c("study_id","case_id", "condition_id","score")
df_ac

## 1.4. bind the simulated dataset into one data set for the simulation---------

simple_dat=rbind(df_ab,df_bc,df_ac)
  
n_observations=nrow(simple_dat)  #number of observations across the 15 studies that have 3 cases each.

View(simple_dat)
  #str(simple_dat)
  #unique(simple_dat$stud)


###############################################################################
# 2. NETWORK META-ANALYSIS - Code adapted from Barbosa Mendes et al (2021)-----
###############################################################################


# OLS on each case seperately 
out = with(simple_dat,
           by(simple_dat, case_id, function(x) lm(score ~ 1 + condition_id, data=x))) 
(outsum = lapply(out, summary))
(coefmat_list <- lapply(outsum,coef))
coefmat=do.call(rbind,coefmat_list)
(coeffs = t(coefmat))

sigma = sapply(outsum, `[`, 'sigma') #extracting the residual st dev from the output. 

unique(simple_dat$condition_id)


coeffs = as_tibble(coeffs, rownames = "case_id", .name_repair = "unique")
View(coeffs)
coeffs$sigma = sigma$V1
coeffs
coeffs$stdES = coeffs$...2/coeffs$sigma





