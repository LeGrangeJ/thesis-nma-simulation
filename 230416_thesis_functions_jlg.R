# functions thesis simulation

# functions ------
    ## Function 1 create a simple time series 
    ##with and mpn = number of measurements and a= autocorrelation fixed at 0.2 
    
    create_ts <- function(mpn,a){
    #mpn=number of measurement points, a=autocorrelation#constant used to make sure     that this is not necessary
      timeseries=c()
      point1=rnorm(1,0,1) #pick 1 pt from stand normal distribution of 0, sd= 1 
      timeseries=c(timeseries,point1) #run it an you'll see that their is no autocorrelation.   𝑎𝑥(𝑡−1) + 𝜀𝑡
      for (i in 2:mpn){ #for loop
        point=a*timeseries[i-1] + rnorm(1)
        timeseries=c(timeseries,point)
      }
    #timeseries=timeseries
      return(timeseries)
    }
    
    (create_ts(10,.2))


    ## FUNCTIONS 2: GENERATE A TIME SERIES FOR EACH STUDY

    ## Adapts the simple times series above and creates a timeseries of for a          single case with a priori defined standard mean differences 

    ## A created this function for comparing three studies (study 1, 2 , 3). 
    ## Depending on which conditions, I generated the following condition
    ## (condition A = baseline, condition B = treatments 1,
    ## condition C = treatment 2):
    ## 
    ## Study 1: compares condition A and B
    ## study 2: compares condition B and C 
    ## study 3: compares condition A and C 


    create_dat_ab <-function(timeseries){
      phaselength <- np_1 + np_2
      p1 <- timeseries[1:np_1]  
      p2 <- timeseries[(np_1 + 1):phaselength] + smd1
      labels <- c(rep(0, np_1), rep(1, np_2))
      ab_dat <- list(labels, c(p1, p2))  # Updated variable names to lowercase
      return(ab_dat)
    }

    
    
    
    create_dat_bc <-function(timeseries){
      phaselength <- np_1 + np_2
      p1 <- timeseries[1:np_1]+smd1  
      p2 <- timeseries[(np_1 + 1):phaselength] + smd2
      labels <- c(rep(1, np_1), rep(2, np_2))
      ab_data_bc <- list(labels, c(p1, p2))  # Updated variable names to lowercase
      return(ab_data_bc)
    }

    create_dat_ac<-function(timeseries){
      phaselength = np_1 + np_2 
      p1 <- timeseries[1:np_1] 
      p2 <- timeseries[(np_1 + 1):phaselength] + smd3
      labels <- c(rep(0, np_1), rep(2, np_2))
      ab_data_bc <- list(labels, c(p1, p2))  # Updated variable names to lowercase
      return(ab_data_bc)
    }
    