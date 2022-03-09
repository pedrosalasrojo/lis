#         Author: Pedro Salas Rojo 
#         Date: 09/03/2022 
#         Dataset: LIS 
#         Name of project: Get LIS data and show some basic results 

rm(list = ls(all.names = TRUE))  
library(tidyverse) 
library(haven) 
library(dineq) 
library(reldist) 
library(xtable) 

# Get and clean data ---- 

# Define function to get household-related variables 
# Include in "hhold" all variables  
seth <- function(data_file) {     
  hhold <- c('hid', 'hpopwgt', 'hilabour')  
  data1 <- read.LIS(data_file, labels = FALSE, vars = hhold)   
  return(data1)   
} 

# Define function to get personal-related variables 
# Include in "hhold" all variables  

setp <- function(data_file){ 
  pers <- c('hid', 'pid', 'pilabour', 'sex', 'age') 
  data2 <- read.LIS(data_file, labels = FALSE, vars = pers)    
  return(data2) 
} 

# Store names of the datasets 
datasets <- c('il17', 'il18')  
results <- NA 

# For each dataset,  
for (ccyy in datasets) {  
  
  # Print name of dataset 
  print(ccyy) 
   
  # Get data with functions previouisly defined 
  data1 <- seth(paste0(ccyy,'h'))  
  data2 <- setp(paste0(ccyy,'p')) 
  
  # Merge by ID 
  data <- merge(data1, data2, by=c("hid"), sort=TRUE) 
  
  # Get is.na information 
  print(summary(is.na(data))) 
  
  # Eliminate NA. variables (USE THIS LINE WITH CAUTION) 
  data <- na.omit(data) 
  print(summary(is.na(data))) 
  
  # Arrange the data, names and so on. 
  data <- data %>% 
    filter(age<66) %>% 
    rename(weights = hpopwgt, labinc = hilabour) 
  
  # Compute the analysis of interest. Here, we show simple baseline statistics  
  
  tab <- data %>% 
    summarise(country = ccyy, 
              observations = nrow(data), 
              mean = round(weighted.mean(labinc, weights, na.rm = TRUE), 2), 
              gini = round(gini.wtd(labinc, weights), 4), 
              mld  = round(mld.wtd(labinc, weights), 4))  
  
  # Once you have all your results, store them in an object
  results <- rbind(results, tab) 
  
} 

# Omit first row of results (NA) 
results <- na.omit(results) 

# Plug results in a Latex Table.  
table <- xtable(results , digits=c(0,0,0,2,4,4), caption=paste0("Original Statistics"),  
                label=paste0("originalstatistics")) 
names(table)<-c('Country', 'N', 'Mean', 'Gini', 'MLD') 
print(table, include.rownames=FALSE,  caption.placement = "top")

# You can download this table and plug it in your research
