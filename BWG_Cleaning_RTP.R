
#### DATA CLEANING AND STANDARDS FOR BWG DATABASE ######################################################################

# Rolando Trejo P.
# September 16, 2022
# BWG database coming from the bromeliad working group
# Main database owners: Diane Srivastava,Michael Melnychuk and Jana Petermann
# Dataset used for this assigment: bwgv1_bromeliads.csv
# Full proyect disponible at https://github.com/RolandoTrejo/Rolando_Data_Management_LDP

#### 1. CONTEXT--------------------------------------------------------------------------------------------------------

# This code is part of the third data cleaning task. 

# Perform quality control checks on three measured variables 
# in the bromeliads dataset (e.g., max_water, num_leaf, height, 
# diameter, ph, etc., etc. -- see names(bromeliads)). Your checks 
# might include, for example, looking for the proportion of 
# observations that include data on each variable, or identifying 
# outliers or improbably values. Remember that some bromeliad 
# variables should be correlated with each other. [suggested package: 
# assertr; see example code in tutorials 2 and 3]

#### 2. THE PROBLEM-----------------------------------------------------------------------------------------------------

# Let's imagine that we want to predict the total detritus as function of the number 
# of leaves and diameter of bromeliads using a model approach (detritus <- number of leaves * diameter).  
# The data must contained less than 5 NA in each column. The numeric data must not contain as well
# outliers with a SD=3. In addition, if the number of leaves and diameter as predictors are not correlated
# among them, they must be transformed into a categorical classification. The final dataset must conserve
# no redundant variables.
  
# 3. THE SOLUTION-------------------------------------------------------------------------------------------------------

# To solve the problem stated before, a data cleaning considering the following tasks is needed:
  
# 3.1. Check for the number of observations containing NA, if there are more than 5 in a column, 
# they must be suppressed from the final dataset.
# 3.2. Check for outliers in the number of leaves, diameter and total detritus. 
# Observation with a SD=3 must be excluded from the final dataset.
# 3.3. Check for correlation between number of leaves and diameter of bromeliads. If they are 
# correlated, one of them must be excluded to prevent using redundant predictors in the final model.
# 3.4. If the number of leaves and diameter are not correlated, then transform them into 
# categorical variables. Use the following criteria:

#                   Leaves: < 15, "low"/ <= 30, "medium"/ > 30, "high"
#                   Diameter: < 50, "small"/ <= 100, "medium" / > 100, "large"

# 3.5. Write the final clean dataset as bromeliads_clean.csv


# 4. CODE TO SOLVE THE PROBLEM AND GET THE FINAL CLEAN DATA

# 4.1. INSTALLING R PACKAGES AND DATA IMPORTATION-----------------------------------------------------------------------

# check if working directory is set to correct folder

getwd()

# if it isn't, use setwd() to change it to the correct folder

# Check for R packages to install

RequiredPackages <- c("tidyverse","dplyr","assertr","corrplot")
for (i in RequiredPackages) { #Installs packages if not yet installed
  if (!require(i, character.only = TRUE)) install.packages(i)}

# Then charge de following libraries:

library("tidyverse")
library("dplyr")
library("assertr")
library("corrplot")

### Import files ###

## what files are in the BWG data folder?

myfiles <- list.files(path = "BWG_database/", pattern = "*.csv", full.names = TRUE)

# import all tables as separate data frames, remove file path and file extensions (.csv)

list2env(
  lapply(
    setNames(myfiles, 
             make.names(gsub(".*1_", "", 
                             tools::file_path_sans_ext(myfiles)))), 
    read_csv), 
  envir = .GlobalEnv)

# 4.2 DATA CLEANING-----------------------------------------------------------------------------------------------------

## 4.2.0 Once the BWG_database is imported, use the bwgv1_bromeliads.csv dataset to create a 
# new table that includes only bromeliads id,species and the three variable measured 
# (num_leaf,extended_diameter,total_detritus)

names(bromeliads)

(bromeliads_cleaning <- bromeliads %>%
    select(bromeliad_id, species, num_leaf,extended_diameter,total_detritus))

## 4.2.1: Check for the proportion of observations that includes NA

sum(is.na(bromeliads_cleaning$num_leaf))
sum(is.na(bromeliads_cleaning$extended_diameter))
sum(is.na(bromeliads_cleaning$total_detritus))

# There are 1,1, and 3 NA for num_leaf, extended_diameter, and total_detritus, respectively.

## 4.2.2: Check for the number of outliers in the number of leaves, diameter and total detritus.
  
bromeliads_cleaning %>% 
  insist(within_n_sds(3), num_leaf) %>% 
  group_by(species) %>% 
  summarise(mean_num_leaf = mean(num_leaf, na.rm = TRUE))

bromeliads_cleaning %>% 
  insist(within_n_sds(3), extended_diameter) %>% 
  group_by(species) %>% 
  summarise(mean_num_extended_diameter = mean(extended_diameter, na.rm = TRUE))

bromeliads_cleaning %>% 
  insist(within_n_sds(3), total_detritus) %>% 
  group_by(species) %>% 
  summarise(mean_num_total_detritus = mean(total_detritus, na.rm = TRUE))

## Only the variable total_detritus stopped execution because at least two values (134.5521 and 171.5467) 
## are less than 3 SD away from the global mean_num_total_detritus.

## 4.2.3: Correlation of numeric variables (num_leaf, extended_diameter, and total_detritus)

# To corroborate correlation, the followin code allows to conserve only numeric variables
  Variables_correlation = subset(bromeliads_cleaning,
                                 select = c(-bromeliad_id, -species))
  
# Compute pearson correlation (note they are absolute values)
  
  corr_matrix=cor(na.omit(Variables_correlation),
               method="pearson") 
  corr_matrix
  
# Correlation graphic: number of leaves, diameter and total detritus.
  
  corrplot(corr_mat, 
           method = "circle",
           tl.col = "black",
           col = COL2('RdBu'))

# The leaves and diameter are not correlated among them. Hence, we can convert them into categorical variables.

## 4.2.4: Variable transformation: from numeric to cateforical
  
# Number of leaves transformed into a categorical variable
  bromeliads_categorical <- bromeliads_cleaning%>%
    mutate(bromeliad_leaf_size = ifelse(num_leaf < 15, "low",
                                        ifelse(num_leaf<= 30, "medium",
                                               ifelse(num_leaf > 30, "high", NA))))

# Including  the diameter of bromeliads as categorical variable
  
  bromeliads_categorical_b <- bromeliads_categorical%>%
    mutate(bromeliad_diameter_size = ifelse(extended_diameter < 50, "small",
                                            ifelse(extended_diameter <= 100, "medium",
                                                   ifelse(extended_diameter > 100, "large", NA))))
  
  
## Deletion of columns containing the number of leaves and diameter as numeric variables and rows containing outliers

    bromeliads_clean <- bromeliads_categorical_b[-c(18,58),-c(3,4)] ## Delete rows 18 and 58. 
                                                                    ## Delete number of leaves and diameter columns
    bromeliads_clean

# 4.3 DATA EXPORTATION-------------------------------------------------------------------------------------------------
    
## Saving the final dataset with solved issues.
  
    write_csv(bromeliads_clean, 
              "bromeliads_clean.csv")


########################################## END OF SCRIPT ###############################################################
