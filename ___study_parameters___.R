####################################################################
###                        Model v Screen                        ###
###                       Study Parameters                       ### 
####################################################################

#########################Cohort generation##########################
#set age at first screen parameters (min, max)
age_range = c(10, 18)

#set recruitment window (min, max)
recruit_range = c("2019-09-01", "2021-08-31")


############################Modelling################################

#number of random splits
boot         <- 10

#p-value cut off for marginal screening
p_cut_off    <- 0.10

#maximum iteration for glmnet models
iter_size    <- 50000

#set cross-validation folds for lambda selection in glmnet
cv_folds     <- 5

#random seed for glmnet
seed = 999
