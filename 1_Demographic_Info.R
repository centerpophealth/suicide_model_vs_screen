####################################################################
###                        Model v Screen                        ###
###                  1. Cohort Characteristics                   ### 
####################################################################

##########################set directory#############################

dir = "redacted"
setwd(dir)

######################load functions/packages#######################

#functions/packages
source("./___functions___.R")

#########################load data##################################

load(file = "./Data/cohort.RData")

#################subset data to SA status###########################

case = dat[dat$event == 1, ]
cont = dat[dat$event == 0, ]


######################age info######################################

fancyP(quantile(as.numeric(dat$age), probs = c(0.50, 0.25, 0.75))/100)
fancyP(quantile(as.numeric(dat$age[dat$event == 1]), probs = c(0.50, 0.25, 0.75))/100)
fancyP(quantile(as.numeric(dat$age[dat$event == 0]), probs = c(0.50, 0.25, 0.75))/100)

#####################get demo summaries#############################

#create placeholder
tbl_1 = matrix("", nrow = 21, ncol = 3)

#define colnames
colnames(tbl_1) = c(paste0("Total (n=", fancyN(nrow(dat)), ")"),
                    paste0("Suicide attempers (n=", fancyN(nrow(case)), ")"),
                    paste0("Non-attempters (n=", fancyN(nrow(cont)), ")"))

#define rownames
rownames(tbl_1) = c("Gender, No. (%)", 
                    "Male", "Female",
                    "Age group, No. (%)", 
                    "10-13", "14-18", 
                    "Race and ethnicity, No. (%)", 
                    "Black or African American", "Hispanic or Latino", "Other", "White",
                    "Medicaid or Medicare, No. (%)", 
                    "Yes", "No",
                    "Screening results, No. (%)", 
                    "Not completed",
                    "Negative, minimal/low risk", "Positive, moderate/high risk", 
                    "Look back/Follow up period, days, median (IQR)", 
                    "Pre-screening", "Post-screening")


#############male##############

n = sum(dat$gender == 1)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$gender == 1)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$gender == 1)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[2, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############female##############

n = sum(dat$gender == 0)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$gender == 0)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$gender == 0)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[3, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############10-13 years old##############

n = sum(dat$age == 0)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$age == 0)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$age == 0)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[5, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############14-18 years old##############

n = sum(dat$age == 1)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$age == 1)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$age == 1)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[6, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Black##############

n = sum(dat$race == "Black")
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$race == "Black")
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$race == "Black")
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[8, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Hispanic##############

n = sum(dat$race == "Hispanic")
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$race == "Hispanic")
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$race == "Hispanic")
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[9, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Other race##############

n = sum(dat$race == "Other")
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$race == "Other")
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$race == "Other")
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[10, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############White##############

n = sum(dat$race == "White")
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$race == "White")
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$race == "White")
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[11, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Payor yes##############

n = sum(dat$payor == 1)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$payor == 1)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$payor == 1)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[13, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Payor no##############

n = sum(dat$payor == 0)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$payor == 0)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$payor == 0)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[14, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

############Incomplete screen#############

n = sum(dat$screened == 0)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$screened == 0)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$screened == 0)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[16, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Negative screen##############

n = sum(dat$risk_group < 2 & dat$screened == 1)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$risk_group < 2 & case$screened == 1)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$risk_group < 2 & cont$screened == 1)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[17, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

#############Positive screen##############

n = sum(dat$risk_group >= 2 & dat$screened == 1)
p = fancyP(n/nrow(dat))
n = fancyN(n)

case_n = sum(case$risk_group >= 2 & case$screened == 1)
case_p = fancyP(case_n/nrow(case))
case_n = fancyN(case_n)

cont_n = sum(cont$risk_group >= 2 & cont$screened == 1)
cont_p = fancyP(cont_n/nrow(cont))
cont_n = fancyN(cont_n)

tbl_1[18, ] = c(combo(n, p), combo(case_n, case_p), combo(cont_n, cont_p))

###########History lookback###############

med = getMedianIQR(dat$history_time)
case_med = getMedianIQR(case$history_time)
cont_med = getMedianIQR(cont$history_time)

tbl_1[20, ] = c(med, case_med, cont_med)

##############Follow obs##################

med = getMedianIQR(dat$follow_time)
case_med = getMedianIQR(case$follow_time)
cont_med = getMedianIQR(cont$follow_time)

tbl_1[21, ] = c(med, case_med, cont_med)

#####################get demo comparisons#############################

#create placeholder
supp_1 = matrix("", nrow = 14, ncol = 1)

#define rownames
rownames(supp_1) = c("Gender", 
                     "Age", 
                     "Race and Ethnicity", 
                     "Black v else",
                     "Hispanic v else",
                     "Other v else",
                     "White v else",
                     "Medicaid or medicare",
                     "Screening results",
                     "Missed",
                     "Minimal/Low",
                     "Moderate/High",
                     "Historical lookback",
                     "Follow-up window")
#define colnames
colnames(supp_1) = c("p")

#gender
supp_1[1] = fancyPval(chisq.test(dat$event, dat$gender)$p.value)

#age
supp_1[2] = fancyPval(chisq.test(dat$event, dat$age)$p.value)

#age continous
dat$age = as.numeric(dat$age)
fancyPval(wilcox.test(age ~ event, data = dat, paired = F)$p.value)

#race and ethnicity
supp_1[3] = fancyPval(chisq.test(dat$event, dat$race)$p.value)
#multiple comparisons
supp_1[4:7] = fancyPval(
                p.adjust(
                  c(chisq.test(dat$event, dat$race == "Black")$p.value,
                    chisq.test(dat$event, dat$race == "Hispanic")$p.value,
                    chisq.test(dat$event, dat$race == "Other")$p.value,
                    chisq.test(dat$event, dat$race == "White")$p.value)))

#medicaid or medicare
supp_1[8] = fancyPval(chisq.test(dat$event, dat$payor)$p.value)

dat$risk_group[dat$screened == 0] = "Missed"
#screening results
supp_1[9] = fancyPval(chisq.test(dat$event, dat$risk_group)$p.value)

#multiple comparisons
supp_1[10:12] = fancyPval(
  p.adjust(
    c(chisq.test(dat$event, dat$risk_group == "Missed")$p.value,
      chisq.test(dat$event, grepl("0|1", dat$risk_group))$p.value,
      chisq.test(dat$event, grepl("2|3", dat$risk_group))$p.value)))

#history time
supp_1[13] = fancyPval(wilcox.test(history_time ~ event, data = dat, paired = F)$p.value)

#follow time
supp_1[14] = fancyPval(wilcox.test(follow_time ~ event, data = dat, paired = F)$p.value)

############################Create tables#############################

tbl_1 = tbl_1 %>%
          kbl() %>%
            kable_classic(html_font = "Times new roman")

supp_1 = supp_1 %>%
            kbl() %>%
              kable_classic(html_font = "Times new roman")

save_kable(tbl_1, file = "./Results/1_Demographic_Information.HTML")

save_kable(supp_1, file = "./Results/Supp_1_Demographic_Information.HTML")

#clean up 
rm(list = ls())
gc()
closeAllConnections()

##########################END OF SCRIPT#################################################

