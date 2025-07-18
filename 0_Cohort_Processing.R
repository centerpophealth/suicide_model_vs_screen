####################################################################
###                        Model v Screen                        ###
###                     0. Cohort Processing                     ### 
####################################################################

##########################set directory#############################

dir = "redacted"
setwd(dir)

##########load study parameters and functions/packages##############

#study parameters
source("./___study_parameters___.R")

#functions/packages
source("./___functions___.R")

#########################load data##################################

#load full encounter and diagnosis data as "Full_Clean"
load("./data/Full_Clean.RData")

#load screening data as "dat"
load("./data/ASQ_CSSRS_Ready_Analysis.RData")

#load full ppt encounters and demographics seen in ED as "enc_cohort"
load("./data/Encounter_cohort.RData")

#############subset screen data to study parameters####################

#subset screening data to recruitment period
ind = as.Date(dat$AssessmentDateTime, "%m/%d/%Y %H:%M") >= as.Date(recruit_range[1]) &
      as.Date(dat$AssessmentDateTime, "%m/%d/%Y %H:%M") <= as.Date(recruit_range[2])
MRN_list = unique(dat$MRN[ind])
dat = dat[dat$MRN %in% MRN_list, ]

#subset screening data to age range
ind = as.numeric(dat$Age) >= age_range[1] & 
      as.numeric(dat$Age) < (age_range[2] + 1)
MRN_list = unique(dat$MRN[ind])
dat = dat[dat$MRN %in% MRN_list, ]

############subset full ED cohort to study parameters##################

#subset ed data to recruitment period
ind = as.Date(enc_cohort$ContactDate) >= as.Date(recruit_range[1]) &
      as.Date(enc_cohort$ContactDate) <= as.Date(recruit_range[2])
MRN_list = unique(enc_cohort$MRN[ind])
enc_cohort = enc_cohort[enc_cohort$MRN %in% MRN_list, ]

#subset ed data to age range
#calculate age
enc_cohort$Age = as.numeric((as.Date(enc_cohort$ContactDate) - as.Date(enc_cohort$DOB, "%m/%d/%Y")) / 365)

#get first encounter
first_encounter = as.data.table(enc_cohort)[, list(AssessmentDateTime = min(as.Date(ContactDate))),
                                            by = "MRN"]

#merge first encounter info
enc_cohort = merge(enc_cohort, first_encounter, by = "MRN")

#clean up
rm(first_encounter)

#set to data from first encounter
enc_cohort = enc_cohort[as.Date(enc_cohort$ContactDate) == as.Date(enc_cohort$AssessmentDateTime), ]

#subset to age range
ind = as.numeric(enc_cohort$Age) >= age_range[1] & 
      as.numeric(enc_cohort$Age) < (age_range[2] + 1)
MRN_list = unique(enc_cohort$MRN[ind])
enc_cohort = enc_cohort[enc_cohort$MRN %in% MRN_list, ]

#code race
enc_cohort$Race[enc_cohort$Ethnicity == "Hispanic or Latino"] = "Hispanic"      #get hisp/latino info from ethnicity variable
enc_cohort$Race[grepl("Unspec|Refused|Unknown", enc_cohort$Race)] = "Missing"   #combine missing labels into one label
enc_cohort$Race[enc_cohort$Race == "White or Caucasian"] = "White"              #simplify white label
enc_cohort$Race[enc_cohort$Race == "Black or African American"] = "Black"       #simplify black label
enc_cohort$Race[grepl("White|Black|Hispanic", enc_cohort$Race) == F] = "Other"  #code all other races as "Other"

#create screening vars
enc_cohort$screened = 0    #placeholder for whether a ppt was screened
enc_cohort$risk_group = 0  #placeholder for screening risk scores

#subset ED data to demographics
enc_cohort = subset(enc_cohort, select = c("MRN", 
                                           "Age", 
                                           "Gender", 
                                           "Race", 
                                           "PrimaryPayor",
                                           "AssessmentDateTime",
                                           "screened",
                                           "risk_group"))

#subset ED data to non-screened ppts
enc_cohort = enc_cohort[enc_cohort$MRN %in% dat$MRN == F, ]

#aggregate to unique ppt level
enc_cohort = as.data.table(enc_cohort)[, list(Age                = keepUnique(Age),
                                              Gender             = keepUnique(Gender),
                                              Race               = keepUnique(Race),
                                              PrimaryPayor       = keepUnique(PrimaryPayor),
                                              AssessmentDateTime = keepUnique(AssessmentDateTime),
                                              screened           = keepUnique(screened),
                                              risk_group         = keepUnique(risk_group)),
                                        by = "MRN"]

##################merge screen data and full data####################

#subset screening data to demos, assessment time, risk scores
dat = subset(dat, select = c("MRN", 
                             "Age", 
                             "Gender",
                             "Race",
                             "PrimaryPayor",
                             "AssessmentDateTime",
                             "risk_group"))

#standardize time
dat$AssessmentDateTime = as.Date(dat$AssessmentDateTime, "%m/%d/%Y %H:%M")

#create screened variable
dat$screened = 1

#merge screening data with not screened ED data to create full cohort
dat = as.data.frame(rbind(dat, enc_cohort))

#simplify race labels
dat$Race[dat$Race == "Black or African American"] = "Black"
dat$Race[dat$Race == "Hispanic or Latino"] = "Hispanic"

#subset diagnosis data to cohort
ind = Full_Clean$MRN %in% dat$MRN
Full_Clean = Full_Clean[ind, ]

#subset dx data to contact date and ICD-10 codes
Full_Clean = subset(Full_Clean, select = c("MRN", 
                                           "ContactDate",
                                           "ICD_10"))

#merge dx data by ppt
dat = merge(Full_Clean, dat, all.x = T, by = "MRN")

#clean up
rm(Full_Clean, enc_cohort, ind)

####################get Suicide Attempt status#######################

#calculate presence of SA and create variable
dat$SA = SuicideAttemptCodes(DX_list = dat$ICD_10, full = F, group_3_big = T)

##################get historical/followup partition#################

#calculate difference between encounter date of dx and screening date 
dat$follow = as.Date(dat$ContactDate) - as.Date(dat$AssessmentDateTime)

#create binary variable separating history (0) and follow-up (1)
dat$follow = dat$follow > 0

#################get event status in follow-up######################

#subset to follow-up data
follow = dat[dat$follow == T, ]

#get cases
cases = follow[follow$SA == T, ]

#get first event date and declare event status
cases = as.data.table(cases)[, list(event_date = min(as.Date(ContactDate)),
                                    event = 1),
                              by = "MRN"]

#get controls
cont = follow[follow$MRN %in% cases$MRN == F, ]

#get last visit and declare event status
cont = as.data.table(cont)[, list(event_date = max(as.Date(ContactDate)),
                                  event = 0),
                             by = "MRN"]

#merge case and control event info
follow = rbind(cases, cont)

#add event status and time to full data
dat = merge(dat, follow, all.x = T, by = "MRN")

#code patients without follow-up as no event
dat$event[is.na(dat$event)] = 0

#code event date of patients with no follow-up as last contact
dat$event_date[is.na(dat$event_date)] = max(as.Date(dat$ContactDate))
  
#clean up
rm(cases, cont, follow, require_follow)

##############get length of historical and follow up#################

#get first record 
record_length = as.data.table(dat)[, list(first_record = min(as.Date(ContactDate)),
                                          assess_record = unique(AssessmentDateTime)),
                                     by = "MRN"]

#calculate length of lookback in days
record_length$history_time = as.Date(record_length$assess_record, "%m/%d/%Y %H:%M") -
                             as.Date(record_length$first_record)

#calculate length of follow-up observation in days
record_length$follow_time  = as.Date("2022-03-05") -
                             as.Date(record_length$assess_record, "%m/%d/%Y %H:%M")

#merge record lengths to full data
dat = merge(dat, record_length, by = "MRN")

#get time-to-event as difference between event date and screening date
dat$event_time = as.Date(dat$event_date) - 
                 as.Date(dat$assess_record, "%m/%d/%Y %H:%M")

#clean up
rm(record_length)

#######################get demographics##############################

#flatten data
demo = as.data.table(dat)[, list(age          = keepUnique(Age),
                                 gender       = keepUnique(Gender),
                                 race         = keepUnique(Race),
                                 payor        = keepUnique(PrimaryPayor),
                                 screened     = keepUnique(screened),
                                 risk_group   = keepUnique(risk_group),
                                 event        = keepUnique(event),
                                 screen_date  = keepUnique(AssessmentDateTime),
                                 event_time   = keepUnique(event_time),
                                 history_time = keepUnique(history_time),
                                 follow_time  = keepUnique(follow_time)),
                          by = "MRN"]

#format variables
demo$gender = as.numeric(demo$gender == "Male")
demo$payor = as.numeric(grepl("MEDICAID|MEDICARE", demo$payor))
demo$screened = as.numeric(demo$screened)
demo$risk_group = as.numeric(demo$risk_group)
demo$event = as.numeric(demo$event)
demo$screen_date = as.Date(demo$screen_date)
demo$event_time = as.numeric(demo$event_time)
demo$history_time = as.numeric(demo$history_time)
demo$follow_time = as.numeric(demo$follow_time)

#######################get historical ICD-10 codes###########################

#add suicide attempts to dx codes
dat$ICD_10[dat$SA == T] = paste0(dat$ICD_10[dat$SA == T], ", SA")

#subset dx data to historical period
history = dat[dat$follow == F, ]

#aggregate data to ppt level
history = as.data.table(history)[ , list(dx_all = keepUnique(ICD_10)),
                                 by = "MRN"]

## create design matrix for ICD codes
code_list <- lapply(strsplit(history$dx_all, ","), function(a) {
  a <- trimws(a,which="both")                           #remove white spaces
  sort(a[a != "" & a != "-2" & grepl("NA", a) == F])    #remove error codes
})

#subset dx codes to 3-digits
cat_list <- lapply(code_list, function(a) {
    sort(unique(substr(a, 1, 3)))
})
  
#define all available unique codes
total_cat <- sort(unique(unlist(cat_list)))

#create placeholder matrix for ICD codes
my_icd_mat <- matrix(0, nrow = nrow(history), ncol = length(total_cat))

#rename columes to match available codes
colnames(my_icd_mat) <- paste0("icd10_", total_cat)

#loop through ppts and available codes and populate ppt data as present/absent
for (i in seq_len(nrow(my_icd_mat))) {
  
  my_icd_mat[i, ] <- sapply(total_cat, function(b) {
    sum(cat_list[[i]] == b)
  })
  
  cat("\r", "Extracting ICD-10 codes...", fancyP(i/nrow(my_icd_mat)), "%                         ")
  
}

#clean up
rm(history, cat_list, code_list, i, total_cat, expand_ICD_SA)

##################merge aggregated file and flat ICD data###############

dat <- as.data.frame(cbind(demo, my_icd_mat))

##remove unneeded data
rm(demo, my_icd_mat)

##############Save data###############################################

#save final data
save(dat, file=paste0("./Data/cohort.RData"))

#clean up 
rm(list = ls())
gc()
closeAllConnections()

##########################END OF SCRIPT#################################################

