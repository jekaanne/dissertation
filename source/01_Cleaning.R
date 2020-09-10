library(here) #Sets working directory
library(renv) #Package management by RStudio
library(data.table) #Smart data frames
library(tidyverse) #Packages for tidy data

# Chapter 2 - 3 ####
ardraw <- fread("data/Survey1-complete.csv", na.strings = c("",NA))
ardraw2 <- fread("data/Survey2-complete.csv", na.strings = c("",NA))

# survey 1 cleaning  ###
ardraw$move_physically[ardraw$disability_status == "Nondisabled"] = NA
ardraw$understand_info[ardraw$disability_status == "Nondisabled"] = NA
ardraw$see_hear_info[ardraw$disability_status == "Nondisabled"] = NA
ardraw$be_around_people[ardraw$disability_status == "Nondisabled"] = NA
ardraw$deal_w_frustration[ardraw$disability_status == "Nondisabled"] = NA
ardraw$communicate[ardraw$disability_status == "Nondisabled"] = NA


# calculate mean score of final model from Ch.2, Survey 1
ttbpn <- dplyr::select(ardraw, aut3,  aut4, aut5, aut6, rel1, rel2, rel3, rel4, com1, com2, com3, com6)
ardraw$ttbpnmean <- rowMeans(ttbpn, na.rm = TRUE)

# survey 2 cleaning ###
# create binary disability status variable
ardraw2$disability_status[is.na(ardraw2$global_disability)] = 0
ardraw2$disability_status[ardraw2$global_disability == 1] = 0
ardraw2$disability_status[ardraw2$global_disability == 2 | ardraw2$global_disability == 3] = 1


#convert zeros in ttbpn scale to NAs
ardraw2[aut1 == 0, aut1:=NA]
ardraw2[aut2 == 0, aut2:=NA]
ardraw2[aut3 == 0, aut3:=NA]
ardraw2[aut4 == 0, aut4:=NA]
ardraw2[aut5 == 0, aut5:=NA]
ardraw2[aut6 == 0, aut6:=NA]
ardraw2[rel1 == 0, rel1:=NA]
ardraw2[rel2 == 0, rel2:=NA]
ardraw2[rel3 == 0, rel3:=NA]
ardraw2[rel4 == 0, rel4:=NA]
ardraw2[rel5 == 0, rel5:=NA]
ardraw2[rel6 == 0, rel6:=NA]
ardraw2[com1 == 0, com1:=NA]
ardraw2[com2 == 0, com2:=NA]
ardraw2[com3 == 0, com3:=NA]
ardraw2[com4 == 0, com4:=NA]

#create binary white/nonwhite variable
ardraw2$white <- NA
ardraw2$white[ardraw2$eth_white == 1] = 1
ardraw2$white[ardraw2$eth_asian == 1] = 0
ardraw2$white[ardraw2$eth_black == 1] = 0
ardraw2$white[ardraw2$eth_hispanic == 1] = 0
ardraw2$white[ardraw2$eth_nativeamer == 1] = 0
ardraw2$white[ardraw2$eth_indian == 1] = 0
ardraw2$white[ardraw2$eth_mideast == 1] = 0
ardraw2$white[ardraw2$eth_pacislander == 1] = 0
ardraw2$white[ardraw2$ethnicity_other == 1] = 0

#SS status variable
ardraw2$ss_status <- NA
ardraw2$ss_status[ardraw2$ssi == 1] = 1
ardraw2$ss_status[ardraw2$ssdi == 1] = 1
ardraw2$ss_status[is.na(ardraw2$ss_status)] = 0

#convert age to age ranges
ardraw2$age_ranges<-cut(ardraw2$age, breaks=c(0,18,36,51,65), labels = c("18-35", "36-50", "51-65", "65+"))
# survey 2 cleaning ##
#calculate income groups
ardraw2$hhincome <- as.numeric(ardraw2$hhincome)
ardraw2$income_range <- cut(ardraw2$hhincome, breaks=c(0,10000,30000,70000,100000),labels = c("<10,000","10,000-30,000", "30,000 - 70,000", "100,000+"), include.lowest = TRUE, ordered_result = TRUE)
ardraw2$income_range <- as.character(ardraw2$income_range)
ardraw2$income_range[is.na(ardraw2$income_range)] <- "Not specified"
ardraw2$income_range <- as.factor(ardraw2$income_range)

#convert age to age ranges

# replace NAs with zeros in disability variables
ardraw2[is.na(dis_seeing), dis_seeing:= 0]
ardraw2[is.na(dis_hearing), dis_hearing:= 0]
ardraw2[is.na(dis_walking), dis_walking:= 0]
ardraw2[is.na(dis_cognitive), dis_cognitive:= 0]
ardraw2[is.na(dis_selfcare), dis_selfcare:= 0]
ardraw2[is.na(dis_comm), dis_comm:= 0]
ardraw2[is.na(depression_severity), depression_severity:= 0]
ardraw2[is.na(anxiety_severity), anxiety_severity:= 0]
ardraw2[is.na(pain_severity), pain_severity:= 0]
ardraw2[is.na(fatigue_severity), fatigue_severity:= 0]

# create binary disability variables
ardraw2$bdis_seeing <- (0)
ardraw2$bdis_seeing[ardraw2$dis_seeing == 1 | ardraw2$dis_seeing == 2 | ardraw2$dis_seeing == 3] = 1
ardraw2$bdis_hearing <- (0)
ardraw2$bdis_hearing[ardraw2$dis_hearing == 1 | ardraw2$dis_hearing == 2 | ardraw2$dis_hearing == 3] = 1
ardraw2$bdis_walking <- (0)
ardraw2$bdis_walking[ardraw2$dis_walking == 1 | ardraw2$dis_walking == 2 | ardraw2$dis_walking == 3] = 1
ardraw2$bdis_cognitive <- (0)
ardraw2$bdis_cognitive[ardraw2$dis_cognitive == 1 | ardraw2$dis_cognitive == 2 | ardraw2$dis_cognitive == 3] = 1
ardraw2$bdis_comm <- (0)
ardraw2$bdis_comm[ardraw2$dis_comm == 1 | ardraw2$dis_comm == 2 | ardraw2$dis_comm == 3] = 1
ardraw2$bdis_selfcare <- (0)
ardraw2$bdis_selfcare[ardraw2$dis_selfcare == 1 | ardraw2$dis_selfcare == 2 | ardraw2$dis_selfcare == 3] = 1
ardraw2$banxiety_severity <- (0)
ardraw2$banxiety_severity[ardraw2$anxiety_severity == 3] = 1
ardraw2$bdepression_severity <-(0)
ardraw2$bdepression_severity[ardraw2$depression_severity == 3] = 1
ardraw2$bpain_severity <-(0)
ardraw2$bpain_severity[ardraw2$pain_severity == 3] = 1
ardraw2$bfatigue_severity <- (0)
ardraw2$bfatigue_severity[ardraw2$fatigue_severity == 3] = 1

#conventional control variable
ardraw2$condis <- (0)
ardraw2$condis[ardraw2$bdis_walking == 1] <- 1
ardraw2$condis[ardraw2$bdis_seeing == 1] <- 1
ardraw2$condis[ardraw2$bdis_hearing == 1] <- 1
ardraw2$condis[ardraw2$bdis_cognitive == 1] <- 1
ardraw2$condis[ardraw2$bdis_selfcare == 1] <- 1


# NA's for disability types

alt_disability_status <- dplyr::select(ardraw2, bdis_seeing, bdis_walking, bdis_cognitive, bdis_comm, bdis_selfcare, banxiety_severity, bdepression_severity, bpain_severity, bfatigue_severity)
alt_disability_status$bdis_seeing <- as.numeric(alt_disability_status$bdis_seeing)
alt_disability_status$bdis_hearing <- as.numeric(alt_disability_status$bdis_hearing)
alt_disability_status$bdis_walking <- as.numeric(alt_disability_status$bdis_walking)
alt_disability_status$bdis_cognitive <- as.numeric(alt_disability_status$bdis_cognitive)
alt_disability_status$bdis_comm <- as.numeric(alt_disability_status$bdis_comm)
alt_disability_status$bdis_selfcare <- as.numeric(alt_disability_status$bdis_selfcare)
alt_disability_status$banxiety_severity <- as.numeric(alt_disability_status$banxiety_severity)
alt_disability_status$bdepression_severity <- as.numeric(alt_disability_status$bdepression_severity)
alt_disability_status$bpain_severity <- as.numeric(alt_disability_status$bpain_severity)
alt_disability_status$bfatigue_severity <- as.numeric(alt_disability_status$bfatigue_severity)

ardraw2$alt_disability_status <- rowSums(alt_disability_status, na.rm = TRUE)

#calculate binary transport impact question
ardraw2$bimpact_trans <- NA
ardraw2$bimpact_trans[ardraw2$impact_trans==0] <- 0
ardraw2$bimpact_trans[ardraw2$impact_trans == 1] = 0
ardraw2$bimpact_trans[ardraw2$impact_trans == 2] = 1
ardraw2$bimpact_trans[ardraw2$impact_trans == 3] = 1
ardraw2$bimpact_trans[ardraw2$impact_trans == 4] = 1


# disability scale (10 dis types w/ severities)
dis_score <- dplyr::select(ardraw2, dis_seeing, dis_hearing, dis_walking, dis_cognitive, dis_comm, dis_selfcare, anxiety_severity, depression_severity, pain_severity , fatigue_severity)
ardraw2$dis_score <- rowSums(dis_score, na.rm = TRUE)
ardraw2$dis_score[ardraw2$dis_score == 0] <- NA

#adjust discr to 5 pt. scale
ardraw2$disc1 <- (ardraw2$discr1*5)/6
ardraw2$disc2 <- (ardraw2$discr2*5)/6
ardraw2$disc3 <- (ardraw2$discr3*5)/6
ardraw2$disc4 <- (ardraw2$discr4*5)/6

# calculate mean score of final model from Ch.2, Survey 2
ttbpn2 <- dplyr::select(ardraw2, aut2, aut3, aut5, aut6,  rel2, rel3, rel4, rel6, com1, com2, com3, com4)
ardraw2$ttbpnmean <- rowMeans(ttbpn2, na.rm = TRUE)
altbpnf2 <-  dplyr::select(ardraw2, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  disc1, disc2, disc3, disc4)
ardraw2$altbpnfmean <- rowMeans(altbpnf2, na.rm = TRUE)
flour2 <- dplyr::select(ardraw2, flour1, flour2, flour3, flour4, flour5, flour6, flour7, flour8)
ardraw2$flourmean <- rowMeans(flour2, na.rm = TRUE)

# calculate log hhincome
ardraw2$loghhincome <- log(ardraw2$hhincome)
ardraw2$loghhincome[which(ardraw2$loghhincome=="-Inf")] = NA # replacing inf w/ NA for one weird case

#calculate binary employment status 
ardraw2$employed <- 0
ardraw2$employed[ardraw2$emp_ft ==1] <- 1
ardraw2$employed[ardraw2$emp_pt ==1] <- 1

# replace NAs w/ zeros for summing
ardraw2$aware_ttw[is.na(ardraw2$aware_ttw)]<- 0
ardraw2$aware_pass[is.na(ardraw2$aware_pass)]<- 0
ardraw2$aware_able[is.na(ardraw2$aware_able)]<- 0
ardraw2$aware_medicaid_buy_in[is.na(ardraw2$aware_medicaid_buy_in)]<- 0
ardraw2$aware_wipa[is.na(ardraw2$aware_wipa)]<- 0
ardraw2$aware_reduced_subway[is.na(ardraw2$aware_reduced_subway)]<- 0
ardraw2$aware_reduced_train[is.na(ardraw2$aware_reduced_train)]<- 0
ardraw2$aware_paratransit[is.na(ardraw2$aware_paratransit)]<- 0
ardraw2$aware_free_metrocard[is.na(ardraw2$aware_free_metrocard)]<- 0
ardraw2$aware_trans_discount_none[is.na(ardraw2$aware_trans_discount_none)]<- 0

#SS status variable
ardraw2$ss_status <- NA
ardraw2$ss_status[ardraw2$ssi == 1 | ardraw2$ssdi == 1] <- 1
ardraw2$ss_status[is.na(ardraw2$ss_status) & ardraw2$disability_status == 1]= (0)

ardraw2$tr_motiv_community[ardraw2$tr_motiv_community==0 | ardraw2$tr_motiv_community ==99] <- NA
ardraw2$tr_motiv_dining[ardraw2$tr_motiv_dining==0]<- NA
ardraw2$tr_motiv_exercise[ardraw2$tr_motiv_exercise==0 | ardraw2$tr_motiv_exercise ==99] <- NA
ardraw2$tr_motiv_gym[ardraw2$tr_motiv_gym==0 | ardraw2$tr_motiv_gym ==99] <- NA
ardraw2$tr_motiv_leisure_alone[ardraw2$tr_motiv_leisure_alone==0 | ardraw2$tr_motiv_leisure_alone ==99] <- NA
ardraw2$tr_motiv_medical[ardraw2$tr_motiv_medical==0 | ardraw2$tr_motiv_medical ==99] <- NA
ardraw2$tr_motiv_shopping[ardraw2$tr_motiv_shopping==0] <- NA
ardraw2$tr_motiv_social[ardraw2$tr_motiv_social==0] <- NA
ardraw2$tr_motiv_transport_someone[ardraw2$tr_motiv_transport_someone==0 | ardraw2$tr_motiv_transport_someone ==99] <- NA
ardraw2$tr_motiv_work_school_vol[ardraw2$tr_motiv_work_school_vol==0] <- NA

# label categorical variables
ardraw2 <- ardraw2 %>%  dplyr::mutate(ss_status = recode_factor(ss_status, `1` = "SS Beneficiary", `0` = "Disabled Non-Beneficiary", .missing  = "Nondisabled", .default  = "NA"))

ardraw2 <- ardraw2 %>%  dplyr::mutate(disability_status = recode_factor(disability_status, `1` = "Disabled", `0`  = "Nondisabled", .default ="NA"))

ardraw2 <- ardraw2 %>% 
  dplyr::mutate(gender = recode_factor(gender, `1` = "Female", `0` = "Male", `2` = "Not specified", .missing ="Not specified", .default = "NA"))

ardraw2 <- ardraw2 %>%
  dplyr::mutate(white = recode_factor(white, `1` = "White",`0` = "Non-white", .missing ="Not specified", .default = "NA"))

ardraw2 <- ardraw2 %>% dplyr::mutate(education = recode_factor(education, `1` = "High school or less", `2` = "High school or less", `3` = "High school or less",`4` = "College", `5` = "College",`6` = "Advanced degree", `7` = "Advanced degree", `8` = "Advanced degree", .missing = "Not specified", .default = "NA"))

#reset data.table to get rid of "trucol "setalloccol(y) : can't set ALTREP truelength"length issues
ardraw <- data.table(ardraw)
ardraw2 <- data.table(ardraw2)



# Chapter 4 ####
travel_diary <- fread("data/Travel-Diary-Survey.csv", na.strings = c("",NA))
# travel diary cleaning #
#convert discr to 5 pt. scale
travel_diary$disc1 <- (travel_diary$discr1*5)/6
travel_diary$disc2 <- (travel_diary$discr2*5)/6
travel_diary$disc3 <- (travel_diary$discr3*5)/6
travel_diary$disc4 <- (travel_diary$discr4*5)/6


# ttbpn, altbpnf, flourishing mean scores
ttbpn2 <- dplyr::select(travel_diary, aut2, aut3, aut5, aut6,  rel2, rel3, rel4, rel6, com1, com2, com3, com4)
travel_diary$ttbpnmean <- rowMeans(ttbpn2, na.rm = TRUE)


altbpnf2 <-  dplyr::select(travel_diary, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  disc1, disc2, disc3, discr4)
travel_diary$altbpnfmean <- rowMeans(altbpnf2, na.rm = TRUE)


flour2 <- dplyr::select(travel_diary, flour1, flour2, flour3, flour4, flour5, flour6, flour7, flour8)
travel_diary$flourmean <- rowMeans(flour2, na.rm = TRUE)

# calculate loghhincome 
travel_diary$hhincome <- as.numeric(travel_diary$hhincome)
travel_diary$loghhincome <- log(travel_diary$hhincome)
travel_diary$loghhincome[which(travel_diary$loghhincome=="-Inf")] = 0
travel_diary$loghhincome[is.na(travel_diary$loghhincome)] <- (0) # get rid of 1 weird case

# create binary disability status variable
travel_diary$disabled[is.na(travel_diary$global_disability)] <- NA
travel_diary$disabled[travel_diary$global_disability == 1] = 0
travel_diary$disabled[travel_diary$global_disability == 2 | travel_diary$global_disability == 3] = 1
# remove disability status = NA
travel_diary <- travel_diary[!is.na(travel_diary$disability_status)]

travel_diary$trips <- as.numeric(travel_diary$trips)
travel_diary$od_total <- as.numeric(travel_diary$od_total)
travel_diary$averagetripmood <- rowMeans(travel_diary[,c("besttripmood.m", "worsttripmood.m")], na.rm = TRUE)
travel_diary$od_total<- rowSums(travel_diary[, c("od_totalworst.t", "od_totalbest.t")], na.rm = TRUE)
#avg obs per active day
travel_diary$avg.obsperday <- travel_diary$o_total/travel_diary$trips
#avg delays per active day
travel_diary$avg.delaysperday <- travel_diary$d_total/travel_diary$trips
#avg obs delays per active day
travel_diary$avg.obsdelaysperday <- (travel_diary$o_total + travel_diary$d_total)/travel_diary$trips


# calculate average trip mood
travel_diary$averagetripmood <- rowMeans(travel_diary[,c("besttripmood.m", "worsttripmood.m")], na.rm = TRUE)
travel_diary$od_total<- rowSums(travel_diary[, c("od_totalworst.t", "od_totalbest.t")], na.rm = TRUE)

#rename anon email to id
travel_diary$id <- travel_diary$email
travel_diary$email <- NULL

#create binary white/nonwhite variable
travel_diary$white <- NA
travel_diary$white[travel_diary$eth_white == 1] = 1
travel_diary$white[travel_diary$eth_asian == 1] = 0
travel_diary$white[travel_diary$eth_black == 1] = 0
travel_diary$white[travel_diary$eth_hispanic == 1] = 0
travel_diary$white[travel_diary$eth_nativeamer == 1] = 0
travel_diary$white[travel_diary$eth_indian == 1] = 0
travel_diary$white[travel_diary$eth_mideast == 1] = 0
travel_diary$white[travel_diary$eth_pacislander == 1] = 0
travel_diary$white[travel_diary$ethnicity_other == 1] = 0

#SS status variable
travel_diary$ss_status <- NA
travel_diary$ss_status[travel_diary$ssi == 1] = 1
travel_diary$ss_status[travel_diary$ssdi == 1] = 1
travel_diary$ss_status[is.na(travel_diary$ss_status)] = 0

#convert age to age ranges
travel_diary$age_ranges<-cut(travel_diary$age, breaks=c(0,18,36,51,65), labels = c("18-35", "36-50", "51-65", "65+"))
# survey 2 cleaning ##
#calculate income groups
travel_diary$hhincome <- as.numeric(travel_diary$hhincome)
travel_diary$income_range <- cut(travel_diary$hhincome, breaks=c(0,10000,30000,70000,100000),labels = c("<10,000","10,000-30,000", "30,000 - 70,000", "100,000+"), include.lowest = TRUE, ordered_result = TRUE)
travel_diary$income_range <- as.character(travel_diary$income_range)
travel_diary$income_range[is.na(travel_diary$income_range)] <- "Not specified"
travel_diary$income_range <- as.factor(travel_diary$income_range)

# label factors
travel_diary <- travel_diary %>%  dplyr::mutate(ss_status = recode_factor(ss_status, `1` = "SS Beneficiary", `0` = "Disabled Non-Beneficiary", .missing  = "Nondisabled", .default  = "NA"))

travel_diary <- travel_diary %>%  dplyr::mutate(disability_status = recode_factor(disability_status, `1` = "Disabled", `0`  = "Nondisabled", .default ="NA"))

travel_diary <- travel_diary %>% 
  dplyr::mutate(gender = recode_factor(gender, `1` = "Female", `0` = "Male", `2` = "Not specified", .missing ="Not specified", .default = "NA"))

travel_diary <- travel_diary %>%
  dplyr::mutate(white = recode_factor(white, `1` = "White",`0` = "Non-white", .missing ="Not specified", .default = "NA"))

travel_diary <- travel_diary %>% dplyr::mutate(education = recode_factor(education, `1` = "High school or less", `2` = "High school or less", `3` = "High school or less",`4` = "College", `5` = "College",`6` = "Advanced degree", `7` = "Advanced degree", `8` = "Advanced degree", .missing = "Not specified", .default = "NA"))

#remove one case w/ no trip data at all
travel_diary<- travel_diary[-c(3),]
travel_diary <- data.table(travel_diary)

# fix missing values (99 and 0)

ardraw2$tr_motiv_community[ardraw2$tr_motiv_community==0 | ardraw2$tr_motiv_community ==99] <- NA
ardraw2$tr_motiv_dining[ardraw2$tr_motiv_dining==0 | ardraw2$tr_motiv_dining==99]<- NA
ardraw2$tr_motiv_exercise[ardraw2$tr_motiv_exercise==0 | ardraw2$tr_motiv_exercise ==99] <- NA
ardraw2$tr_motiv_gym[ardraw2$tr_motiv_gym==0 | ardraw2$tr_motiv_gym ==99] <- NA
ardraw2$tr_motiv_leisure_alone[ardraw2$tr_motiv_leisure_alone==0 | ardraw2$tr_motiv_leisure_alone ==99] <- NA
ardraw2$tr_motiv_medical[ardraw2$tr_motiv_medical==0 | ardraw2$tr_motiv_medical ==99] <- NA
ardraw2$tr_motiv_shopping[ardraw2$tr_motiv_shopping==0] <- NA
ardraw2$tr_motiv_social[ardraw2$tr_motiv_social==0] <- NA
ardraw2$tr_motiv_transport_someone[ardraw2$tr_motiv_transport_someone==0 | ardraw2$tr_motiv_transport_someone ==99] <- NA
ardraw2$tr_motiv_work_school_vol[ardraw2$tr_motiv_work_school_vol==0] <- NA

#create travelmotiv scales
travelmotiv <- dplyr::select(ardraw2, tr_motiv_work_school_vol, tr_motiv_shopping, tr_motiv_dining, tr_motiv_medical, tr_motiv_social, tr_motiv_gym, tr_motiv_leisure_alone, tr_motiv_community, tr_motiv_transport_someone, tr_motiv_exercise)
ardraw2$trmotivmean <- rowMeans(travelmotiv, na.rm = TRUE)
travelmotivsoc <- dplyr::select(ardraw2, tr_motiv_dining, tr_motiv_social, tr_motiv_leisure_alone, tr_motiv_community)
ardraw2$trmotivsocmean <- rowMeans(travelmotivsoc, na.rm = TRUE)
travelmotivwork <- dplyr::select(ardraw2, tr_motiv_work_school_vol, tr_motiv_shopping, tr_motiv_medical, tr_motiv_gym, tr_motiv_transport_someone, tr_motiv_exercise)
ardraw2$trmotivworkmean <- rowMeans(travelmotivwork, na.rm = TRUE)


#create binary var for motivation
ardraw2$bwork_motivation <- NA
ardraw2$bwork_motivation[ardraw2$work_motivation == 1 | ardraw2$work_motivation == 2] <- 0
ardraw2$bwork_motivation[ardraw2$work_motivation == 4 | ardraw2$work_motivation == 5] <- 1
ardraw2$bwork_motivation[is.na(ardraw2$bwork_motivation)] <- (0)