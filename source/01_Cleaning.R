library(tidyverse)
library(data.table)
library(dplyr)
library(here)

# Chapter 2 - 3 ####
ardraw <- fread("data/raw/Survey1-complete.csv", na.strings = c("",NA))
ardraw2 <- fread("data/raw/Survey2-complete.csv", na.strings = c("",NA))

# survey 1 cleaning  ###
ardraw$move_physically[ardraw$disability_status == "Nondisabled"] = NA
ardraw$understand_info[ardraw$disability_status == "Nondisabled"] = NA
ardraw$see_hear_info[ardraw$disability_status == "Nondisabled"] = NA
ardraw$be_around_people[ardraw$disability_status == "Nondisabled"] = NA
ardraw$deal_w_frustration[ardraw$disability_status == "Nondisabled"] = NA
ardraw$communicate[ardraw$disability_status == "Nondisabled"] = NA

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

# calculate mean score of final model from Ch.2, Survey 1
ttbpn <- dplyr::select(ardraw, aut3,  aut4, aut5, aut6, rel1, rel2, rel3, rel4, com1, com2, com3, com6)
ardraw$ttbpnmean <- rowMeans(ttbpn, na.rm = TRUE)

# survey 2 cleaning ###
# create binary disability status variable
ardraw2$disability_status[is.na(ardraw2$global_disability)] <- NA
ardraw2$disability_status[ardraw2$global_disability == 1] = 0
ardraw2$disability_status[ardraw2$global_disability == 2 | ardraw2$global_disability == 3] = 1

#SS status variable
ardraw2$ss_status <- NA
ardraw2$ss_status[ardraw2$ssi == 1] = 1
ardraw2$ss_status[ardraw2$ssdi == 1] = 1
ardraw2$ss_status[is.na(ardraw2$ss_status)] <- (0)

#convert age to age ranges
ardraw2$age_ranges<-cut(ardraw2$age, breaks=c(0,18,36,51,65), labels = c("18-35", "36-50", "51-65", "65+"))

#create binary white/nonwhite variable
ardraw2$white <- NA
ardraw2$white[ardraw2$eth_white == 1] = 1
ardraw2$white[ardraw2$eth_black == 1] = 0
ardraw2$white[ardraw2$eth_hispanic == 1] = 0
ardraw2$white[ardraw2$eth_native_am == 1] = 0
ardraw2$white[ardraw2$eth_indian == 1] = 0
ardraw2$white[ardraw2$eth_middle_eastern == 1] = 0
ardraw2$white[ardraw2$eth_pac_islander == 1] = 0
ardraw2$white[ardraw2$ethnicity_other == 1] = 0

#binary gender
ardraw2$bgender <- NA
ardraw2$bgender[ardraw2$gender == "Female"] <- 1
ardraw2$bgender[ardraw2$gender == "Male"] <- 0
ardraw2$bgender <- as.factor(ardraw2$bgender)

#calculate income groups
ardraw2$hhincome <- as.numeric(ardraw2$hhincome)
ardraw2$income_range <- NA
ardraw2$income_range <- cut(ardraw2$hhincome, breaks=c(0,10000,30000,70000,100000),labels = c("<10,000","10,000-30,000", "30,000 - 70,000", "100,000+"))

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

ardraw2$home_sat_mean <- ro

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

# create scale tables and means 
ttbpn2 <- dplyr::select(ardraw2, aut1, aut2, aut3, aut4, aut5, aut6, 
                        rel1, rel2, rel3, rel4, rel5, rel6,
                        com1, com2, com3, com4)
aut2 <- dplyr::select(ardraw2, aut2, aut3, aut4, aut5, aut6)
rel2 <- dplyr::select(ardraw2, rel1, rel2, rel3, rel4, rel5, rel6)
com2 <- dplyr::select(ardraw2, com1, com2, com3, com4)
pac2 <- dplyr::select(ardraw2, trans_pac1, trans_pac2, trans_pac3, trans_pac4)
gse2 <- dplyr::select(ardraw2, gse1, gse2, gse3, gse4)
discr2 <- dplyr::select(ardraw2, discr1, discr2, discr3, discr4)
flour2 <- dplyr::select(ardraw2, flour1, flour2, flour3, flour4, flour5, flour6, flour7, flour8)
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

# calculate log hhincome
ardraw2$loghhincome <- log(ardraw2$hhincome)
ardraw2$loghhincome[which(ardraw2$loghhincome=="-Inf")] = NA # replacing inf w/ NA for one weird case

#calculate binary employment status 
ardraw2$employed <- 0
ardraw2$employed[ardraw2$emp_ft ==1] <- 1
ardraw2$employed[ardraw2$emp_pt ==1] <- 1

# label categorical variables
ardraw2 <- ardraw2 %>% 
  dplyr::mutate(ss_status = recode_factor(ss_status, `1` = "SS Beneficiary", `0` = "Disabled Non-Beneficiary"))
ardraw2 <- ardraw2 %>% 
  dplyr::mutate(gender = recode_factor(gender, `1` = "Female", `0` = "Male", `2` = "Not specified", `2` = "Not specified", .default ="NA"))
ardraw2 <- ardraw2 %>%
  dplyr::mutate(white = recode_factor(white,
                                      `1` = "White",`0` = "Non-white", .default ="NA"))
ardraw2 <- ardraw2 %>%
  dplyr::mutate(education = recode_factor(education,
                                          `1` = "High school or less",
                                          `2` = "High school or less",
                                          `3` = "High school or less",
                                          `4` = "College", 
                                          `5` = "College", 
                                          `6` = "Advanced degree", 
                                          `7` = "Advanced degree", 
                                          `8` = "Advanced degree", .default ="NA"))

ardraw2 <- ardraw2 %>% 
  dplyr::mutate(days_out = dplyr::recode_factor(days_out, `1` = "None", `2` = "1-2 days", `3` = "3-4 days", `4` = "5-6 days", `5` = "7 days", .default ="NA"))
ardraw2 <- ardraw2 %>% 
  dplyr::mutate(travel_comp = dplyr::recode_factor(travel_comp,  `4` = "Very often or always", `3` = "Often", `2` = "Sometimes", `1` = "Not very often", `0` = "Never", .default = "NA"))
ardraw2 <- ardraw2 %>% 
  dplyr::mutate(transit_available = dplyr::recode_factor(transit_available, `1` = "Yes", `0` = "No",  .default ="<NA>"))
ardraw2 <- ardraw2 %>% 
  dplyr::mutate(transit_likely_to_use = dplyr::recode_factor(transit_likely_to_use, `1` = "Very unlikely to use", `2` = "Somewhat unlikely to use", `3` = "Somewhat likely to use", `4` = "Very likely to use",  .default ="<NA>"))

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
travel_diary$loghhincome[which(travel_diary$loghhincome=="-Inf")] = NA # get rid of 1 weird case

# create binary disability status variable
travel_diary$disability_status[is.na(travel_diary$global_disability)] <- NA
travel_diary$disability_status[travel_diary$global_disability == 1] = 0
travel_diary$disability_status[travel_diary$global_disability == 2 | travel_diary$global_disability == 3] = 1
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

# set index group for group analysis
travel_diary <- travel_diary  %>% mutate(disabled = recode_factor(disability_status,  `1` = "Disabled", `0` = "Nondisabled"))

#remove one case w/ no trip data at all
travel_diary<- travel_diary[-c(3),]

