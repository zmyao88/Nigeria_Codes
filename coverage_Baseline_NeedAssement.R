setwd("C:/Users/zmyao/Dropbox/nigeria/Need asse & cov")
require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)
require(data.table)

lga <- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/lgas.csv", stringsAsFactors=F)

#Load original cleaned need assement data
edu_needs_ass <- read.csv("education_cleand_Feb_15.csv", stringsAsFactors=F)
health_needs_ass <- read.csv("health_cleand_Feb_18.csv", stringsAsFactors=F)

#kick out sums
edu_needs_ass <- edu_needs_ass[,c(-11:-16,-4)]
health_needs_ass <- health_needs_ass[,c(-11,-13)]

#melt data and regrepx
edu_ass <- melt(edu_needs_ass,id.vars=1:3)
edu_ass$variable <- str_replace(edu_ass$variable,"student_enrollment.education_facilities.classrooms.num_", "")
names(edu_ass)[4:5] <- c("facili_type", "Need_Assessment_count")
edu_ass$ownership <- NA
edu_ass[str_detect(edu_ass$facili_type, "public"), "ownership"] <- "public"
edu_ass[str_detect(edu_ass$facili_type, "private"), "ownership"] <- "private"
edu_ass$facili_type <- str_replace(edu_ass$facili_type, "_private", "")
edu_ass$facili_type <- str_replace(edu_ass$facili_type, "_public", "")
edu_ass$facili_type <- str_replace(edu_ass$facili_type, "pry", "primary_only")
edu_ass$facili_type <- str_replace(edu_ass$facili_type, "js", "juniors_sec_only")
edu_ass$facili_type <- str_replace(edu_ass$facili_type, "both", "primary_and_junior_sec")


heal_ass <- melt(health_needs_ass,id.vars=c(11,1:2))
heal_ass$variable <- str_replace(heal_ass$variable, "section_c_access_care.existing_facilities.num_", "")
heal_ass$variable <- str_replace(heal_ass$variable, "section_c_access_care.cemoc.num_", "")
names(heal_ass)[4:5] <- c("facili_type", "Need_Assessment_count") 


#load base line data and recode variable for education
edu_base <- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/education_661_outliercleaned.csv", stringsAsFactors=F)
edu_base <- edu_base[,c("X_lga_id", "mylga_state", "mylga", "level_of_education", "school_managed")]
edu_base$school_managed <- str_replace(edu_base$school_managed, "st_gov", "public")
edu_base$school_managed <- str_replace(edu_base$school_managed, "loc_gov", "public")
edu_base$school_managed <- str_replace(edu_base$school_managed, "fed_gov", "public")
edu_base$school_managed <- str_replace(edu_base$school_managed, "priv_profit", "private")
edu_base$school_managed <- str_replace(edu_base$school_managed, "priv_noprofit", "private")
edu_base$school_managed <- str_replace(edu_base$school_managed, "faith_org", "private")

#aggregate
edu_base_agg_manage <- ddply(edu_base, .(X_lga_id, school_managed), nrow)
edu_base_agg_total <- ddply(edu_base, .(X_lga_id), nrow)
names(edu_base_agg_manage)[3] <- "baseLine_total"
names(edu_base_agg_total)[2] <- "baseLine_total"

#load base line data and recode variable for health
heal_base <- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/Health_661_outliercleaned.csv", stringsAsFactors=F)
heal_base <- heal_base[,c("X_lga_id", "mylga_state", "mylga", "facility_type")]
heal_base$facili_type_recode <- NA
heal_base[heal_base$facility_type %in% c("primaryhealthcarecentre", "wardmodelphccentre"), "facili_type_recode"] <- "phcentres"
heal_base[heal_base$facility_type %in% c("primaryhealthclinic"), "facili_type_recode"] <- "clinics"
heal_base[heal_base$facility_type %in% c("healthpostdispensary"), "facili_type_recode"] <- "hpanddispensary"
heal_base[heal_base$facility_type %in% c("generalhospital", "maternity", "comprehensivehealthcentre", 
                                         "cottagehospital"), "facili_type_recode"] <- "generalhospital"
heal_base[heal_base$facility_type %in% c("specialisthospital", "dentalclinic", "teachinghospital", 
                                         "federalmedicalcentre"), "facili_type_recode"] <- "tertiaryhospital"
#aggregate
heal_base_agg<-ddply(heal_base, .(X_lga_id, facili_type_recode), nrow)
names(heal_base_agg) <- c("lga_id", "facili_type","base_line_faciliti_count")

#Combine need assessment & baseline for health
health_combine<- merge(heal_ass, heal_base_agg, all.x=T, all.y=T)
health_combine_total <- ddply(health_combine, .(lga_id), summarise, 
                              Need_Assessment_total = sum(Need_Assessment_count, na.rm=T),
                              base_line_total = sum(base_line_faciliti_count, na.rm=T))
health_combine_total <- merge(health_combine_total, lga, by.x="lga_id", by.y="X_lga_id")
health_combine_total <- health_combine_total[,c(1,5:4,2:3)]

write.csv(health_combine, "health_coverage_breakdown_feb18.csv")
write.csv(health_combine_total, "health_coverage_total_feb18.csv")

######################################################
#2 Senarios analysisi for education Needs assessement#
######################################################
edu_needs_ass2 <- edu_needs_ass

#initialize totals
edu_needs_ass2$total_private <- 0
edu_needs_ass2$total_public <- 0

#save indeces for NAs
na_index <- is.na(edu_needs_ass2)
edu_needs_ass2[is.na(edu_needs_ass2)] <- 0

#save row indeces for A+b=C
priidx<- which( ( (edu_needs_ass2[,4] + edu_needs_ass2[,5]) != edu_needs_ass2[,6] ) | (edu_needs_ass2[,6] == 0) )
priidx_neg <- which(! ( ( (edu_needs_ass2[,4] + edu_needs_ass2[,5]) != edu_needs_ass2[,6] ) | (edu_needs_ass2[,6] == 0) ) )
pubidx<- which( ( (edu_needs_ass2[,7] + edu_needs_ass2[,8]) != edu_needs_ass2[,9] ) | (edu_needs_ass2[,9] == 0) )
pubidx_neg <- which(! ( ( (edu_needs_ass2[,7] + edu_needs_ass2[,8]) != edu_needs_ass2[,9] ) | (edu_needs_ass2[,9] == 0) ) )

edu_needs_ass2[priidx,"total_private"] <- rowSums(edu_needs_ass2[priidx,4:6],na.rm=T)
edu_needs_ass2[pubidx,"total_public"] <- rowSums(edu_needs_ass2[pubidx,7:9],na.rm=T)

# case 1: Single Counting, leave both as it is
edu_needs_ass2[priidx_neg,"total_private"] <- edu_needs_ass2[priidx_neg,6]
edu_needs_ass2[pubidx_neg,"total_public"] <- edu_needs_ass2[pubidx_neg,9]

case1 <- edu_needs_ass2
case1$total_facilities <- rowSums(case1[,10:11])

# case 2: double counting 
edu_needs_ass2[priidx_neg,"total_private"] <-  pmax(edu_needs_ass2[priidx_neg,4], edu_needs_ass2[priidx_neg,5]) 
edu_needs_ass2[pubidx_neg,"total_public"] <- pmax(edu_needs_ass2[pubidx_neg,7], edu_needs_ass2[pubidx_neg,8]) 

case2 <- edu_needs_ass2
case2$total_facilities <- rowSums(case2[,10:11])

### output final result
case1[na_index] <- NA
case2[na_index] <- NA

write.csv(case1, "senario1.csv_single_counting.csv", row.names=F)
write.csv(case2, "senario2_double_counting.csv", row.names=F)


### Merge base line & needs assessment for education sector
names(case1)[10:11] <- c("private", "public")
names(case2)[c(10,11)] <- c("private", "public")
edu_need_ass_manage_case1 <- melt(case1, id=1, measure.vars=c("private", "public"))
edu_need_ass_manage_case2 <- melt(case2, id=1, measure.vars=c("private", "public"))
edu_need_ass_total_case1 <- case1[,c(1,12)]
edu_need_ass_total_case2 <- case2[,c(1,12)]

names(edu_need_ass_manage_case1) <- c("X_lga_id", "school_managed", "need_assessment_total")
names(edu_need_ass_manage_case2) <- c("X_lga_id", "school_managed", "need_assessment_total")
names(edu_need_ass_total_case1) <- c("X_lga_id", "need_assessment_total")
names(edu_need_ass_total_case2) <- c("X_lga_id", "need_assessment_total")

edu_coverage_breakdown_case1<- merge(edu_need_ass_manage_case1, edu_base_agg_manage, all.x=T, all.y=T)
edu_coverage_breakdown_case2<- merge(edu_need_ass_manage_case2, edu_base_agg_manage, all.x=T, all.y=T)
edu_coverage_total_case1 <- merge(edu_need_ass_total_case1, edu_base_agg_total, all.x=T, all.y=T)
edu_coverage_total_case2 <- merge(edu_need_ass_total_case2, edu_base_agg_total, all.x=T, all.y=T)

edu_coverage_breakdown_case1 <- merge(edu_coverage_breakdown_case1, lga)
edu_coverage_breakdown_case2 <- merge(edu_coverage_breakdown_case2, lga)
edu_coverage_total_case1 <- merge(edu_coverage_total_case1, lga)
edu_coverage_total_case2 <- merge(edu_coverage_total_case2, lga)


write.csv(edu_coverage_breakdown_case1, "edu_coverage_breakdown_case1.csv", row.names=F)
write.csv(edu_coverage_breakdown_case2, "edu_coverage_breakdown_case2.csv", row.names=F)
write.csv(edu_coverage_total_case1, "edu_coverage_total_case1.csv", row.names=F)
write.csv(edu_coverage_total_case2, "edu_coverage_total_case2.csv", row.names=F)


#Checking "Both" for education
t <- na.omit(edu_needs_ass)
sum( (t[ ,4] + t[ ,5] ) == t[ ,6]  &  (t[ ,7] + t[ ,8] ) == t[ ,9] )
sum( (t[ ,4] + t[ ,5] ) == t[ ,6] )
sum( (t[ ,7] + t[ ,8] ) == t[ ,9] )
sum((is.na(edu_needs_ass[,4]) | is.na(edu_needs_ass[,5]) ) & !is.na(edu_needs_ass[,6]))