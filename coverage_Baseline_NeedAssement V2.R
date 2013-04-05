setwd("C:/Users/zmyao/Dropbox/Needs Assessment/Scripts/csv file")

require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)
require(data.table)

lga <- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/lgas.csv", stringsAsFactors=F)

##################
##### Health #####
##################

health_needs_ass <- read.csv("health_cleand_Feb_18.csv", stringsAsFactors=F)
health_needs_ass <- health_needs_ass[,c(-11,-13)]

#melt data and regrepx
heal_ass <- melt(health_needs_ass,id.vars=c(11,1:2))
heal_ass$variable <- str_replace(heal_ass$variable, "section_c_access_care.existing_facilities.num_", "")
heal_ass$variable <- str_replace(heal_ass$variable, "section_c_access_care.cemoc.num_", "")
names(heal_ass)[4:5] <- c("facili_type", "Need_Assessment_count") 


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

#Health TOtal witg NAs
health_needs_ass <- read.csv("health_cleand_Feb_18.csv", stringsAsFactors=F)
health_total <- health_needs_ass[,c("lga_id", "total")]
names(health_total)[2] <- "need_assessment_total" 
heal_base_agg_total<-ddply(heal_base, .(X_lga_id), nrow)
names(heal_base_agg_total) <- c("lga_id", "base_line_faciliti_count")

health_combine_total <- merge(health_total, heal_base_agg_total, all.x=T, all.y=T)
health_combine_total<- merge( lga, health_combine_total, by.y="lga_id", by.x="X_lga_id")

write.csv(health_combine, "health_coverage_breakdown_Mar4.csv",row.names=F)
write.csv(health_combine_total, "health_coverage_total_Mar4.csv", row.names=F)




###################
#### Education ####
###################

#Load original cleaned need assement data
edu_needs_ass <- read.csv("education_cleand_Feb_15.csv", stringsAsFactors=F)
edu_needs_ass <- edu_needs_ass[,c(-11:-16,-4)]

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

case1[1:ncol(na_index)][na_index] <- NA 
case2[1:ncol(na_index)][na_index] <- NA


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

##Remove the outlier manually
edu_coverage_total_case2[which(edu_coverage_total_case2$need_assessment_total >= 1000), "need_assessment_total"] <- NA
rownames(edu_coverage_total_case2) <- NULL

write.csv(edu_coverage_breakdown_case1, "edu_coverage_breakdown_case1.csv", row.names=F)
write.csv(edu_coverage_breakdown_case2, "edu_coverage_breakdown_case2.csv", row.names=F)
write.csv(edu_coverage_total_case1, "edu_coverage_total_case1.csv", row.names=F)
write.csv(edu_coverage_total_case2, "edu_coverage_total_case2_Mar_04.csv", row.names=F)



### Create Education Public total tabel 
edu_need_ass_public_case2 <- case2[,c(1,11)]
names(edu_need_ass_public_case2) <- c("X_lga_id", "need_assessment_public")
edu_base_agg_public <- na.omit(edu_base_agg_manage[edu_base_agg_manage$school_managed=="public",])
names(edu_base_agg_public)[3] <- "baseline_public"
edu_need_ass_public_case2 <- merge(edu_need_ass_public_case2, edu_base_agg_public[,c(1,3)], all.x=T, all.y=T)
edu_need_ass_public_case2 <- merge(edu_need_ass_public_case2, lga)
write.csv(edu_need_ass_public_case2, "edu_coverage_public_case2_Mar_05.csv", row.names=F)
