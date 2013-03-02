setwd("C:/Users/zmyao/Dropbox/nigeria/Need asse & cov")
require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)
require(data.table)

education <- read.csv("education_cleand_Feb_6.csv", stringsAsFactors=F)
healthy <- read.csv("health_cleand_Feb_18.csv", stringsAsFactors=F)
lga <- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/lgas.csv", stringsAsFactors=F)
source(file="C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/scripts/outlier plot.R")

forumhubOutlier <- function(dt, cols, rows, value) {
    if (any(rows)) {
        set(dt, rows, cols, value) }
}


dir <- "C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/raw_data/Population DATA/"
edu2 <- population(education,target_col="total_all", lga_col="lga_id", work_dir=dir)
health2 <- population(healthy,target_col="total", lga_col="lga_id", work_dir=dir)

edu3 <- pareto(df=edu2,target_col="total_all", perctl=0.99, ratio_col="ratio", output_dir="C:/Rplot/edu.pdf")
health3<-pareto(df=health2,target_col="total", perctl=0.99, ratio_col="ratio", output_dir="C:/Rplot/health.pdf")

edu3 <- pareto(edu3,"student_enrollment.education_facilities.classrooms.num_pry_private", 0.99)
edu3 <- pareto(edu3,"student_enrollment.education_facilities.classrooms.num_js_private", 0.99)
edu3 <- pareto(edu3,"student_enrollment.education_facilities.classrooms.num_both_private", 0.99)
edu3 <- pareto(edu3,"student_enrollment.education_facilities.classrooms.num_pry_public", 0.99)
edu3 <- pareto(edu3,"student_enrollment.education_facilities.classrooms.num_js_public", 0.99)
edu3 <- pareto(edu3,"student_enrollment.education_facilities.classrooms.num_both_public", 0.99)

names(edu3)
quantile(edu3$student_enrollment.education_facilities.classrooms.num_both_public, probs=0.99,na.rm=T)

forumhubOutlier(edu3,"student_enrollment.education_facilities.classrooms.num_pry_private", 
                which(edu3$student_enrollment.education_facilities.classrooms.num_pry_private>10000), NA)
forumhubOutlier(edu3,"student_enrollment.education_facilities.classrooms.num_js_private", 
                which(edu3$student_enrollment.education_facilities.classrooms.num_js_private>1000), NA)
forumhubOutlier(edu3,"student_enrollment.education_facilities.classrooms.num_both_private", 
                which(edu3$student_enrollment.education_facilities.classrooms.num_both_private>2000), NA)
forumhubOutlier(edu3,"student_enrollment.education_facilities.classrooms.num_pry_public", 
                which(edu3$student_enrollment.education_facilities.classrooms.num_pry_public>10000), NA)
forumhubOutlier(edu3,"student_enrollment.education_facilities.classrooms.num_js_public", 
                which(edu3$student_enrollment.education_facilities.classrooms.num_js_public>3000), NA)
forumhubOutlier(edu3, "student_enrollment.education_facilities.classrooms.num_both_public",
               which(edu3$student_enrollment.education_facilities.classrooms.num_both_public > 2500), NA)

edu3$total_private <- rowSums(edu3[,5:7], na.rm=T)
edu3$total_public <- rowSums(edu3[,8:10], na.rm=T)
edu3$total_all <- rowSums(edu3[, c("total_private", "total_public")], na.rm=T)

edu3 <- pareto(edu3,"total_all", 0.99)
write.csv(edu3,"education_cleand_Feb_15.csv", row.names=F)

# done here






lga_sampel <- edu3[sample(602)[1:10],]




apply(edu3[,c("total_private", "total_public", "total_all")], 1, function(z) any(is.na(z)))

edu_base<- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning/in_process_data/outlier_cleaned/education_661_outliercleaned.csv")
names(edu_base)