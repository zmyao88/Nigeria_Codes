setwd("C:/Users/Zmyao/Dropbox/nigeria/Need asse & cov/")
source(file="C:/Users/zmyao/Documents/GitHub/formhub.R/R/formhub.R")
require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)

#agri2 <- read.csv("agriculture_2_2013_01_28_15_56_57.csv", stringsAsFactors=F,na.strings = "n/a")
#edu <- read.csv("education_2013_01_29_11_13_21.csv", stringsAsFactors=F,na.strings = "n/a")
#edu2 <- read.csv("education_2_2013_01_28_16_03_06.csv", stringsAsFactors=F,na.strings = "n/a")
#health <- read.csv("health_2013_01_29_11_14_35.csv", stringsAsFactors=F,na.strings = "n/a")
#health2 <- read.csv("healthy_2013_01_28_16_06_05.csv", stringsAsFactors=F,na.strings = "n/a")
#water <- read.csv("water_2013_01_29_11_32_31.csv", stringsAsFactors=F,na.strings = "n/a")
#water2 <- read.csv("water_2_2013_01_28_16_09_27.csv", stringsAsFactors=F,na.strings = "n/a")
#coverage <- read.csv("NMIS_FacilityNumbers_for_CoverageAnalysis_2013_01_28_16_12_30.csv", stringsAsFactors=F,na.strings = "n/a")

#which(duplicated(edu2))
#time1 <- strptime(a[,1], format="%Y-%m-%dT%H:%M:%S")

agri2 <- formhubRead("Feb_1_agriculture_2.csv", "agriculture_2.json",na.strings="")@data
edu <- formhubRead("Feb_1_education_1.csv", "education.json",na.strings="")@data
edu2 <- formhubRead("Feb_1_education_2.csv", "education_2.json",na.strings="")@data
health <- formhubRead("Feb_1_health_1.csv", "health.json",na.strings="")@data
health2 <- formhubRead("Feb_1_health_2.csv", "healthy.json",na.strings="")@data
water <- formhubRead("feb_1_water_1.csv", "water.json",na.strings="")@data
water2 <- formhubRead("water_2_2013_01_28_16_09_27.csv", "water_2.json")@data
coverage <- formhubRead("NMIS_FacilityNumbers_for_CoverageAnalysis_2013_01_28_16_12_30.csv", "NMIS_FacilityNumbers_for_CoverageAnalysis.json")@data


df_uni<- function(df, lga, state)
{
    #lga <- "lga"
    #state <- "state"
    #df <- edu
    #lga <- as.character(lga)
    df2 <- unique(df)
    df2 <- df2[which(rowSums(is.na(df2[,]),1) != ncol(df2)),]
    df2 <- df2[order(df2[,lga], df2[,state]), ]
    
    # Select the records with minimal NA's
    na_count <- function(df)
    {
        df$na_count <- rowSums(is.na(df[,]),1)
        #df <- transform(df, na_count = rowSums(is.na(df[,]),1))
        df <- df[order(df$na_count, decreasing = FALSE),]
        df <- head(df,n=1)
        #return(df)
    }
    
    df3 <- ddply(df2, c(lga, state), .fun=na_count)
    df3 <- df3[!is.na(df3[,lga]) & !is.na(df3[,state]), ]
    return(df3)
}

df_valid <- function(df_old, df_new, state, lga)
{
    ifelse( (dim(unique(df_old[,c(state, lga)]))[1] == dim(df_new)[1]) | 
                ((dim(unique(df_old[,c(state, lga)]))[1]-1) == dim(df_new)[1]), "Correct", "Error") 
}

df_uni2<- function(df, lga, state, submit_time)
{
    #lga <- "identification.lga"
    #state <- "identification.state"
    #submit_time="end"
    #df <- edu2
    
    df2 <- unique(df)
    df2 <- df2[which(rowSums(is.na(df2[,]),1) != ncol(df2)),]
    df2 <- df2[order(df2[,state], df2[,lga]), ]
    
    # Select the records with minimal NA's
    na_count <- function(df)
    {
        df$na_count <- rowSums(is.na(df[,]),1)
        #df <- transform(df, na_count = rowSums(is.na(df[,]),1))
        df <- df[order(df$na_count, decreasing = FALSE),]
        df <- head(df,n=1)
        #return(df)
    }
    
    time_order <- function(df)
    {
        df <- df[order(df[,submit_time], decreasing = T),]
        df <- head(df,n=1)    
    }
    
    df3 <- ddply(df2, c(lga, state), .fun=time_order)
    df3 <- df3[!is.na(df3[,lga]) & !is.na(df3[,state]), ]
    return(df3)
}



#edu2_test<- df_uni2(edu2,lga="identification.lga", state="identification.state", submit_time="end")
#names(edu2)



###
edu_uni <- df_uni(edu,lga="lga", state="state")
df_valid(edu, edu_uni, "state", "lga")

health_uni <- df_uni(health,lga="lga", state="state")
df_valid(health, edu_uni, "state", "lga")

water_uni <- df_uni(water,lga="lga", state="state")
df_valid(water, water_uni, "state", "lga")



agri2_uni <- df_uni2(agri2, lga="identification.lga", state="identification.state", submit_time="end")
df_valid(agri2, agri2_uni, "identification.state", "identification.lga")

edu2_uni <- df_uni2(edu2, lga="identification.lga", state="identification.state", submit_time="end")
df_valid(edu2, edu2_uni, "identification.state", "identification.lga")

health2_uni <- df_uni2(health2, lga="identification.lga", state="identification.state", submit_time="end")
df_valid(health2, health2_uni, "identification.state", "identification.lga")

water2_uni <- df_uni2(water2, lga="identification.lga", state="identification.state", submit_time="end")
df_valid(water2, water2_uni, "identification.state", "identification.lga")

edu_nm <- c("student_enrollment.education_facilities.classrooms.num_pry_private", 
            "student_enrollment.education_facilities.classrooms.num_js_private",
            "student_enrollment.education_facilities.classrooms.num_both_private",
            "student_enrollment.education_facilities.classrooms.num_pry_public",	
            "student_enrollment.education_facilities.classrooms.num_js_public",
            "student_enrollment.education_facilities.classrooms.num_both_public")
heal_nm <- c("section_c_access_care.existing_facilities.num_phcentres",
             "section_c_access_care.existing_facilities.num_wards_nophcentre",                                
             "section_c_access_care.existing_facilities.num_clinics",                                         
             "section_c_access_care.existing_facilities.num_wards_noclinic",
             "section_c_access_care.existing_facilities.num_wards_average_hc",
             "section_c_access_care.existing_facilities.num_hpanddispensary")
names(health2_uni)
education1 <- edu_uni[,c("lga", "state", edu_nm)]
education2 <- edu2_uni[,c("identification.lga", "identification.state", edu_nm )]
healthy1 <- health_uni[,c("lga", "state", heal_nm)]
healthy2 <- health2_uni[,c("identification.lga", "identification.state", heal_nm )]

waterr2 <- water2_uni[,c(9:10,124:135)]

names(education2)[1:2] <- c("lga", "state")
names(healthy2)[1:2] <- c("lga", "state")

education <- rbind(education1,education2)
healthy <- rbind(healthy1,healthy2)


write.csv(education, "education_cleaned.csv", row.names=F)
write.csv(healthy, "health_cleaned.csv", row.names=F)