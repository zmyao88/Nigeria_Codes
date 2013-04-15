setwd("c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/randomized samples/health/")
require(RecordLinkage)
library(ggplot2)
require(plyr)
require(stringr)

base <- read.csv("sample3_BASELINE.csv")

faci <- read.csv("sample3_FACILITY.csv")
faci <- subset(faci, select=c("lga_id", "mylga_zone", "mylga_state", "mylga", 
                              "HealthFacilities.ward_name", "HealthFacilities.com_name_h",
                              "HealthFacilities.health_facility_name",  "HealthFacilities.health_facility_type"))
names(faci) <- names(base)
base <- base[, 5:8]
faci <- faci[, 5:8]

base$facility_type <- str_replace(base$facility_type, pattern="wardmodelprimaryhealthcarecentre", replacement="wardmodelphccentre")
base$facility_type <- str_replace(base$facility_type, pattern="dispensary", replacement="healthpostdispensary")
base$facility_type <- str_replace(base$facility_type, pattern="healthpost", replacement="healthpostdispensary")
base$facility_type <- str_replace(base$facility_type, pattern="federalmedicalcare", replacement="federalmedicalcentre")

rpairs <- compare.linkage(base, faci, blockfld=4, strcmp = 1:3, strcmpfun = jarowinkler, )
rpairs <- epiWeights(rpairs)

test <- rpairs[[3]]

test2 <- rpairs[[6]]

summary(rpairs)
getPairs(rpairs, 0.75, 0.7)
result <- epiClassify(rpairs, 0.75)
summary(result)

getParetoThreshold(rpairs)


test <- classifyUnsup(rpairs, method="kmeans")

summary(test)
link_df <- data.frame(cbind(test[[3]], linked = test[[7]]))

test[which(test$facility_type == 1 & test$community > 0.55 & test$ward < 0.55 & test$facility_name < 0.6),]

linked_df_sample <- link_df[link_df$linked == "L" & !is.na(link_df$community) ,]

base[5,]
faci[c(51:61),]

rpairs[[3]]


## PUll the raw data & simple cleaning
facility_list <- read.csv("../../FACILITY_LIST_hospitals.csv")
# table(facility_list$HealthFacilities.health_facility_type)
one13 <- read.csv("../../../../raw_data/113/Health_PhaseII_RoundI&II&III_Clean_2011.11.16.csv")
six61 <- read.csv("../../../outlier_cleaned/Health_661_outliercleaned.csv")
base_line <- rbind.fill(six61,one13)
base_line$facility_type <- str_replace(base_line$facility_type, pattern="wardmodelprimaryhealthcarecentre", replacement="wardmodelphccentre")
base_line$facility_type <- str_replace(base_line$facility_type, pattern="dispensary", replacement="healthpostdispensary")
base_line$facility_type <- str_replace(base_line$facility_type, pattern="healthpost", replacement="healthpostdispensary")
base_line$facility_type <- str_replace(base_line$facility_type, pattern="federalmedicalcare", replacement="federalmedicalcentre")


generic_name_remover <- function(df, col="facility_name")
{
#     df[,col] <- str_replace_all(df[,col], ignore.case("Centre"), "")
#     df[,col] <- str_replace_all(df[,col], ignore.case("Center"), "")
# #     df[,col] <- str_replace_all(df[,col], ignore.case("primary"), "")
#     df[,col] <- str_replace_all(df[,col], ignore.case("hospital"), "")
#     df[,col] <- str_replace_all(df[,col], ignore.case("CLINIC"), "")
#     df[,col] <- str_replace_all(df[,col], '[:punct:]', " ")
    df[,col] <- str_replace_all(df[,col], ignore.case('health|clinic|center|centre|hospital|BASIC|Comprehensive|General|Model|POST|primary|care'), "")
    df[,col] <- str_replace_all(df[,col], ignore.case('^(P|B|)HC|^HC.'), "")
    df[,col] <- str_replace_all(df[,col], ignore.case('^(P|B|).H.(C.|Clinic.|center.|centre|C.(C.|Clinic.|center.|centre.))'), "")
    df[,col] <- str_replace_all(df[,col], ignore.case('(P.H.C)|PHC'), "")
    df[,col] <- str_replace_all(df[,col], " ", "")
    return(df)
}


# Subsetting
base_5 <- subset(base_line, X_lga_id == 470 ,select=c("ward","community",  "facility_name", "facility_type"))
faci_5 <- subset(facility_list, lga_id == 470 ,select=c("HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                      "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type"))

names(faci_5) <- names(base_5)

write.csv(base_5, "../../temp/base.csv", row.names=F)
write.csv(faci_5, "../../temp/faci.csv", row.names=F)


names(faci_5) <- names(base_5)

base_5 <- generic_name_remover(base_5)
faci_5 <- generic_name_remover(faci_5)


rpairs <- compare.linkage(base_5, faci_5, blockfld=4, strcmp = 1:3, strcmpfun = jarowinkler)
# rpairs <- compare.linkage(base_5, faci_5, blockfld=4, phonetic = 1:3, phonfun = pho_h, )

#Sol 1. EM weight
rpairs <- emWeights(rpairs)
summary(rpairs)
getPairs(rpairs, 3.5, -1)
threshold=getParetoThreshold(rpairs,interval=c(-1,3.5))
result <- emClassify(rpairs,threshold)
summary(result)


#Sol 2. Epi weight
rpairs <- epiWeights(rpairs)
threshold <- getParetoThreshold(rpairs, interval=c(0.6, 0.9))
result <- epiClassify(rpairs, threshold)
summary(result)
getPairs(rpairs, 1, 0.6)

link_df <- data.frame(cbind(result[[3]], linked = result[[7]])) 
subset(link_df, linked == "L")


base_5[4,]
faci_5[2,]

base_5[9,]
faci_5[5,]

base_5[23,]
faci_5[1,]

#string distance
base_5[4,]
faci_5[c(5,6,8),]
base_5[8,]
faci_5[c(2,4:7),]
base_5[5,]
faci_5[6,]

#Phonetuc similarity
base_5[8,]
faci_5[c(2,4,7),]
base_5[5,]
faci_5[6,]


getParetoThreshold(rpairs)
optimalThreshold(rpairs)


test <- classifyUnsup(rpairs, method="kmeans")

summary(test)
link_df <- data.frame(cbind(test[[3]], linked = test[[7]]))

test[which(test$facility_type == 1 & test$community > 0.55 & test$ward < 0.55 & test$facility_name < 0.6),]

linked_df_sample <- link_df[link_df$linked == "L" & !is.na(link_df$community) ,]

base[5,]
faci[c(51:61),]

rpairs[[3]]


