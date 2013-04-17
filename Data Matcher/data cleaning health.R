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


generic_name_remover <- function(df)
{
    #     df[,col] <- str_replace_all(df[,col], ignore.case("Centre"), "")
    #     df[,col] <- str_replace_all(df[,col], ignore.case("Center"), "")
    # #     df[,col] <- str_replace_all(df[,col], ignore.case("primary"), "")
    #     df[,col] <- str_replace_all(df[,col], ignore.case("hospital"), "")
    #     df[,col] <- str_replace_all(df[,col], ignore.case("CLINIC"), "")
    #     df[,col] <- str_replace_all(df[,col], '[:punct:]', " ")
    df <- str_replace_all(df, ignore.case('health|clinic|center|centre|hospital|BASIC|Comprehensive|General|Model|POST|primary|care'), "")
    df <- str_replace_all(df, ignore.case('^(P|B|)HC|^HC.'), "")
    df <- str_replace_all(df, ignore.case('^(P|B|).H.(C.|Clinic.|center.|centre|C.(C.|Clinic.|center.|centre.))'), "")
    df <- str_replace_all(df, ignore.case('(P.H.C)|PHC'), "")
    df <- str_replace_all(df, " ", "")
    return(df)
}

x<-sample(26181,1000)
paste(faci_test$facility_name[x], faci_test$facility_type[x], sep=" ********** ")

faci_test <- subset(facility_list, select=c("lga_id" ,"HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                    "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type", "ta_name"))
base_test <- subset(base_line, select=c("X_lga_id" ,"ward","community",  "facility_name", "facility_type"))
names(faci_test)[1:5] <- names(base_test)[1:5]


str_replace(faci_test$ward, ignore.case("ward"), "")
str_extract(faci_test$ward, '[0-9]+')    
which(str_detect(faci_test$base, ignore.case("ward")))

which(str_detect(base_test$ward, '[0-9]'))
which()

test <- faci_test[which(is.na(faci_test$community) ), c("X_lga_id", "ward", "ta_name")]
test$flag <- str_detect(test$ward, '[/,]')

test <- faci_test[which(is.na(faci_test$community) & str_detect(faci_test$ward, '[/,]') ), c("X_lga_id", "ward", "ta_name")]
test <- arrange(test, ta_name, X_lga_id, ward)

str_extract(test$ward, '.+[/,]')
str_extract(test$ward, '[/]$')

str_replace(str_extract(test$ward, '[/,].+'), "^[/,]", "")
str_extract(test$ward, '[:alnum:]$')
str_extract(test$ward, '[/,] *[a-zA-Z]+$')
str_extract(test$ward, '[/,].+')
test$ward[1441:1485]


x <- sample(26181,1000)
paste(faci_test$facility_name[x], faci_test$facility_type[x], sep=" ***** ")

length(which(combined_total$facility_name > 0.75 & combined_total$match == 1))
length(which(combined_total$facility_name > 0.75))


length(which(combined_total$unique_name > 0.75 & combined_total$match == 1))
length(which(combined_total$unique_name > 0.75))


length(which((combined_total$unique_name > 0.75 | combined_total$facility_name > 0.75) & combined_total$match ==1 ))
length(which(combined_total$unique_name > 0.75 | combined_total$facility_name > 0.75))

length(which(combined_total$ward==1))
length(which(combined_total$ward == 0 & combined_total$facility_name > 0.75 & combined_total$match ==1 ))
length(which(combined_total$ward == 0 & combined_total$facility_name > 0.75))

## used to fix ward/community
#wrad
str_trim(str_replace(str_extract(test$ward, '.+[/,]'), "[/,]$", ""))  
#comm
str_trim(str_replace(str_extract(test$ward, '[/,] *[a-zA-Z]+$'), '^[/,]', "")) 
#pull ward number
x <- str_extract(faci_test$ward, '[0-9]+')
x <-x[!is.na(x)]
y <- str_replace(x, "^0+", "")





write.csv(test, "../../Data Matcher/community_missing.csv")
which(is.na(base_test$community))
which(base_test$ward == )






subset(combined_total, unique_name == 0 & facility_name > 0.7 & match == 1)