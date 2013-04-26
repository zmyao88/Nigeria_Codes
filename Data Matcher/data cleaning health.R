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





str_replace(facility_list$ward, ignore.case("ward"), "")
str_extract(faci_test$ward, '[0-9]+')    
which(str_detect(faci_test$base, ignore.case("ward")))

which(str_detect(base_test$ward, '[0-9]'))
which()

test <- facility_list[which(is.na(facility_list$community) ), c("X_lga_id", "ward")]
test$flag <- str_detect(test$ward, '[/,]')

test <- facility_list[which(is.na(facility_list$community) & str_detect(facility_list$ward, '[/,]') ), c("X_lga_id", "ward")]
test <- arrange(test, X_lga_id, ward)

str_extract(test$ward, '.+[/,]')

a1 <- str_extract(test$ward, '[a-zA-Z0-9 \')]+[/,]')
a2 <- str_replace(test$ward, '[a-zA-Z0-9 \')]+[/,]', "")

str_extract(test$ward, '[/]$')

str_extract(test$ward, '.+/.+/')

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
# base_line[which(str_detect(base_line$ward, '[0-9]+')),c("ward", "community")]
# facility_list[which(str_detect(facility_list$ward, '[0-9]+')),c("ward", "community")]
# 
# facility_list[which(str_detect(facility_list$ward, '/')),c("ward", "community")]
facility_list$ward <- str_replace(facility_list$ward, ignore.case("ward"), "")

test <- facility_list[which(is.na(facility_list$community) & str_detect(facility_list$ward, '[/,]') ), c("X_lga_id", "ward")]
#wrad
str_trim(str_replace(str_extract(test$ward, '[a-zA-Z0-9 \')]+[/,]'), "[/,]$", ""))
#comm
str_trim(str_replace(test$ward, '[a-zA-Z0-9 \')]+[/,]', ""))
#pull ward number
facility_list$ward <- str_trim(facility_list$ward)
facility_list$ward[which(str_detect(facility_list$ward, '^[0-9]+$'))] <- str_replace(facility_list$ward[which(str_detect(facility_list$ward, '^[0-9]+$'))], "^0+", "")


## character to numeric convert
sum(is.na(as.numeric(test)))/
    
    
## pull all the characgters
    
sapply(str_extract_all(facility_list$ward[1:5], '[a-zA-Z]'), function(x) paste0(x, collapse=""))

###facility_name
df <- str_replace_all(df, ignore.case('health|clinic|center|centre|hospital|BASIC|Comprehensive|General|Model|POST|primary|care'), "")
df <- str_replace_all(df, ignore.case('^(P|B|)HC|^HC.'), "")
df <- str_replace_all(df, ignore.case('^(P|B|).H.(C.|Clinic.|center.|centre|C.(C.|Clinic.|center.|centre.))'), "")
df <- str_replace_all(df, ignore.case('(P.H.C)|PHC'), "")
df <- str_replace_all(df, " ", "")







#######################
#####Health############
#######################

# for identify
tmp[which(str_detect(tmp$facility_name, ignore.case('PRI.+HEALTH'))), "facility_name"]
tmp[which(str_detect(tmp$facility_name, ignore.case('P.H.C.C'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('P.H.C.+(clinic|centre)|PHC.+(clinic|centre)'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('P(\\.| )H(\\.| )C\\.|pri.+Health.centre'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('B(\\.| )H(\\.| )C\\.|BHC'))), "facility_name"]
tmp[which(str_detect(tmp$facility_name, ignore.case('m\\.'))), "facility_name"]
tmp[which(str_detect(tmp$facility_name, ignore.case('(H/(P|post)|HP)'))), "facility_name"]
tmp[which(str_detect(tmp$facility_name, ignore.case('<(CHC|c.h.c.)>'))), c("facility_name", "facility_type")]
tmp[which(str_detect(tmp$facility_name, ignore.case('(MCHC|M.c.h.c.)'))), c("facility_name", "facility_type")]
tmp[which(str_detect(tmp$facility_name, ignore.case('(maternity)'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('hosp\\.'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('/mat(\\.| )'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('hosp/'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('gen.'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('comp(\\.| )'))), c("facility_name")]
tmp[which(str_detect(tmp$facility_name, ignore.case('h/c |h/c$'))), c("facility_name", "facility_type")]
tmp[which(str_detect(tmp$facility_name, ignore.case('p h c c'))), c("facility_name", "facility_type")]



facility_list[which(str_detect(facility_list$facility_name, ignore.case('primary.+health.'))), "facility_name"]


# for replacing
tmp <- facility_list
tmp$facility_name <- sub('pry.+health.|PRI.+HEALTH',  "Primary Health ", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('center', "Centre", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('B(\\.| )H(\\.| )C\\.|BHC', "Basic Health Centre", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('P.H.C.+(clinic|centre)|PHC.+(clinic|centre)', "PHCC", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('p h c c', "PHCC ", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('P(\\.| )H(\\.| )C\\.|pri.+Health.centre', "PHC", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('(H/(P|post)|health post|HP)', "Health Post", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('hosp\\.', "Hospital", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('/mat(\\.| |)', "/Maternity ", tmp$facility_name, ignore.case=T)
tmp$acility_name <- sub('hosp/', "Hospital/ ", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('gen(\\.| )', "General ", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('comp(\\.| )', "Comprehensive ", tmp$facility_name, ignore.case=T)
tmp$facility_name <- sub('h/c |h/c$', "HC ", tmp$facility_name, ignore.case=T)


tmp$facility_name[sample(1:27074,1000)]

tmp$test <- generic_name_remover(tmp$facility_name)

# tmp$facility_name <- str_replace(tmp$facility_name, ignore.case('pry.+health.'), "Primary Health ")
# tmp$facility_name <- str_replace(tmp$facility_name, ignore.case('center'), "Centre")
# tmp$facility_name <- str_replace(tmp$facility_name, ignore.case('B(\\.| )H(\\.| )C\\.|BHC'), "Basic Health Centre")

#######################
#####Education#########
#######################
tmp <- facility_list


# for identify
tmp[which(str_detect(tmp$school_name, 'comm(\\.| )|comm$')), "school_name"]
tmp[which(str_detect(tmp$school_name, '(sch(\\.| )|sch$)')), "school_name"]

tmp[which(str_detect(tmp$school_name, ignore.case('(sec(\\.| )|sec$)'))), "school_name"]
tmp[which(str_detect(tmp$school_name, ignore.case('(snr(\\.| )|snr$)'))), "school_name"]
# for replacing

tmp$school_name <- sub('comm(\\.| )|comm$',  "Community ", tmp$school_name, ignore.case=T)
tmp$school_name <- sub('(sch(\\.| )|sch$)',  "School ", tmp$school_name, ignore.case=T)
tmp$school_name <- sub('(sec(\\.| )|sec$)',  "Secondary ", tmp$school_name, ignore.case=T)
tmp$school_name <- sub('(snr(\\.| )|snr$)',  "Senior ", tmp$school_name, ignore.case=T)