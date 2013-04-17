pairs_dist_1 <- function(dataset1, dataset2)
{
    pair_ids = merge(1:nrow(dataset1), 1:nrow(dataset2), 
                     all = TRUE)
    pair_ids = pair_ids[order(pair_ids[, 1], pair_ids[, 2]), ]
    
    left = dataset1[pair_ids[, 1], , drop = FALSE]
    right = dataset2[pair_ids[, 2], , drop = FALSE]
    patterns = matrix(0, ncol = ncol(left), nrow = nrow(left))
    
    
    patterns[, 1] <- left$ward == right$ward
    patterns[, 2] <- jarowinkler(as.character(left$community), as.character(right$community))
    patterns[, 3] <- jarowinkler(as.character(left$facility_name), as.character(right$facility_name))
    patterns[, 4] <- ifelse(left$facility_type == right$facility_type, 1,
                            ifelse(left$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic") & 
                                       right$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic"), 0.5, 0))
    pair_score <- cbind(pair_ids, patterns)
    names(pair_score) <- c("id1", "id2", "ward", "community", "facility_name", "facility_type")
    #     rm(list=c("left", "right", "patterns"))
    return(pair_score)
}

pairs_dist_2 <- function(dataset1, dataset2)
{
    pair_ids = merge(1:nrow(dataset1), 1:nrow(dataset2), 
                     all = TRUE)
    pair_ids = pair_ids[order(pair_ids[, 1], pair_ids[, 2]), ]
    
    left = dataset1[pair_ids[, 1], , drop = FALSE]
    right = dataset2[pair_ids[, 2], , drop = FALSE]
    patterns = matrix(0, ncol = ncol(left), nrow = nrow(left))
    
    
    patterns[, 1] <- jarowinkler(as.character(left$ward), as.character(right$ward))
    patterns[, 2] <- jarowinkler(as.character(left$community), as.character(right$community))
    patterns[, 3] <- jarowinkler(as.character(left$facility_name), as.character(right$facility_name))
    patterns[, 4] <- ifelse(left$facility_type == right$facility_type, 1,
                            ifelse(left$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic") & 
                                       right$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic"), 0.5, 0))
    pair_score <- cbind(pair_ids, patterns)
    names(pair_score) <- c("id1", "id2", "ward", "community", "facility_name", "facility_type")
    #     rm(list=c("left", "right", "patterns"))
    return(pair_score)
}


fill_match <- function(df, id_1, id_2)
{
    final_570 <- df
    id1 <- id_1
    id2 <- id_2
    
    match_pairs <- cbind(id1, id2)
    final_570$match <- 0
    
    for (i in 1:dim(match_pairs)[1])
    {
        final_570[which(final_570$id1 == match_pairs[i,1] &  final_570$id2 == match_pairs[i,2]), "match"] <- 1
    }
    rm(list=c("match_pairs", "id1", "id2"))
    return(final_570)
}



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


# Subsetting
###############LGA == 570

base_570 <- subset(base_line, X_lga_id == 570 ,select=c("ward","community",  "facility_name", "facility_type"))
faci_570 <- subset(facility_list, lga_id == 570 ,select=c("HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                        "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type"))
names(faci_570) <- names(base_570)
row.names(base_570) <- NULL
row.names(faci_570) <- NULL


#fix ward
base_570$ward <- as.character(str_replace(str_replace(base_570$ward, pattern=" ", ""),
            pattern=ignore.case("ward"), ""))

# str_extract(faci_570$ward, "/.*")

faci_570$ward <- as.character(str_replace(str_replace(faci_570$ward, pattern=" ", ""),
                         pattern=ignore.case("ward"), ""))
faci_570$community <- str_extract(faci_570$ward, '[a-zA-Z]+')
faci_570$ward <- as.character(as.numeric(str_extract(faci_570$ward, "[0-9]+")))


base_570 <- colwise(as.character)(base_570)
faci_570 <- colwise(as.character)(faci_570)


final_570 <- pairs_dist_1(base_570, faci_570)

id1 <- c(6, 7, 8, 9, 10, 17, 19, 26, 27, 29, 28)
id2 <- c(14,13, 27, 16, 10, 6, 11, 1, 5, 4, 28)


final_570 <- fill_match(df=final_570, id1, id2)

which(final_570$match == 1 )
################################ LGA==5
base_5 <- subset(base_line, X_lga_id == 5 ,select=c("ward","community",  "facility_name", "facility_type"))
faci_5 <- subset(facility_list, lga_id == 5 ,select=c("HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                          "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type"))
names(faci_5) <- names(base_5)
row.names(base_5) <- NULL
row.names(faci_5) <- NULL


#fix ward
# base_5$ward <- as.character(str_replace(str_replace(base_5$ward, pattern=" ", ""),
#                                         pattern=ignore.case("ward"), ""))
base_5$ward <- str_extract(base_5$ward, '[a-zA-Z]+')

# str_extract(faci_5$ward, "/.*")

faci_5$ward <- as.character(str_replace(str_replace_all(faci_5$ward, pattern=" ", ""),
                                        pattern=ignore.case("ward"), ""))
# faci_5$community <- str_extract(faci_5$ward, '[a-zA-Z]+')
# faci_5$ward <- as.character(as.numeric(str_extract(faci_5$ward, "[0-9]+")))

base_5 <- colwise(as.character)(base_5)
faci_5 <- colwise(as.character)(faci_5)



final_5 <- pairs_dist_2(base_5, faci_5)

id1 <- c(3, 4, 5, 8)
id2 <- c(8, 1, 6, 4)


final_5 <- fill_match(df=final_5, id1, id2)
which(final_5$match == 1 )


for (i in 1: 4)
{
    print( paste(base_5[id1[i], 3], faci_5[id2[i],3], sep="******"))
}


################################ LGA==120

base_120 <- subset(base_line, X_lga_id == 120 ,select=c("ward","community",  "facility_name", "facility_type"))
faci_120 <- subset(facility_list, lga_id == 120 ,select=c("HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                      "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type"))
names(faci_120) <- names(base_120)
row.names(base_120) <- NULL
row.names(faci_120) <- NULL


#fix ward
base_120$ward <- as.character(str_replace(str_replace(base_120$ward, pattern=" ", ""),
                                         pattern=ignore.case("ward"), ""))
# base_120$ward <- str_extract(base_5$ward, '[a-zA-Z]+')

# str_extract(faci_5$ward, "/.*")

faci_120$ward <- as.character(str_replace(str_replace_all(faci_120$ward, pattern=" ", ""),
                                        pattern=ignore.case("ward"), ""))
# faci_5$community <- str_extract(faci_5$ward, '[a-zA-Z]+')
faci_120$ward <- as.character(as.numeric(str_extract(faci_120$ward, "[0-9]+")))

base_120 <- colwise(as.character)(base_120)
faci_120 <- colwise(as.character)(faci_120)



final_120 <- pairs_dist_2(base_120, faci_120)

id1 <- c(3, 4, 5, 6, 8, 9)
id2 <- c(9, 7, 1, 8, 19, 2)


final_120 <- fill_match(df=final_120, id1, id2)
which(final_120$match == 1 )


for (i in 1: 6)
{
    print( paste(base_120[id1[i], 2], faci_120[id2[i],2], sep="******"))
}

################################ LGA==30

base_30 <- subset(base_line, X_lga_id == 30 ,select=c("ward","community",  "facility_name", "facility_type"))
faci_30 <- subset(facility_list, lga_id == 30 ,select=c("HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                          "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type"))
names(faci_30) <- names(base_30)
row.names(base_30) <- NULL
row.names(faci_30) <- NULL


#fix ward
base_30$ward <- as.character(str_replace(str_replace(base_30$ward, pattern=" ", ""),
                                          pattern=ignore.case("ward"), ""))
base_30$ward <- as.character(as.numeric(str_extract(base_30$ward, "[0-9]+")))
# base_30$ward <- str_extract(base_5$ward, '[a-zA-Z]+')

# str_extract(faci_5$ward, "/.*")

faci_30$ward <- as.character(str_replace(str_replace_all(faci_30$ward, pattern=" ", ""),
                                          pattern=ignore.case("ward"), ""))
faci_30$ward <- str_replace_all(faci_30$ward, '[/]', "")

# faci_5$community <- str_extract(faci_5$ward, '[a-zA-Z]+')
faci_30$ward <- as.character(as.numeric(str_extract_all(faci_30$ward, "[0-9]+")))

base_30 <- colwise(as.character)(base_30)
faci_30 <- colwise(as.character)(faci_30)



final_30 <- pairs_dist_1(base_30, faci_30)

id1 <- c(1, 3, 5, 6, 7, 9, 11, 14, 18, 22, 26, 29, 33, 37, 39)
id2 <- c(6, 19, 24, 43, 35, 38, 36, 41, 59, 40, 17, 49, 29, 22, 18)


final_30 <- fill_match(df=final_30, id1, id2)
which(final_30$match == 1 )


for (i in 1: 15)
{
    print( paste(base_30[id1[i], 3], faci_30[id2[i],3], sep="******"))
}

##########################################360
base_360 <- subset(base_line, X_lga_id == 360 ,select=c("ward","community",  "facility_name", "facility_type"))
faci_360 <- subset(facility_list, lga_id == 360 ,select=c("HealthFacilities.ward_name","HealthFacilities.com_name_h",  
                                                        "HealthFacilities.health_facility_name", "HealthFacilities.health_facility_type"))
names(faci_360) <- names(base_360)
row.names(base_360) <- NULL
row.names(faci_360) <- NULL


#fix ward
base_360$ward <- as.character(str_replace(str_replace(base_360$ward, pattern=" ", ""),
                                         pattern=ignore.case("ward"), ""))
base_360$ward <- as.character(as.numeric(str_extract(base_360$ward, "[0-9]+")))
# base_360$ward <- str_extract(base_5$ward, '[a-zA-Z]+')

# str_extract(faci_5$ward, "/.*")

faci_360$ward <- as.character(str_replace(str_replace_all(faci_360$ward, pattern=" ", ""),
                                         pattern=ignore.case("ward"), ""))
faci_360$ward <- str_replace_all(faci_360$ward, '[/]', "")

# faci_5$community <- str_extract(faci_5$ward, '[a-zA-Z]+')
faci_360$ward <- as.character(as.numeric(str_extract_all(faci_360$ward, "[0-9]+")))

base_360 <- colwise(as.character)(base_360)
faci_360 <- colwise(as.character)(faci_360)



final_360 <- pairs_dist_1(base_360, faci_360)


id1 <- c(1, 3, 6, 8, 9, 12, 13, 16, 19, 20, 21, 22, 24, 25, 26, 27, 30, 36, 37, 40)
id2 <- c(18, 19, 6, 17, 27, 32, 12, 36, 26, 5, 7, 25, 38, 14, 31, 2, 11, 29, 34, 16)


final_360 <- fill_match(df=final_360, id1, id2)
which(final_360$match == 1 )


for (i in 1: 20)
{
    print( paste(base_360[id1[i], 3], faci_360[id2[i],3], sep="******"))
}




#####

combined_total <- rbind(final_5, final_30, final_120, final_570, final_360)
sum(combined_total$match == 1)

ggplot(combined_total, aes(x=ward, y = match)) + geom_point(position = "jitter")

cbbPalette <- c("#000000", "#FF0000")
ggplot(combined_total, aes(x=facility_name, y = community, color = factor(match))) + 
    geom_point(aes(shape = factor(match), size = 0.5 * match)) + 
    scale_color_manual(values = cbbPalette)

open3d()
x <- combined_total$ward
y <- combined_total$facility_name
z <- combined_total$community
color_match <- combined_total$match + 1

plot3d(x=x,y=y,z=z, col = color_match, type='s', size=0.5)
rgl.close()

