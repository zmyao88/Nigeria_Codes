pairs_dist_1 <- function(dataset1, dataset2)
{
    pair_ids = merge(1:nrow(dataset1), 1:nrow(dataset2), 
                     all = TRUE)
    pair_ids = pair_ids[order(pair_ids[, 1], pair_ids[, 2]), ]
    
    left = dataset1[pair_ids[, 1], , drop = FALSE]
    right = dataset2[pair_ids[, 2], , drop = FALSE]
    patterns = matrix(0, ncol = ncol(left) , nrow = nrow(left))
    
    
    patterns[, 1] <- as.numeric(left$X_lga_id)
    patterns[, 2] <- left$ward == right$ward
    patterns[, 3] <- jarowinkler(as.character(left$community), as.character(right$community))
    patterns[, 4] <- jarowinkler(as.character(left$facility_name), as.character(right$facility_name))
    patterns[, 6] <- jarowinkler(as.character(left$unique_name), as.character(right$unique_name))
    patterns[, 5] <- ifelse(left$facility_type == right$facility_type, 1,
                            ifelse(left$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic") & 
                                       right$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic"), 0.5, 0))
    pair_score <- cbind(pair_ids, patterns)
    names(pair_score) <- c("id1", "id2", "X_lga_id", "ward", "community", "facility_name", "facility_type", "unique_name")
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
    patterns = matrix(0, ncol = ncol(left) , nrow = nrow(left))
    
    patterns[, 1] <- as.numeric(left$X_lga_id)
    patterns[, 2] <- jarowinkler(as.character(left$ward), as.character(right$ward))
    patterns[, 3] <- jarowinkler(as.character(left$community), as.character(right$community))
    patterns[, 4] <- jarowinkler(as.character(left$facility_name), as.character(right$facility_name))
    patterns[, 6] <- jarowinkler(as.character(left$unique_name), as.character(right$unique_name))
    patterns[, 5] <- ifelse(left$facility_type == right$facility_type, 1,
                            ifelse(left$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic") & 
                                       right$facility_type %in% c("primaryhealthcarecentre", "primaryhealthclinic"), 0.5, 0))
    pair_score <- cbind(pair_ids, patterns)
    names(pair_score) <- c("id1", "id2", "X_lga_id", "ward", "community", "facility_name", "facility_type", "unique_name")
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


generic_name_remover <- function(df)
{
    #     df[,col] <- str_replace_all(df[,col], ignore.case("Centre"), "")
    #     df[,col] <- str_replace_all(df[,col], ignore.case("Center"), "")
    # #     df[,col] <- str_replace_all(df[,col], ignore.case("primary"), "")
    #     df[,col] <- str_replace_all(df[,col], ignore.case("hospital"), "")
    #     df[,col] <- str_replace_all(df[,col], ignore.case("CLINIC"), "")
    #     df[,col] <- str_replace_all(df[,col], '[:punct:]', " ")
    df <- str_replace_all(df, ignore.case('health|clinic|center|centre|hospital|BASIC|Comprehensive|General|Model|POST|primary|care|maternity'), "")
    df <- str_replace_all(df, ignore.case('^(P|B|)HC|^HC.'), "")
    df <- str_replace_all(df, ignore.case('^(P|B|).H.(C.|Clinic.|center.|centre|C.(C.|Clinic.|center.|centre.))'), "")
    df <- str_replace_all(df, ignore.case('(P.H.C)|PHC'), "")
    df <- str_replace_all(df, ignore.case('CHC|C.H.c'), "")
    df <- str_trim(df)
    return(df)
}


## PUll the raw data & simple cleaning
facility_list <- read.csv("../../FACILITY_LIST_hospitals.csv", stringsAsFactors = F)
# table(facility_list$HealthFacilities.health_facility_type)
one13 <- read.csv("../../../../raw_data/113/Health_PhaseII_RoundI&II&III_Clean_2011.11.16.csv", stringsAsFactors = F)
six61 <- read.csv("../../../outlier_cleaned/Health_661_outliercleaned.csv", stringsAsFactors = F)
base_line <- rbind.fill(six61,one13)
base_line$facility_type <- str_replace(base_line$facility_type, pattern="wardmodelprimaryhealthcarecentre", replacement="wardmodelphccentre")
base_line$facility_type <- str_replace(base_line$facility_type, pattern="dispensary", replacement="healthpostdispensary")
base_line$facility_type <- str_replace(base_line$facility_type, pattern="healthpost", replacement="healthpostdispensary")
base_line$facility_type <- str_replace(base_line$facility_type, pattern="federalmedicalcare", replacement="federalmedicalcentre")



base_line <- subset(base_line, select=c("X_lga_id", "ward","community",  "facility_name", "facility_type"))
facility_list <- subset(facility_list, select=c("lga_id", "ward_name","com_name_h",  
                                                          "health_facility_name", "health_facility_type"))
names(facility_list) <- names(base_line)




# Subsetting
###############LGA == 570

base_570 <- subset(base_line, X_lga_id == 570)
faci_570 <- subset(facility_list, X_lga_id == 570)
base_570$unique_name <- generic_name_remover(base_570$facility_name)
faci_570$unique_name <- generic_name_remover(faci_570$facility_name)


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
base_5 <- subset(base_line, X_lga_id == 5)
faci_5 <- subset(facility_list, X_lga_id == 5)


base_5$unique_name <- generic_name_remover(base_5$facility_name)
faci_5$unique_name <- generic_name_remover(faci_5$facility_name)

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

base_120 <- subset(base_line, X_lga_id == 120)
faci_120 <- subset(facility_list, X_lga_id == 120)
names(faci_120) <- names(base_120)
base_120$unique_name <- generic_name_remover(base_120$facility_name)
faci_120$unique_name <- generic_name_remover(faci_120$facility_name)

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

base_30 <- subset(base_line, X_lga_id == 30)
faci_30 <- subset(facility_list, X_lga_id == 30)

base_30$unique_name <- generic_name_remover(base_30$facility_name)
faci_30$unique_name <- generic_name_remover(faci_30$facility_name)

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
base_360 <- subset(base_line, X_lga_id == 360)
faci_360 <- subset(facility_list, X_lga_id == 360)

base_360$unique_name <- generic_name_remover(base_360$facility_name)
faci_360$unique_name <- generic_name_remover(faci_360$facility_name)

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




##### Combine the data

combined_total <- rbind(final_5, final_30, final_120, final_570)
sum(combined_total$match == 1)


#### simple visualization
ggplot(combined_total, aes(x=ward, y = match)) + geom_point(position = "jitter")

cbbPalette <- c("#000000", "#FF0000")
ggplot(combined_total, aes(x=community, y = facility_name, color = factor(match))) + 
    geom_point(aes(shape = factor(match), size = 0.5 * match)) + 
    scale_color_manual(values = cbbPalette)

open3d()
x <- combined_total$facility_name
y <- combined_total$ward
z <- combined_total$community
color_match <- combined_total$match + 1

plot3d(x=x,y=y,z=z, xlab="Facility_Name", ylab="Ward", zlab="Comunity",col = color_match, type='s', size=0.5)
rgl.close()



#### Training logistic regression based on 4 lgas and validata on 1 lga
train_df <-(combined_total)
exp <- as.matrix(train_df[,3:7])
resp <- as.matrix(train_df[,8])

# model_1 <- cv.glmnet(x=exp, y=resp, family='binomial')
# pred <- predict(model_1, exp)
# plot(pred)

model_2 <- glm(factor(match) ~ ward + community + facility_name +
                   facility_type, family=binomial, data=combined_total)
summary(model_2)
confint(model_2)
confint.default(model_2)
logLik(model_2)

plot(model_2$fitted.values)
train_df$fitted <- predict(model_2, combined_total, type="response")
final_360$fitted <- predict(model_2, final_360,type="response")


ggplot(train_df, aes(x=facility_name, y = fitted, color = factor(match))) + 
    geom_point(aes(shape = factor(match), size = 0.5 * match), position ="jitter") + 
    scale_color_manual(values = cbbPalette) +
    geom_hline(yintercept= 0.09)


ggplot(ttt, aes(x=community, y = fitted, color = factor(match))) + 
    geom_point(aes(shape = factor(match), size = 0.5 * match)) + 
    scale_color_manual(values = cbbPalette) +
    geom_hline(yintercept= 0.09) + 
    labs(title = "cutoff at 0.09 which is very low probability to be honest")




sum(train_df$match == 1)
length(which(train_df$fitted > 0.09))
length(which(train_df$fitted > 0.09 & train_df$match == 1))

final_360$fitted <- pred
length(which(final_360$fitted > 0.07))
length(which(final_360$match == 1))
length(which(final_360$match == 1 & final_360$fitted > 0.06))





train_df[which(train_df$fitted < 0.12 & train_df$match ==1),]




###### Evualtion: counting number of records ta has to go through to find match with recommendation
test <- arrange(train_df, X_lga_id, id1, desc(fitted))
test$order <- c(rep(1:9, 10), rep(1:59, 40), rep(1:19, 9), rep(1:37, 29))
count_test <- test[which(test$match == 1),c("id1", "order", "fitted")]

test2 <- arrange(final_360, desc(fitted),na.last=T)
length(which(test2$match ==1))
test3 <- arrange(final_360, id1, desc(fitted))
test3$order <- rep(1:42,40)



count_test3 <- test3[which(test3$match == 1),c("id1", "order", "fitted")]
count <- rbind(count_test, count_test3)
count$index <- 1: nrow(count)
tt <- rbind(test,test3)

length(which(tt$order <= 3 & tt$fitted <= 0.05))

length(which(count$order <= 3 ))

ggplot(count,aes(index, order)) + geom_point() +
    labs(title="most matched pairs are within the top5 \n the point on top right is due to NA's ")


ggplot(count,aes(index, fitted, label=order)) + geom_text() +
    labs(title="pred_probability of matches and order/rank \n ignore x axis")




length(which(test$order <= 4 & test$match == 1 & test$X_lga_id == 30))
length(which(test$match == 1 & test$X_lga_id == 30))

p5 <- ggplot(subset(test3, order <= 4),aes(x=id1, y=fitted, color=factor(match), label=order)) +
    #     geom_point(aes(shape = factor(match))) + 
    scale_color_manual(values = cbbPalette) +
    geom_text() +
    labs(title = "lga_id == 360 \n captured 18 out of 20 matches")+ ylab("Predicted probalility of being matched")

test <- test[test$order <= 4,]
p1 <- ggplot(subset(test, X_lga_id == 30 & order <= 4),aes(x=id1, y=fitted, color=factor(match), label=order)) +
#     geom_point(aes(shape = factor(match))) + 
    scale_color_manual(values = cbbPalette) +
    geom_text() +
    labs(title = "lga_id == 30 \n captured 13 out of 15 matches")+ ylab("Predicted probalility of being matched")


test[test$order <= 4 & test$match ==1 & test$X_lga_id == 5,]
test[test$match ==1 & test$X_lga_id == 5,]


pdf()
p1
p2
p3
p4
p5
dev.off()



sum <- 0
for (i in 0:39)
{
    k <- 1
    for(j in 1:42)
    {
        sum <- sum + 1
        if(test3$match[i*42+j] == 1)
        {
            print(paste(i, j, i*42+j,sep="***"))
            break
        }
        
    }
}
count_pt1 <- test3[which(test3$match == 1),c("id1", "order")]

count <- matrix(nrow=40,ncol=2)
count[,1] <- 1:40

for (i in 1:40)
{
    if (i %in% count_pt1$id1)
    {
        count[i,2] <- count_pt1[count_pt1$id1 == i, 2]
    }else
    {
        count[i,2] <- (42 - sum((1:(i-1) %in% count_pt1$id1)))
    }
}
sum(count[,2])
length(which(test3$match == 1))