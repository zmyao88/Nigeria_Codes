setwd("C:/Users/Zmyao/Dropbox/nigeria/Need asse & cov/")
require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)

education <- read.csv("education_cleaned.csv", stringsAsFactors=F)
healthy <- read.csv("health_cleaned.csv", stringsAsFactors=F)
lga <- read.csv("C:/Users/zmyao/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/lgas.csv", stringsAsFactors=F)
lga$lga <- toupper(lga$lga)
lga$state <- toupper(lga$state)
head(lga)

test <- merge(lga,education)
test2 <- merge(lga,healthy)

education$state_lga <- paste(education$state, education$lga, sep=', ')
healthy$state_lga <- paste(healthy$state, healthy$lga, sep=', ')
lga$state_lga <- paste(lga$state, lga$lga, sep=", ")

a <- as.vector(as.matrix(rbind(education$state_lga, healthy$state_lga)))
a <- data.frame(unique(a))

c <- as.vector(as.matrix(lga$state_lga))

a$correct <- NA
pair2 <- vector("list")

for (i in 1:dim(a)[1])
{
    pair2[[i]] <- agrep(a[i,1], c, max=1, value=T, ignore.case=T)
    print(i)
    print(pair2[[i]])
    if (length(pair2[[i]]) == 1)
    {
        a$correct[i] <- pair2[[i]]
    }else{
        a$correct[i] <- NA  
    } 
}
fixed1 <- a[!is.na(a[,2]),]
fixed2 <- read.csv("needs_manual_cleaning_done.csv", stringsAsFactors=F)

fixed_list <- rbind(fixed1,fixed2)
fixed_list <- merge(fixed_list, lga, by.x="correct", by.y="state_lga", all.x=T)

write.csv(fixed_list, "spelling_revise_list.csv", row.names=F)
#write.csv(a[is.na(a[,2]),], "needs_manual_cleaning.csv", row.names=F)
#write.csv(lga$state_lga, "correct_list.csv", row.names=F)


education$lga_id <- NA
for (i in 1:dim(fixed_list)[1])
{
    #i <- 1
    test1 <- expression(education[,"state_lga"] == fixed_list[i, "unique.a."])
    #one48[eval(test1),"lga"]
    #miss_match[1,"corr_lga"]
    education[eval(test1), "lga"] <- fixed_list[i, "lga"]
    education[eval(test1), "state"] <- fixed_list[i, "state"]
    education[eval(test1), "lga_id"] <- fixed_list[i, "X_lga_id"]
    
}
education <- education[!is.na(education$lga_id),]


healthy$lga_id <- NA
for (i in 1:dim(fixed_list)[1])
{
    #i <- 1
    test1 <- expression(healthy[,"state_lga"] == fixed_list[i, "unique.a."])
    #one48[eval(test1),"lga"]
    #miss_match[1,"corr_lga"]
    healthy[eval(test1), "lga"] <- fixed_list[i, "lga"]
    healthy[eval(test1), "state"] <- fixed_list[i, "state"]
    healthy[eval(test1), "lga_id"] <- fixed_list[i, "X_lga_id"]
    
}
healthy <- healthy[!is.na(healthy$lga_id),]

education$total_private <- rowSums(education[,3:5], na.rm=T)
education$total_public <- rowSums(education[,6:8], na.rm=T)
education$total_all <- rowSums(education[, c("total_private", "total_public")], na.rm=T)


healthy$total <- rowSums(healthy[,3:8], na.rm=T, dims=1)


write.csv(healthy, "health_cleand_Feb_6.csv", row.names=F)
write.csv(education, "education_cleand_Feb_6.csv", row.names=F)

########################
# Manual cleaning part #
########################
lga[which(lga$state == "OYO"),]
agri2_uni[which(agri2_uni$state == "OYO"),10:11]
water_uni[which(water_uni$state == "OYO"), 1:2]

lga[which(str_detect(lga$lga, pattern="ILORIN")), ]
agri2_uni[which(str_detect(agri2_uni$lga, pattern="ILORIN")), 10:11]
water_uni[which(str_detect(water_uni$lga, pattern="ILORIN")), 1:2]

View(healthy[which(str_detect(healthy$lga, pattern="OGBOMOSHO")),])
View(lga[which(lga$state == "OYO"),])
#################################
# Manual cleaning part ends here#
#################################





names(agri2_uni)[10:11] <- c("lga", "state")
agri2_uni$state_lga <- paste(agri2_uni$state, agri2_uni$lga, sep=', ')
water_uni$state_lga <- paste(water_uni$state, water_uni$lga, sep=', ')


a <- c(as.vector(as.matrix(agri2_uni$state_lga)), as.vector(as.matrix(water_uni$state_lga))) 
a <- data.frame(unique(a))
c <- as.vector(as.matrix(lga$state_lga))

a$correct <- NA
pair2 <- vector("list")

for (i in 1:dim(a)[1])
{
    pair2[[i]] <- agrep(a[i,1], c, max=1, value=T, ignore.case=T)
    print(i)
    print(pair2[[i]])
    if (length(pair2[[i]]) == 1)
    {
        a$correct[i] <- pair2[[i]]
    }else{
        a$correct[i] <- NA  
    } 
}
write.csv(a[is.na(a[,2]),], "needs_manual_cleaning.csv", row.names=F)

fixed1 <- a[!is.na(a[,2]),]
fixed2 <- read.csv("needs_manual_cleaning_done_agri.csv", stringsAsFactors=F)

fixed_list <- rbind(fixed1,fixed2)
fixed_list <- merge(fixed_list, lga, by.x="correct", by.y="state_lga", all.x=T)

write.csv(fixed_list, "spelling_revise_list_agri.csv", row.names=F)

agri2_uni$lga_id <- NA
for (i in 1:dim(fixed_list)[1])
{
    #i <- 1
    test1 <- expression(agri2_uni[,"state_lga"] == fixed_list[i, "unique.a."])
    #one48[eval(test1),"lga"]
    #miss_match[1,"corr_lga"]
    agri2_uni[eval(test1), "lga"] <- fixed_list[i, "lga"]
    agri2_uni[eval(test1), "state"] <- fixed_list[i, "state"]
    agri2_uni[eval(test1), "lga_id"] <- fixed_list[i, "X_lga_id"]
    
}
agri2_uni <- agri2_uni[!is.na(agri2_uni$lga_id),]

water_uni$lga_id <- NA
for (i in 1:dim(fixed_list)[1])
{
    #i <- 1
    test1 <- expression(water_uni[,"state_lga"] == fixed_list[i, "unique.a."])
    #one48[eval(test1),"lga"]
    #miss_match[1,"corr_lga"]
    water_uni[eval(test1), "lga"] <- fixed_list[i, "lga"]
    water_uni[eval(test1), "state"] <- fixed_list[i, "state"]
    water_uni[eval(test1), "lga_id"] <- fixed_list[i, "X_lga_id"]
    
}
water_uni <- water_uni[!is.na(water_uni$lga_id),]



write.csv(water_uni, "water_cleand_Feb_7.csv", row.names=F)
write.csv(agri2_uni, "agri2_cleand_Feb_7.csv", row.names=F)