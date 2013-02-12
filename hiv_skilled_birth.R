library("foreign")
require(gdata)
require(ply)

setwd("C:/Users/Zaiming/Dropbox/Raw_Data/")
sec_3 <- read.dta("section_3.dta")


sec3_sub <- subset(sec_3, s105y < 5, select=c(state, lg, count, s105y, s3d14, s106))
unique(sec3_sub$s3d14)
count(sec3_sub, vars="s3d14")

sec3_sub$skilled <- NA
sec3_sub[sec3_sub$s3d14==1 | sec3_sub$s3d14==2 | sec3_sub$s3d14==3, "skilled"] <- 1 

pt1 <- ddply(sec3_sub, .(state, lg), summarise, skilled_count = length(count[which(skilled==1)])) 
pt2 <- ddply(sec3_sub, .(state, lg), summarise, total_count = length(count))


result <- merge(pt1,pt2)
result$pct <- result$skilled_count/result$total_count


sec3_sub <- subset(sec_3, (s105y > 14 & s105y<50), select=c(state, lg, count, s105y, s3f05))
count(sec3_sub, vars="s3f05")
sec3_sub$hiv <- NA
sec3_sub[sec3_sub$s3f05=="Y", "hiv"] <- 1 
pt3 <- ddply(sec3_sub, .(state, lg), summarise, hiv_count = length(count[which(hiv==1)])) 
pt4 <- ddply(sec3_sub, .(state, lg), summarise, total_count = length(count))
result2 <- merge(pt3,pt4)
result2$pct <- result2$hiv_count/result2$total_count