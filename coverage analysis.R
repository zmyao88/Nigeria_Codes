install.packages("gdata")
install.packages("stringr")
require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)

setwd("C:/Users/Zaiming/Dropbox/nigeria/Coverage/")

one48 <- read.csv("148_final_list.csv",stringsAsFactors=F)
faciliti_cover <- read.csv("LGAFacilityCount - Sheet1.csv",stringsAsFactors=F)
faciliti_cover <- subset(faciliti_cover, select=c(LGA, Type, Count))
health <- read.csv("Health_661_999Cleaned.csv",stringsAsFactors=F)
edu <- read.csv("Education_661_999Cleaned.csv", stringsAsFactors=F)
lga <- read.csv("lgas.csv", stringsAsFactors=F)

health_ficiliti <- ddply(health, .(X_lga_id, facility_type), nrow)
edu_level <- ddply(edu, .(X_lga_id, level_of_education), nrow)
faciliti_lga <- merge(faciliti_cover,lga[,1:2], by.x="LGA", by.y="lga")
##### lga name does not match FML, try fuzzy matching here
a <- unique(faciliti_lga[,1])
b <- unique(faciliti_cover[,1])
a <- a[order(a)]
b <- b[order(b)]

#check if there are 2 lga with same name in faciliti cover sheet
duplicated(lga$lga[lga$lga %in% a])
faciliti_err <- as.character(b[!(b %in% a)])
write.csv(faciliti_err,"faciliti_lga_miss.csv")
rstudio::viewData(faciliti_err)

c <- as.vector(as.matrix(lga$lga))

pair1 <- vector("list")
pair2 <- vector("list")
for (i in 1:length(faciliti_err))
{
    pair1[[i]] <- faciliti_err[i]
    pair2[[i]] <- agrep(faciliti_err[i], c, max=2, value=T, ignore.case=F)
}

#############################


# Check the discrepency in the facilities type
z <- unique(faciliti_cover$LGA)
z[order(z)]

x <- data.frame(unique(edu$level_of_education))
y <- data.frame(unique(health$facility_type))
z <- data.frame(unique(faciliti_cover$Type))

write.csv(x,"edu.csv")
write.csv(y,"health.csv")
write.csv(z,"cover.csv")
############################################################


###### Regularization of spelling in Facility type and Lga names
facilitie_fix <- read.xls("faciliti_lga_miss_fixed.xlsx",stringsAsFactors=F)
lga_fix <- read.xls("faciliti_lga_miss.xlsx",stringsAsFactors=F)

for (i in 1: dim(facilitie_fix)[1])
{
    faciliti_cover[faciliti_cover[,"Type"] == facilitie_fix[i,"Err"], "Type"] <- str_replace_all(faciliti_cover[faciliti_cover[,"Type"] == facilitie_fix[i, "Err"], "Type"], facilitie_fix[i, "Err"], facilitie_fix[i, "Correct"])
}


for (i in 1: dim(lga_fix)[1])
{
    faciliti_cover[faciliti_cover[, "LGA"] == lga_fix[i, "err"], "LGA"] <- 
        str_replace_all(faciliti_cover[faciliti_cover[, "LGA"] == lga_fix[i, "err"], "LGA"], 
                        lga_fix[i, "err"], lga_fix[i, "correct"])
}

#Re-calculate the "new facilities" conts in the external cover sheet 
faciliti_cover_2 <- ddply(faciliti_cover, .(LGA, Type), summarise, sum = sum(Count))
faciliti_lga <- merge(faciliti_cover_2,lga[,1:2], by.x="LGA", by.y="lga")
faciliti_lga <- faciliti_lga[,-1]

# combine all types of facilitites(Education & Health)
names(health_ficiliti)[2:3] <- c("nimbs_type", "nimbs_count")
names(edu_level)[2:3] <- c("nimbs_type", "nimbs_count")
all_facility <- rbind(edu_level,health_ficiliti, deparse.level=0)

#Genetrate 148 lga facility list
one48_facil <- merge(all_facility, one48, by="X_lga_id")

#Generate 148 lga facility list with 66lga reference
one48_facil2 <- merge(one48_facil, faciliti_lga, by.x=c("X_lga_id", "nimbs_type"),
                     by.y=c("X_lga_id", "Type"), all.x=T, all.y=F)
final_148_facili_all <- one48_facil2[, c("X_lga_id", "zone", "state", "lga", "nimbs_type", "nimbs_count", "sum")] 
final_148_facili_sub <- final_148_facili_all[which(!is.na(final_148_facili_all$sum)),]

sel <- unique(final_148_facili_sub$nimbs_type)
health <- sel[c(1,5,6)]

final_148_facili_sub$sector <- NA
final_148_facili_sub[final_148_facili_sub$nimbs_type %in% health, "sector"] <- "Health"
final_148_facili_sub[!(final_148_facili_sub$nimbs_type %in% health), "sector"] <- "Education"

# Simple Viz, which is not really simple, and some reshaping work is needed :( 
test <- melt(final_148_facili_sub,id.vars=c(1:5,8))


p <- ggplot(test, aes(x=nimbs_type, y=value,fill=variable)) + 
    geom_bar(stat="identity", position="dodge") +
    facet_wrap(~lga)

library(doSNOW)
registerDoSNOW(makeCluster(4, type = "SOCK"))

myplot <- function(df)
{
    ggplot(df,aes(x=nimbs_type,y=value, fill=variable)) + 
        geom_bar(stat="identity", position="dodge") + 
        labs(title = paste(unique(df$sector), unique(df$lga), sep=", "), x="Facilities Type", y="Count") + 
        scale_fill_discrete(name="Data Source",
                            breaks=c("nimbs_count", "sum"),
                            labels=c("Nims Data", "66 Coverage Data")) + 
        theme(axis.text = element_text(size = rel(0.75)))
        
}


pdf("Viz.pdf", width=15, height=10)  
d_ply(test, .(sector, lga), .fun=myplot, .print=T)
dev.off()


pdf("Viz2.pdf", width=10, height=6) 
print(p)
dev.off()


###### Ratio & Diff
final_148_facili_sub$diff <- final_148_facili_sub$sum - final_148_facili_sub$nimbs_count
final_148_facili_sub$ratio <- final_148_facili_sub$sum / final_148_facili_sub$nimbs_count

myplot2 <- function(df)
{
    ggplot(df,aes(x=nimbs_type,y=diff)) + 
        geom_bar(stat="identity", position="dodge") + 
        labs(title = unique(df$sector), x="Facilities Type", y="Difference = Coverage data - Nims Data") + 
        #scale_fill_discrete(name="Data Source",
        #                    breaks=c("nimbs_count", "sum"),
        #                    labels=c("Nims Data", "66 Coverage Data")) + 
        theme(axis.text = element_text(size = rel(0.33))) + 
        facet_wrap(~lga, ncol=4)
    
}
pdf("Viz2.pdf", width=15, height=10)  
d_ply(final_148_facili_sub, .(sector), .fun=myplot2, .print=T)
dev.off()




myplot3 <- function(df)
{
    ggplot(df,aes(x=nimbs_type,y=ratio)) + 
        geom_bar(stat="identity", position="dodge") + 
        labs(title = unique(df$sector), x="Facilities Type", y="Difference = Coverage data / Nims Data") + 
        #scale_fill_discrete(name="Data Source",
        #                    breaks=c("nimbs_count", "sum"),
        #                    labels=c("Nims Data", "66 Coverage Data")) + 
        theme(axis.text = element_text(size = rel(0.33))) + 
        facet_wrap(~lga, ncol=4)
    
}
pdf("Viz3.pdf", width=15, height=10)  
d_ply(final_148_facili_sub, .(sector), .fun=myplot3, .print=T)
dev.off()

write.csv(final_148_facili_sub, "coverage_result.csv", row.names=F)