require(gdata)
require(plyr)
require(reshape2)

setwd("C:/Users/Zaiming/Desktop/")
one148_new <- read.csv("C:/Users/Zaiming/Desktop/148.csv",  na.strings = "")
one48 <- melt(one148_new,id.vars="STATE", na.rm = T)

one48 <- subset(one48,select=c("STATE", "value"))
names(one48) <- c("state", "lga")


#lgas <- read.csv("lgas.csv")
new_148 <- merge(one48, lgas)

# Save the un-merged lgas in lgas_miss and generate new variable of both state & lga
miss_match <- one48[which(!(one48$lga %in% new_148$lga)),]
miss_match$state_lga <- paste(miss_match$state, miss_match$lga, sep=", ")


lgas_miss <- transform(lgas, state_lga=paste(state, lga, sep=", "))
lgas_miss<- lgas_miss[order(lgas_miss$state,lgas_miss$lga),]

# use fuzzy match in R to compare un-merged lga with all lgas in Marks file then replace 
miss_match$correct <- NA
pair2 <- vector("list")
c <- as.vector(as.matrix(lgas_miss$state_lga))
for (i in 1:dim(miss_match)[1])
{
    
    pair2[[i]] <- agrep(miss_match[i,3], c, max=2, value=T, ignore.case=F)
    print(i)
    print(pair2[[i]])
    if (length(pair2[[i]]) == 1)
    {
        miss_match$correct[i] <- pair2[[i]]
    }else{
        miss_match$correct[i] <- NA  
    } 
}

# Manually fix the lefted 
miss_match[14,"correct"] <- pair2[[14]][1]
miss_match[18,"correct"] <- pair2[[18]][1]
miss_match[31,"correct"] <- pair2[[31]][2]
miss_match[17,"correct"] <- "Gombe, Yalmatu/Deba"
miss_match[2,"correct"] <- "Bayelsa, Kolokuma"
miss_match[25,"correct"] <- "Bayelsa, Ogbia"
miss_match[22,"correct"] <- "Plateau, Langtang South"

# merge with correct spelling file and generate new list of correct and not correct state&lga pairs
miss_match <- merge(miss_match[,c("state","lga", "correct")], lgas_miss[,c("state", "lga", "state_lga")],by.x="correct", by.y="state_lga")
names(miss_match)[2:5] <- c("state", "lga", "corr_state", "corr_lga")
miss_match <- subset(miss_match, select=c("state", "lga",        "corr_state", "corr_lga"))


# Critical HERE change the data type 
miss_match[,1] <- as.character(miss_match[,1])
miss_match[,2] <- as.character(miss_match[,2])
miss_match[,3] <- as.character(miss_match[,3])
miss_match[,4] <- as.character(miss_match[,4])

# Loop through all unmatched lga and replace with correct spelling
for (i in 1:dim(miss_match)[1])
{
    #i <- 1
    test1 <- expression(one48[,"state"] == miss_match[i, "state"] & one48[,"lga"] == miss_match[i, "lga"])
    #one48[eval(test1),"lga"]
    #miss_match[1,"corr_lga"]
    one48[eval(test1), "lga"] <- miss_match[i, "corr_lga"]
    one48[eval(test1), "state"] <- miss_match[i, "corr_state"]
    
}

# Final result here
one48_final <- merge(one48, lgas)
write.csv(one48_final, "148lga_list.csv", row.names=F)














