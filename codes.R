require(stringr)
require(gdata)
ref <- read.xls("~/Dropbox//Nigeria 661 Baseline Data Cleaning (2)/external data/Nigeria Master Codes_SP.xlsx")
ref <- ref[,c("LGA_id", "state", "LGA")]
setwd("~/Dropbox/nigeria/pop/indicators/")


##### pt1 #####
#   primary   #
###############
primary <- read.xls("keyLGAPrimaryTabular.xls")

pri_lga <- primary[4:777,1:2]
pri_lga[,1] <- as.character(pri_lga[,1]) 
pri_lga[which(pri_lga[,1] == ""), 1] <- NA
names(pri_lga) <- c("Area", "lga")

area_fill <- function(data_frame){
  for (i in 1: dim(data_frame)[1]){
    if (is.na(data_frame[i,"Area"] ) == T ){
      data_frame[i,"Area"] <- data_frame[i-1,"Area"]
    }
    
  }
  return(data_frame)
}

pri_lga <- area_fill(pri_lga)

p1 <- primary[4:777, c("X.67","X.68")]
names(p1) <- c("repetition_rate_primary_male", "repetition_rate_primary_female")
p2 <- primary[4:777, c("X.64","X.65")]
names(p2) <- c("transition_rate_primary_to_js1_male", "transition_rate_primary_to_js1_female")
p3 <- data.frame(primary[4:777, "X.6"])
names(p3) <- "girl_boy_ratio_primary"
p4 <- data.frame(primary[4:777, "X.3"])
names(p4) <- "gender_parity_index_primary"

pt1 <- as.data.frame(cbind(pri_lga, p1, p2, p3, p4))

##### pt2 #####
#  secondary  #
###############
secondary <- read.xls("keyLGAJSSTabular.xls")
sec_lga <- secondary[4:777,1:2]
sec_lga[,1] <- as.character(sec_lga[,1]) 
sec_lga[which(sec_lga[,1] == ""), 1] <- NA
names(sec_lga) <- c("Area", "lga")

sec_lga <- area_fill(sec_lga)
p1 <- data.frame(primary[4:777, "X.6"])
names(p1) <- "girl_boy_ratio_js"
p2 <- data.frame(primary[4:777, "X.3"])
names(p2) <- "gender_parity_index_js"
pt2 <- as.data.frame(cbind(sec_lga, p1, p2))


######### pt3 ##########
#  lga spell checking  #
########################
pt1$lga <- as.character(pt1$lga)
pt2$lga <- as.character(pt2$lga)

pt1$lga <- toupper(pt1$lga)
pt2$lga <- toupper(pt2$lga)
pt1$Area <- toupper(pt1$Area)
pt2$Area <- toupper(pt2$Area)
ref$lga <- toupper(ref$LGA)

names(pt1)[1:2] <- c("state", "LGA")
names(pt2)[1:2] <- c("state", "LGA")

temp1 <- read.csv("~/Dropbox/nigeria/pop/PriS_JSS/pri_lga_spell_ref.csv")
temp2 <- read.csv("~/Dropbox/nigeria/pop/PriS_JSS/js_lga_spell_ref.csv")
lga_spell <- function(final_data,temp1){
  for (i in 1: dim(temp1)[1]){
    final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],"LGA"] <- str_replace_all(final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],"LGA"],as.character(temp1[i,3]), as.character(temp1[i,1]))
  }
  return(final_data)
}


primary2 <- lga_spell(pt1,temp1)
Junior2 <- lga_spell(pt2,temp2)

primary3 <- merge(primary2, ref)
Junior3 <- merge(Junior2, ref)

final <- merge(primary3, Junior3)
write.csv(final,"~/Dropbox/nigeria/pop/indicators/Others.csv")