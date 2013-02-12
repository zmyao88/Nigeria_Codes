require(gdata)
require(stringr)
setwd("C:/Users/Zaiming/")
ref <- read.xls("Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/Nigeria Master Codes_SP.xlsx") 
ref <- ref[,c("LGA_id", "state", "LGA")]

file_location <- "Dropbox/nigeria/pop/PriS_JSS/enrollment_data_Pri_SSJ.xlsx"

primary <- read.xls(file_location, header=T,sheet="Pr1-6 Enrollment",na.strings=c("NA","#DIV/0!","","-"))
primary_age <- read.xls(file_location, header=T,sheet="Age Pr1-6 Enrollment",na.strings=c("NA","#DIV/0!","","-"))
Junior_sec <- read.xls(file_location, header=T,sheet="JS1-3 Enrollment",na.strings=c("NA","#DIV/0!","","-"))
Junior_sec_age <- read.xls(file_location, header=T,sheet="Age JS1-3",na.strings=c("NA","#DIV/0!","","-"))

area_fill <- function(data_frame){
    for (i in 1: dim(data_frame)[1]){
        if (is.na(data_frame[i,"Area"] ) == T ){
            data_frame[i,"Area"] <- data_frame[i-1,"Area"]
        }
        
    }
    return(data_frame)
}



primary <- area_fill(primary)
primary_age <- area_fill(primary_age)
Junior_sec <- area_fill(Junior_sec)
Junior_sec_age <- area_fill(Junior_sec_age)


names(primary)[c(1,3:4)] <- c("state", "Primary_Enroll_Male","Primary_Enroll_Female")
names(primary_age)[c(1,3:4)] <- c("state","Primary_Enroll_age_6_11_Male", "Primary_Enroll_age_6_11_Female")
names(Junior_sec)[c(1,3:4)] <- c("state","Secondary_Enroll_Male", "Secondary_Enroll_Female")
names(Junior_sec_age)[c(1,3:4)] <- c("state","Secondary_Enroll_age_12_14_Male", "Secondary_Enroll_age_12_14_Female")


primary <- primary[,c(1:4)]
primary_age <- primary_age[,c(1:4)]
Junior_sec <- Junior_sec[,c(1:4)]
Junior_sec_age <- Junior_sec_age[,c(1:4)]


primary[,1] <- toupper(primary[,1])
primary_age[,1] <- toupper(primary_age[,1])
Junior_sec[,1] <- toupper(Junior_sec[,1])
Junior_sec_age[,1] <- toupper(Junior_sec_age[,1])

primary[,2] <- toupper(primary[,2])
primary_age[,2] <- toupper(primary_age[,2])
Junior_sec[,2] <- toupper(Junior_sec[,2])
Junior_sec_age[,2] <- toupper(Junior_sec_age[,2])




##########################
# Spelling check in LGA
#######################
t1 <- merge(primary, ref)
t2 <- merge(primary_age, ref)
t3 <- merge(Junior_sec, ref)
t4 <- merge(Junior_sec_age, ref)


jr_miss <- merge(Junior_sec, ref, all.x=T)
jr_miss <- unique(jr_miss[is.na(jr_miss$LGA_id) == T,c("LGA", "state")])
jr_miss <- jr_miss[order(jr_miss$state,jr_miss$LGA),]

ref_miss <- merge(Junior_sec, ref, all.y=T)
ref_miss <- unique(ref_miss[is.na(ref_miss$Secondary_Enroll) == T,c("LGA", "state")])
ref_miss <- ref_miss[order(ref_miss$state,ref_miss$LGA),]


write.csv(jr_miss,"C:/Users/Zaiming/Desktop/nigeria/pop/pr.csv",row.names=F)
write.csv(ref_miss,"C:/Users/Zaiming/Desktop/nigeria/pop/ref.csv",row.names=F)

##################################
####################################
total <- read.csv("Dropbox/nigeria/pop/total_state_lga_population.csv")
js <- total[total$Age_Groups=="10 - 14", ]
js$Male <- round(js$Male*3/5)
js$Female <- round(js$Female*3/5)
js$Total <- round(js$Total*3/5)
js <- js[,3:7]

pri1 <- total[total$Age_Groups=="10 - 14", ] 
pri1 <- transform(pri1, Male = round(Male*2/5), Female = round(Female*2/5), Total=round(Total*2/5))
pri2 <- total[total$Age_Groups=="5 - 9", ]
pri2 <- transform(pri2, Male = round(Male*4/5), Female = round(Female*4/5), Total=round(Total*4/5))
pri1 <- pri1[,3:7]
pri2 <- pri2[,3:7]

pri3 <- merge(pri1,pri2, by=c("state", "LGA"))
pri3 <- transform(pri3, Total=Total.x+Total.y, Male=Male.x+Male.y, Female=Female.x+Female.y)
pri3 <- pri3[,c(1:2,9:11)]

temp1 <- read.csv("Dropbox/nigeria/pop/PriS_JSS/pri_lga_spell_ref.csv")
temp2 <- read.csv("Dropbox/nigeria/pop/PriS_JSS/js_lga_spell_ref.csv")
lga_spell <- function(final_data,temp1){
    for (i in 1: dim(temp1)[1]){
        final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],"LGA"] <- str_replace_all(final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],"LGA"],as.character(temp1[i,3]), as.character(temp1[i,1]))
    }
    return(final_data)
}


primary2 <- lga_spell(primary,temp1)
primary_age2 <- lga_spell(primary_age,temp1)
Junior_sec2 <- lga_spell(Junior_sec,temp2)
Junior_sec_age2 <- lga_spell(Junior_sec_age,temp2)

js2 <- merge(Junior_sec_age2, js)
js2 <- merge(js2, ref)

js2$net_enrollment_ratio_JS_M <- js2$Secondary_Enroll_age_12_14_Male/js2$Male
js2$net_enrollment_ratio_JS_F <- js2$Secondary_Enroll_age_12_14_Female/js2$Female

pri <- merge(primary_age2, pri3)
pri <- merge(pri, ref)

pri$net_enrollment_ratio_Pri_M <- pri$Primary_Enroll_age_6_11_Male/pri$Male
pri$net_enrollment_ratio_Pri_F <- pri$Primary_Enroll_age_6_11_Female/pri$Female


write.csv(js2, "Dropbox/nigeria/pop/indicators/net_enroll_JS_M_F.csv")
write.csv(pri, "Dropbox/nigeria/pop/indicators/net_enroll_Primary_M_F.csv")













full3[full3$Secondary_Enroll_age_12_14 == 0, "Secondary_Enroll_age_12_14"] <- NA
full3[full3$Secondary_Enroll == 0, "Secondary_Enroll"] <- NA
full3[full3$Primary_Enroll == 0, "Primary_Enroll"] <- NA
full3[full3$Primary_Enroll_age_6_11 == 0, "Primary_Enroll_age_6_11"] <- NA

write.csv(full3, "C:/Users/Zaiming/Desktop/nigeria/pop/PriS_JSS/final_result.csv",row.names=F)



# NEt enrollment 
full3$net_enrollment_ratio_primary_education <- full3$Primary_Enroll_age_6_11/full3$Total_Pop_pri_6_11
full3$net_enrollment_ratio_secondary_education <- full3$Secondary_Enroll_age_12_14/full3$Total_Pop_JS_12_14
net_enroll <- full3[,c(1:3,10:11)]
net_enroll[na.omit(net_enroll[,5] > 1),5] <- NA
net_enroll[na.omit(net_enroll[,4] > 1),4] <- NA
write.csv(net_enroll,"C:/Users/Zaiming/Desktop/nigeria/pop/PriS_JSS/net_enrollment.csv",row.names=F)



