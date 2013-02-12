require(stringr)
require(gdata)
require(plyr)

file_location <- "C:/Users/Zaiming/Desktop/nigeria/pop/Age groups by population(Combined Raw Data).xlsx"
a1 <- read.csv("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/hnlss_LGA_Reference_Sheet.csv")
state_list <- as.character(unique(a1[,"state.x"]))
state_list <- state_list[order(state_list)]


# check if every lga table has only 4 columns
debug_col <- function(file_location, state_list)
{
    k <- 1
    err_state_list <- vector("character")
    for (state in state_list)
    {
        data <- read.xls(file_location, header=F,sheet=state,na.strings=c("NA","#DIV/0!","","-"))
        if (dim(data)[2] != 4){
            err_state_list[k] <- state
            k <- k+1
        }
    }
    return(err_state_list)
}



debug_col(file_location=file_location, state_list=state_list)



# Print out incorrect state list
debug <- function(file_location, state_list) 
{
    k <- 1
    err_state_list <- vector("character")
    
    for (state in state_list)
    {
        data <- read.xls(file_location, header=F,sheet=state,na.strings=c("NA","#DIV/0!",""))
        
        lga_nm <- data[which(is.na(data[,2]) & is.na(data[,3]) & is.na(data[,4])),]
        lga_nm <- as.vector(as.matrix(lga_nm[,1]))
        lga_nm <- trim(str_replace(lga_nm, "LGA", ""))
        
        dat <- data[which(!is.na(data[,1]) & !is.na(data[,2]) & !is.na(data[,3]) & !is.na(data[,4])),]    
        if (dim(dat)[1]/length(lga_nm) != 19)
        {
            err_state_list[k] <- state
            k <- k+1
        }
    }
    return(err_state_list)
    
}



l1 <- debug(file_location = file_location, state_list = state_list)


load_2_test <- function(data)
{
    flag <- vector("integer")
    for (i in 1:dim(data)[1])
    {
        a <- as.vector(as.matrix(data[i,])) %in% ignore.case(c("Age Groups", "Total", "Sex", "Males", "Females", "Age", "Groups"))
        flag[i] <- sum(a)
        
    }
    data2 <- data[flag == 0,]
    #Strip lga names from transfromed data
    lga_nm <- lga_name(data2)
    #Pull out only population data
    dat <- data2[which(!is.na(data2[,1]) & !is.na(data2[,2]) & !is.na(data2[,3]) & !is.na(data2[,4])),]    
    
    output <- vector("list", length = 2)
    output[[1]] <- dat
    output[[2]] <- lga_nm
    return(output)
}


# test load2
debug2 <- function(file_location, state_list) 
{
    k <- 1
    err_state_list <- vector("character")
    
    for (state in state_list)
    {
        data <- read.xls(file_location, header=F,sheet=state,na.strings=c("NA","#DIV/0!","","-"))
        
        result <- load_2_test(data)
        if (dim(result[[1]])[1]/length(result[[2]]) != 19)
        {
            err_state_list[k] <- state
            k <- k+1
        }
    }
    return(err_state_list)
    
}

l2 <- debug2(file_location = file_location, state_list = l1)






lga_name <- function(data)
{
    lga_nm <- data[which(is.na(data[,2]) & is.na(data[,3]) & is.na(data[,4])),]
    lga_nm <- as.vector(as.matrix(lga_nm[,1]))
    lga_nm <- trim(str_replace(lga_nm, "LGA", ""))
    return(lga_nm)
}
               





# construct LGA name list for each row
lga_list <- function(lga_nm)
{
    lga <- vector("list")
    
    j <- 1
    for (i in lga_nm)
    {
        lga[[j]] <- rep(i,19)
        j <- j+1
    }
    
    lga_data <- as.matrix(lga[[1]])
    for (i in 2:length(lga))
    {
        lga_data <- rbind(lga_data,as.matrix(lga[[i]]))
    }
    
    return(lga_data)
}


#Load Data
load_1 <- function(data)
{
    lga_nm <- lga_name(data)
    
    dat <- data[which(!is.na(data[,1]) & !is.na(data[,2]) & !is.na(data[,3]) & !is.na(data[,4])),]    
    
    names(dat) <- c("Age Groups", "Total", "Male", "Female")
    
    #regular expression
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"41038","5 - 9")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"41008","5 - 9")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"'5 - 9","5 - 9")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"41196","10 - 14")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"'10 - 14","10 - 14")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"â???","-")
    
    lga_nm2 <- lga_list(lga_nm)
    dat <- cbind(dat, LGA.names = lga_nm2)
    
    return(dat)    
}

# Attempt for loading all data with very slow load_2
load_2 <- function(data)
{
    flag <- vector("integer")
    for (i in 1:dim(data)[1])
    {
        a <- as.vector(as.matrix(data[i,])) %in% ignore.case(c("Age Groups", "Total", "Sex", "Males", "Females", "Age", "Groups"))
        flag[i] <- sum(a)
        
    }
    data2 <- data[flag == 0,]
    #Strip lga names from transfromed data
    lga_nm <- lga_name(data2)
    
    #Pull out only population data
    dat <- data2[which(!is.na(data2[,1]) & !is.na(data2[,2]) & !is.na(data2[,3]) & !is.na(data2[,4])),]    
    names(dat) <- c("Age Groups", "Total", "Male", "Female")
    #Some regular expression here
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"41038","5 - 9")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"41008","5 - 9")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"'5 - 9","5 - 9")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"41196","10 - 14")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"'10 - 14","10 - 14")
    dat[,"Age Groups"] <- str_replace_all(dat[,"Age Groups"],"â???"","-")
    
    #Contruct lga list with rep of 19 for each lga
    lga_nm2 <- lga_list(lga_nm)
    dat <- cbind(dat, LGA.names = lga_nm2)
    
    return(dat)
}





word_process <- function(file_location, state_list) 
{
    k <- 1
    lga_state_list <- vector("list")
    
    for (state in state_list)
    {
        data <- read.xls(file_location, header=F,sheet=state,na.strings=c("NA","#DIV/0!",""))
        temp <- data[which(!is.na(data[,1]) & !is.na(data[,2]) & !is.na(data[,3]) & !is.na(data[,4])),]
        lga_tmp <- lga_name(data)
        if(dim(temp)[1]/length(lga_tmp) == 19){
            data2 <- load_1(data)
        }else{
            data <- read.xls(file_location, header=F,sheet=state,na.strings=c("NA","#DIV/0!","-",""))
            data2 <- load_2(data)
        }
    
        data2 <- cbind(data2, State.names = rep(state, dim(data2)[1]))
        lga_state_list[[k]] <- data2
        k <- k+1
    }
    return(lga_state_list)
}



# load and all data from 37 states and store in a list
final <- word_process(file_location=file_location, state_list=state_list)

# combine all data from 37 states
final_data <- final[[1]]
for (i in 2:length(final)){
    final_data <- rbind(final_data,final[[i]])
}
names(final_data)[1] <- "Age_Groups"
row.names(final_data) <- NULL

# regular expression in age_groups
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"41403","5 - 9")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"41157","5 - 9")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"â???~5 - 9","5 - 9")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"4 - 9","5 - 9")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"41561","10 - 14")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"41913","10 - 14")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"â???~10 - 14","10 - 14")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"0 4","0 - 4")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"5 9","5 - 9")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"1014","10 - 14")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"15 19","15 - 19")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"20 24","20 - 24")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"25 29","25 - 29")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"30 34","30 - 34")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"35 39","35 - 39")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"40 44","40 - 44")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"45 49","45 - 49")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"50 54","50 - 54")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"55 59","55 - 59")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"60 64","60 - 64")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"65 69","65 - 69")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"70 74","70 - 74")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"75 79","75 - 79")
final_data[,"Age_Groups"] <- str_replace_all(final_data[,"Age_Groups"],"80 84","80 - 84")


#################
# data checking #
#################

#Strip lga and state from final_data
lga_state <- unique(final_data[,c("LGA", "state")])
names(lga_state) <- c("lga", "state")
lga_state[,1] <- toupper(lga_state[,1])
lga_state <- lga_state[order(lga_state[,2],lga_state[,1]),]

# load reference sheet from external data for combining the lga_id
ref <- read.xls("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/Nigeria Master Codes_SP.xlsx") 
ref <- ref[,c("LGA_id", "state", "LGA")]

ref_lga_state <- ref[,2:3]
names(ref_lga_state) <- c("state", "lga")
ref_lga_state <- ref_lga_state[order(ref_lga_state[,1],ref_lga_state[,2]),]

# non-matching lga list 
internal <- lga_state[!as.vector(as.matrix(lga_state[,"lga"])) %in% as.vector(as.matrix(ref_lga_state[,"lga"])), "lga"]
external <- ref_lga_state[!as.vector(as.matrix(lga_state[,"lga"])) %in% as.vector(as.matrix(ref_lga_state[,"lga"])), "lga"]
external <- as.character(external)
# 38 state & lga pair did not match
test1 <-merge(lga_state, ref_lga_state)




#### find state & LGAs did not match
final_data_miss <- unique(final_data[final_data$LGA %in% internal,c("LGA", "state")])
final_data_miss <- final_data_miss[order(final_data_miss$state,final_data_miss$LGA),]

final_data_miss2 <-  merge(final_data,ref,all.x=T)
final_data_miss2 <- unique(final_data_miss2[is.na(final_data_miss2$LGA_id) == T,c("LGA", "state")])
final_data_miss2 <- final_data_miss2[order(final_data_miss2$state,final_data_miss2$LGA),]



ref_miss <- ref[ref$LGA_id %in% as.vector(as.matrix(temp1[,4])),]
ref_miss <- ref_miss[order(ref_miss$state,ref_miss$LGA),]


ref_miss2 <- merge(final_data,ref,all.y=T)
ref_miss2 <- unique(ref_miss2[is.na(ref_miss2$Total) == T,c("LGA", "state")])
ref_miss2 <- ref_miss2[order(ref_miss2$state,ref_miss2$LGA),]


names(final_data_miss2) <- c("lga_wrong", "state_wrong")
names(ref_miss2)  <- c("lga_candidate", "state_candidate")
err <- cbind(final_data_miss2,ref_miss2)



final_data[final_data$LGA == "EKITI",]
dim(a1 <- unique(final_data[,c("LGA", "state")]))

dim(a1[a1$state == "KADUNA",])
dim(ref[ref$state == "KADUNA",])


###########################################################################



names(final_data)[5:6] <- c("LGA", "state")
final_data[,"LGA"] <- toupper(final_data[,"LGA"])
final_data$state <- as.character(final_data$state)

#i <-1 
#final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],]

# Load correct LGA name list 
temp1 <- read.csv("C:/Users/Zaiming/Desktop/nigeria/pop/non_matching_LGAs_revise.csv")



for (i in 1: dim(temp1)[1]){
    final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],"LGA"] <- str_replace_all(final_data[final_data[,"LGA"] == temp1[i,"lga_wrong"] & final_data[,"state"] == temp1[i,"state_wrong"],"LGA"],as.character(temp1[i,2]), as.character(temp1[i,1]))
}


#final_data[final_data$LGA == final_data_miss2[2,1],]



#for (i in 1: dim(temp1)[1]){
#    final_data[,"LGA"] <- str_replace_all(final_data[,"LGA"],as.character(temp1[i,2]), as.character(temp1[i,1]))
#}


# Analysis PART
p1 <- final_data[final_data[,"Age_Groups"] == "5 - 9",c("Age_Groups", "Total", "LGA", "state")]
p2 <- final_data[final_data[,"Age_Groups"] == "10 - 14",c("Age_Groups", "Total", "LGA", "state")]
p3 <- final_data[final_data[,"Age_Groups"] == "10 - 14",c("Age_Groups", "Total", "LGA", "state")]
p4 <- final_data[final_data[,"Age_Groups"] == "15 - 19",c("Age_Groups", "Total", "LGA", "state")]
row.names(p1) <- NULL
row.names(p2) <- NULL
row.names(p3) <- NULL
row.names(p4) <- NULL

p1$Total <- round(as.numeric(p1$Total)*4/5)
p2$Total <- round(as.numeric(p2$Total)*2/5)
p3$Total <- round(as.numeric(p3$Total)*3/5)
p4$Total <- round(as.numeric(p4$Total)*3/5)

# Primary
primary <- rbind(p1,p2)
primary<- primary[order(primary$state,primary$LGA),]
row.names(primary) <- NULL
primary <- ddply(primary, .(state, LGA), summarise, total_enroll = sum(Total)) 

# Secondary
secondary <- rbind(p3,p4)
secondary<- secondary[order(secondary$state,secondary$LGA),]
row.names(secondary) <- NULL
secondary <- ddply(secondary, .(state, LGA), summarise, total_enroll = sum(Total)) 


primary2 <- merge(primary,ref)
secondary2 <- merge(secondary,ref)

# Junior Secondary
junior <- p3[order(p3$state,p3$LGA),]
junior <- merge(junior, ref)
write.csv(junior,"C:/Users/Zaiming/Desktop/nigeria/pop/Junior_Secondary.csv", row.names=F)




############################################
####Load External Primary & seconday data###
############################################
ext_prim <- read.csv("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/gross_enrollment_ratio_primary_education.csv")
ext_prim <- ext_prim[,c("count_enroll", "stlga")]

ext_sec <- read.csv("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/gross_enrollment_ratio_secondary_education.csv")
ext_sec <- ext_sec[,c("count_enroll", "stlga")]

ref_master <- read.csv("C:/Users/Zaiming/Dropbox/Nigeria 661 Baseline Data Cleaning (2)/external data/hnlss_LGA_Reference_Sheet.csv")
ref_master <- ref_master[,c("LGA_id", "stlga")]

ext_prim <- merge(ext_prim, ref_master)
ext_prim <- ddply(ext_prim, .(LGA_id), summarise, act_enroll = sum(count_enroll))
ext_sec <- merge(ext_sec, ref_master)
ext_sec <- ddply(ext_sec, .(LGA_id), summarise, act_enroll = sum(count_enroll))
#############################################

###########################################################
###combine enroll number and population at schooling age###
###########################################################

primary3 <- merge(primary2, ext_prim)
secondary3 <- merge(secondary2, ext_sec)

primary3$net_enroll <- primary3$act_enroll/primary3$total_enroll
secondary3$net_enroll <- secondary3$act_enroll/secondary3$total_enroll

############################################################

####Putout the result & error list
write.csv(primary3,"C:/Users/Zaiming/Desktop/nigeria/pop/primary_net_enroll.csv")
write.csv(secondary3,"C:/Users/Zaiming/Desktop/nigeria/pop/secondary_net_enroll.csv")
write.csv(final_data,"C:/Users/Zaiming/Desktop/nigeria/pop/total_state_lga_population.csv")


write.csv(err,"C:/Users/Zaiming/Desktop/nigeria/pop/non_matching_LGAs.csv")

names(ext_prim)





