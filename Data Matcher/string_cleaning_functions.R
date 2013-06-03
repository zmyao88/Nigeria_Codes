# For EDUCATION facility list run the next 4 lines first
# I generally combined ward_num & ward_name 
# You might want to replace "facility_list" with the name in your workspace

index <- which(is.na(facility_list$ward_num) & !is.na(facility_list$ward_name))
facility_list$ward_num[index] <- facility_list$ward_name[index]
rm(index)
facility_list$ward_name <- NULL



# this is for HEALTH facility_list Only
# plug in df = 'facility_list' ward_col = 'ward' comunity_col = 'cominity' and 
# it will handle the cleaning process for you
ward_comm_fix_health <- function(df, ward_col, comunity_col)
{
#     ward_col <- "ward"
#     comunity_col <- "community"
#     df <- facility_list
    
    # Take out "ward"
    df[,ward_col] <- str_replace(df[,ward_col], ignore.case("ward"), "")
    # find row.name for those com == NA & ward contain (",", "/")
    idx <- which(is.na(df[, comunity_col]) & str_detect(df[, ward_col], '[/,]') )
    # replace comm with string after "/"
    df[idx, comunity_col] <- str_trim(str_replace(df[idx,ward_col], '[a-zA-Z0-9 \')]+[/,]', ""))
    # replace war with string before "/"
    df[idx, ward_col] <- str_trim(str_replace(str_extract(df[idx, ward_col], '[a-zA-Z0-9 \')]+[/,]'), "[/,]$", ""))
    # trim off leading & tailing blanks
    df[, ward_col] <- str_trim(df[, ward_col])
    # trim off "0" in front of 01,02 & etc
    df[which(str_detect(df[, ward_col], '^[0-9]+$')), ward_col] <- str_replace(df[which(str_detect(df[,ward_col], '^[0-9]+$')), ward_col], "^0+", "")
    # replace consecutive blanks with only one blank
    df[,ward_col] <- gsub('  +', " ", df[,ward_col], ignore.case=T)
    df[,comunity_col] <- gsub('  +', " ", df[,comunity_col], ignore.case=T)
    return(df)
} 



# this is for facility_list & Baseline in EDUCATION & for HEALTH Baseline
# plug in df = 'facility_list' ward_col = 'ward' comunity_col = 'cominity' and 
# it will handle the cleaning process for you
ward_comm_fix_health <- function(df, ward_col, comunity_col)
{
    #     ward_col <- "ward"
    #     comunity_col <- "community"
    #     df <- facility_list
    
    # Take out "ward"
    df[,ward_col] <- str_replace(df[,ward_col], ignore.case("ward"), "")
    # trim off leading & tailing blanks
    df[, ward_col] <- str_trim(df[, ward_col])
    df[, comunity_col] <- str_trim(df[, comunity_col])
    # trim off "0" in front of 01,02 & etc
    df[which(str_detect(df[, ward_col], '^[0-9]+$')), ward_col] <- str_replace(df[which(str_detect(df[,ward_col], '^[0-9]+$')), ward_col], "^0+", "")
    # replace consecutive blanks with only one blank
    df[,ward_col] <- gsub('  +', " ", df[,ward_col], ignore.case=T)
    df[,comunity_col] <- gsub('  +', " ", df[,comunity_col], ignore.case=T)
    return(df)
} 






# this is for BOTH facility_list & health Baseline in HEALTH
# plug in df = 'facility_list' ward_col = 'facility_name' and 
# it will handle the cleaning process for you
facility_name_fix_health <- function(df, facility_name_col)
{
#     df <- facility_list
#     facility_name_col <- "facility_name"
    
    df[,facility_name_col] <- sub('pry.+health.|PRI.+HEALTH',  "Primary Health ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('center', "Centre", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('B(\\.| )H(\\.| )C\\.|BHC', "Basic Health Centre", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('P.H.C.+(clinic|centre)|PHC.+(clinic|centre)', "PHCC", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('p h c c', "PHCC ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('P(\\.| )H(\\.| )C\\.|pri.+Health.centre', "PHC", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('(H/(P|post)|health post|HP)', "Health Post", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('hosp\\.', "Hospital", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('/mat(\\.| |)', "/Maternity ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('hosp/', "Hospital/ ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('gen(\\.| )', "General ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('comp(\\.| )', "Comprehensive ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('h/c |h/c$', "HC ", df[, facility_name_col], ignore.case=T)
    return(df)
}


# this is for BOTH facility_list & health Baseline in EDUCATION
# plug in df = 'facility_list' ward_col = 'facility_name' and 
# it will handle the cleaning process for you
facility_name_fix_education <- function(df, school_name_col)
{
#     df <- facility_list
#     school_name_col <- "school_name"
    
    df[, school_name_col] <- gsub('comm(\\.| )|comm$',  "Community ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(sch(\\.| )|sch$)',  "School ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(sec(\\.| )|sec$)',  "Secondary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('snr(\\.| )|snr$|snr)',  "Senior ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(nur/(pri|pry)(.|$))|N/P(\\.| |$)',  "Nursery/Primary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(pri|pry|prim)(\\.| )',  "Primary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('jnr',  "Junior ", df[, school_name_col], ignore.case=T)
    return(df)
}




#Generate random character id

id_generate <- function(df)
{
    
    l <- letters
    set.seed(1)
    x1 <- l[sample(1:26, dim(df)[1], replace=T)]
    x2 <- l[sample(1:26, dim(df)[1], replace=T)]
    x3 <- l[sample(1:26, dim(df)[1], replace=T)]
    x4 <- l[sample(1:26, dim(df)[1], replace=T)]
    x5 <- l[sample(1:26, dim(df)[1], replace=T)]
     
    df$random_id <- paste0(x1, x2, x3, x4, x5)
    return(df)
}    

facility_list <- id_generate(facility_list)

#This is for tesing if the random_id is unique within each lga
#If output is "integer(0)" then we're good
t <- ddply(facility_list, .(lga_id), summarise, 
           unique_short_id = length(unique(random_id)), 
           n_fac = length(random_id))

which(t$unique_short_id != t$n_fac)
####

#####################################
###### Create Sequential IDs ########
#####################################
# order by lga_id  and submittion time
facility_list <- arrange(facility_list, lga_id, end)

# Create serial from 1 to number of records in that lga
facility_list <- ddply(facility_list, .(lga_id), transform, 
                       seq_id = 1:length(lga_id))

# the following 4 lines are for adding leading "0"s 
idx <- which(sapply(facility_list$seq_id, nchar) == 1)
facility_list$seq_id[idx] <- paste0("00", facility_list$seq_id[idx])
idx <- which(sapply(facility_list$seq_id, nchar) == 2)
facility_list$seq_id[idx] <- paste0("0", facility_list$seq_id[idx])
    
# Adding the leading character, as Roger suggested
# Feel fee to change "F" into waterver alphabet you want
facility_list$seq_id <- paste0("F", df$seq_id)

