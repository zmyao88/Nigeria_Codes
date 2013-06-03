l <- letters
set.seed(1)
x1 <- l[sample(1:26, 81877, replace=T)]
x2 <- l[sample(1:26, 81877, replace=T)]
x3 <- l[sample(1:26, 81877, replace=T)]
x4 <- l[sample(1:26, 81877, replace=T)]
x5 <- l[sample(1:26, 81877, replace=T)]

facility_list$random_id <- paste0(x1, x2, x3, x4, x5)

t2 <- arrange(facility_list, lga_id, end)
t2 <- ddply(t2, .(t2[,"lga_id"]), transform, tmp2 = paste0("A", as.character(1:length(tmp))))



id_generate <- function(df, lga_col = "lga_id", submit_end_col = "end", header = "F")
{
    
    l <- letters
    set.seed(1)
    
    x1 <- l[sample(1:26, dim(df)[1], replace=T)]
    x2 <- l[sample(1:26, dim(df)[1], replace=T)]
    x3 <- l[sample(1:26, dim(df)[1], replace=T)]
    x4 <- l[sample(1:26, dim(df)[1], replace=T)]
    x5 <- l[sample(1:26, dim(df)[1], replace=T)]
    
    df <- arrange(df, df[, lga_col], df[, submit_end_col])
    df <- ddply(df, .(df[,lga_col]), transform, 
                seq_id = paste0(header, as.character(1:length(df[,lga_col]))))
    df$random_id <- paste0(x1, x2, x3, x4, x5)
    
    return(df)
}    
    



t <- ddply(facility_list, .(lga_id), summarise, 
           unique_short_id = length(unique(tmp)), 
           n_fac = length(tmp))

which(t$unique_short_id != t$n_fac)
