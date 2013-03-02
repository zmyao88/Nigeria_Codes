setwd("C:/Users/zmyao/Dropbox/nigeria/Need asse & cov")
list.files(path=".")
require(plyr)
require(gdata)
require(stringr)
require(ggplot2)
require(reshape2)
require(data.table)


edu_coverage_breakdown_case1 <- read.csv("edu_coverage_breakdown_case1.csv")
edu_coverage_breakdown_case2 <- read.csv("edu_coverage_breakdown_case2.csv")
edu_coverage_total_case1 <- read.csv("edu_coverage_total_case1.csv")
edu_coverage_total_case2 <- read.csv("edu_coverage_total_case2.csv")
health_coverage_breakdown <-  read.csv("health_coverage_breakdown_feb18.csv")
health_coverage_total <- read.csv("health_coverage_total_feb18.csv")

count_missing <- function(df, var1="need_assessment_total", var2="baseLine_total"){
    require(plyr)
    #df <- edu_coverage_total_case1
    #var1 <- "need_assessment_total"
    #var2 <- "baseLine_total"
    df$delta <- df[,var1] - df[,var2]
    df <- df[!is.na(df[,"delta"]),]
    df$group <- NA
    df[df[,"delta"] < 0, "group"] <- "< 0"
    df[df[,"delta"] >=0 & df[,"delta"] < 10, "group"] <- ">= 0"
    df[df[,"delta"] >=10 & df[,"delta"] < 20, "group"] <- ">= 10"
    df[df[,"delta"] >=20 & df[,"delta"] < 30, "group"] <- ">= 20"
    df[df[,"delta"] >=30 & df[,"delta"] < 40, "group"] <- ">= 30"
    df[df[,"delta"] >=40 & df[,"delta"] < 50, "group"] <- ">= 40"
    df[df[,"delta"] >=50 , "group"] <- ">= 50"
    
    df2 <- ddply(df, .(group), nrow)
    names(df2)[2] <- "count"
    df2$x <- seq(-20, by=20, length.out=7)
    df2$y <- seq(70, by=-10, length.out=7)
    return(df2)    
}




ggplot(edu_coverage_total_case1, aes(x=(need_assessment_total - baseLine_total ))) + 
    geom_histogram(aes(y=..count..) , breaks = seq(-100, 150, by = 2)) + 
    scale_x_continuous(breaks = seq(-100,150, by = 10)) 

ggplot(edu_coverage_total_case2, aes(x=(need_assessment_total - baseLine_total))) + 
    geom_histogram(aes(y= ..count.. ) , binwidth=10) + 
    geom_density(fill=NA, aes( y= ..count..), trim=T) + 
    scale_x_continuous(breaks = seq(-500,550, by = 25), limits = c(-500,550))




#bell shaped net missing with label for education
edu_total1<- count_missing(edu_coverage_total_case1)
p1 <- ggplot(edu_coverage_total_case1, aes(x=(need_assessment_total - baseLine_total))) +
    geom_histogram(aes(y=..count..), breaks = seq(-100, 550, by = 10)) + 
      geom_freqpoly(binwidth=20, stat = "bin", color= "blue", linetype=1, breaks = seq(-100, 550, by = 10), size=0.9, alpha=0.7) + 
        scale_x_continuous(breaks = seq(-100,550, by = 50)) + 
    geom_text(data=edu_total1, aes(x= x, y= y, label= paste(group, count, sep=": "), fontface=5), size=5, color= "red" )+
    labs(list (title = "Education Net Missing \n Totoal records = 693 & NAs = 99", x = "Net Missing (Need Assessment - Baseline)"))

#bell shaped net missing with label for Health
health_total1<- count_missing(health_coverage_total,var1="Need_Assessment_total", var2="base_line_total")
p2 <- ggplot(health_coverage_total, aes(x=(Need_Assessment_total - base_line_total))) +
    geom_histogram(aes(y=..count..), breaks = seq(-100, 550, by = 10)) + 
    geom_freqpoly(binwidth=20, stat = "bin", color= "blue", linetype=1, breaks = seq(-100, 550, by = 10), size=0.9, alpha=0.7) + 
    scale_x_continuous(breaks = seq(-100,550, by = 50)) + 
    geom_text(data=health_total1, aes(x= x, y= y, label= paste(group, count, sep=": "), fontface=5), size=5, color= "red" ) + 
    labs(list (title = "Health Net Missing \n Totoal records = 668 & NAs = 0", x = "Net Missing (Need Assessment - Baseline)"))

pdf(file="../../Needs Assessment/histogram_output/total_net_missing_v1.pdf")
p1
p2
dev.off()
#education total count by school managed 
test <- melt(edu_coverage_breakdown_case1, id.vars=c("X_lga_id", "school_managed"), measure.vars=c("need_assessment_total", "baseLine_total"))
p1 <- ggplot(test, aes(x=value, fill=variable)) +
    geom_histogram(aes(y=..count..), binwidth=5, alpha = 0.5, position="identity") + 
    #geom_freqpoly(binwidth=20, stat = "bin", color= "blue", linetype=1, breaks = seq(-100, 550, by = 10), size=0.9, alpha=0.7) + 
    geom_density(fill=NA, aes(y= ..count..)) + 
    geom_vline(xintercept = seq(0,50,by=10), colour="black", linetype = 2) + 
    scale_x_continuous(breaks = seq(-50,550, by = 25), limits = c(-50,550))+    
    facet_wrap(~ school_managed)+
    labs(list (title = "Education Total Counts by school type", x = "Actual total number of schools"))

#education net missing by shcool managed
p2 <- ggplot(edu_coverage_breakdown_case1, aes(x=(need_assessment_total - baseLine_total), fill=school_managed)) +
    geom_histogram(aes(y=..count..), binwidth = 10, alpha = 0.5, position="identity") + 
    geom_density(fill=NA, aes(y= ..count..)) +
    #geom_freqpoly(binwidth=20, stat = "bin", color= "blue", linetype=1, breaks = seq(-100, 550, by = 10), size=0.9, alpha=0.7) + 
    geom_vline(xintercept = seq(0,50,by=10), colour="black", linetype = 2) +
    scale_x_continuous(breaks = seq(-100,550, by = 25), limits = c(-100,550)) + 
    labs(list (title = "Education Net missing break down by school type", x = "Net Missing (Need Assessment - Baseline)"))

#Health net missing by shcool managed
p3 <- ggplot(health_coverage_breakdown, aes(x=(Need_Assessment_count - base_line_faciliti_count))) +
    geom_histogram(aes(y=..count..), binwidth = 10, alpha = 0.5, position="identity") + 
    geom_density(fill=NA, aes(y= ..count..)) +
    #geom_freqpoly(binwidth=20, stat = "bin", color= "blue", linetype=1, breaks = seq(-100, 550, by = 10), size=0.9, alpha=0.7) + 
    geom_vline(xintercept = seq(0,50,by=10), colour="black", linetype = 2) +
    scale_x_continuous(breaks = seq(-100,120, by = 10), limits = c(-100,120))+
    facet_wrap(~facili_type) +
    labs(list (title = "Health Total Counts by school type", x = "Actual total number of schools"))

#health total count by facilitie type 
test <- melt(health_coverage_breakdown, id.vars=c("lga_id", "facili_type"), measure.vars=c("Need_Assessment_count", "base_line_faciliti_count"))
p4 <- ggplot(test, aes(x=value, fill=variable)) +
    geom_histogram(aes(y=..count..), binwidth=5, alpha = 0.5, position="identity") + 
    #geom_freqpoly(binwidth=20, stat = "bin", color= "blue", linetype=1, breaks = seq(-100, 550, by = 10), size=0.9, alpha=0.7) + 
    geom_density(fill=NA, aes(y= ..count..)) + 
    geom_vline(xintercept = seq(0,50,by=10), colour="black", linetype = 2) + 
    scale_x_continuous(breaks = seq(-50,100, by = 10), limits = c(-50,100))+    
    facet_wrap(~ facili_type) +
    labs(list (title = "Health Net missing break down by school type", x = "Net Missing (Need Assessment - Baseline)"))

pdf(file="../../Needs Assessment/histogram_output/break_down_net_missing_v1.pdf")
p1
p3
dev.off()

pdf(file="../../Needs Assessment/histogram_output/total_number_count_by_faciliti_type_v1.pdf")
p2
p4
dev.off()




source(file="C:/Users/zmyao/Documents/GitHub/formhub.R/R/formhub.R")
edu <- formhubRead("Feb_1_education_1.csv", "education.json",na.strings="")@data
edu2 <- formhubRead("./test/education_2_2013_02_25_17_12_26.csv", "education_2.json",na.strings="")@data