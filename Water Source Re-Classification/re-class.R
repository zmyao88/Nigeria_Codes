setwd("C:/Users/zmyao/Dropbox/Nigeria/ReClassification")
re_classify <- read.csv("reclassify_final_148.csv", stringsAsFactors=F)
water_classify <- read.csv("../Nigeria 661 Baseline Data Cleaning/in_process_data/merged/Water_661_Merged.csv", stringsAsFactors=F)
# "Water_661_NMIS_Facility.csv" $ water_point_type
# water_survey <- read.csv("../Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Water_661_999Cleaned_Reclassified.csv", stringsAsFactors=F)
water_survey <- water_classify


require(plyr)
require(stringr)
require(glmnet)
require(randomForest)
require(lubridate)
require(data.table)
require(ggplot2)

time_fixer <- function(var)
{
    result <- ymd_hms(str_extract(var, '^[^+Z]*(T| )[^+Z-]*'))    
    return(result)
}

clean <- function(dt, cols, rows, value) {
    if (any(rows)) {
        set(dt, rows, cols, value) }
}


####data manipulation for re-classification data
# Re-classify
# re_classify <- subset(re_classify, ! Classification %in% c("No Photo", "no_photo", "No_photo"))
table(re_classify$Classification)
re_classify <- rename(re_classify, c("Classification" = "Re_classify"))
re_classify <- subset(re_classify, select=c("uuid", "Re_classify"))               

#water_classify
water_classify <- subset(water_classify, select=c("uuid", "water_source_type", "water_scheme_type", "lift_mechanism"))

#Combine re-classify & classify results
#
classify_result <- merge(re_classify, water_classify)

table(classify_result$lift_mechanism)
table(classify_result$water_source_type)
table(classify_result$water_scheme_type)
table(classify_result$Re_classify)



#Cleaning up the miss-spelling 
clean(classify_result, "Re_classify", 
      which(classify_result$Re_classify %in% c('unimproved' , 'unimproved ') ), "unimproved")
clean(classify_result, "Re_classify", 
      which(classify_result$Re_classify %in% c('No Photo' , 'no_photo', "No_photo") ), "no_photo")
clean(classify_result, "Re_classify", 
      which(classify_result$Re_classify %in% c('ten_thousand_ovehead' , 'ten_thousand_overhead') ), "ten_thousand_overhead")



# Creating Target boolean column for identification of errors
attach(classify_result)
classify_result$Target <- 0
clean(classify_result, "Target", 
      which(Re_classify == 'tap'& water_scheme_type == 'outlet'), 1)

clean(classify_result, "Target", 
      which(Re_classify == 'handpump'& (lift_mechanism == 'manual_power' | lift_mechanism == 'hand_pump')), 1)

clean(classify_result, "Target", 
      which( (Re_classify %in% c('one_thousand_overhead' , 'ten_thousand_overhead') ) & 
                 water_source_type == 'borehole_tube_well' ), 1)

clean(classify_result, "Target", 
      which( (Re_classify == "rainwater" ) & 
                 water_source_type == 'rainwater_harvesting_scheme' ), 1)

clean(classify_result, "Target", 
      which( (Re_classify == 'unimproved') & 
                 water_scheme_type != "outlet" & !(water_source_type %in% c("borehole_tube_well", "rainwater_harvesting_scheme"))), 1)
detach()



# Water Survey data cleaning
water_survey <- subset(water_survey, select=-c(X, phonenumber, photo, gps, fh_photo_url, fh_photo_url_med, fh_photo_url_sml, unique_lga,
                                      water_scheme_type, water_source_type, lift_mechanism))

# Fix data type
water_survey$deviceid <- as.character(format(water_survey$deviceid, scientific = FALSE))
water_survey$start <- time_fixer(water_survey$start)
water_survey$end <- time_fixer(water_survey$end)

# Adding new features
water_survey$dur <- duration(interval(water_survey$start, water_survey$end))
water_survey$late_night <- (hour(water_survey$end) > 22 | hour(water_survey$end) <5)
water_survey$weekend <- (wday(water_survey$end) >= 6)

#counting the NAs
water_final <- merge(classify_result, water_survey, by="uuid")
water_final$na_count <- rowSums(is.na(water_final),1)


water_final$dur<- as.numeric(water_final$dur)



# simple viz and tabulation
ggplot(water_final, aes(factor(Target), dur)) + geom_boxplot()



tab <- table(water_final$Target, water_final$late_night)
addmargins(tab)
prop.table(tab,margin=1)


prop.table(table(water_final$weekend, water_final$Target), margin=1)

t1 <- ddply(water_final, .(late_night, weekend, Target), nrow)
t1$pct <- t1$V1 / nrow(water_final)
t1 <- ddply(t1, .(late_night, weekend), transform, group_pct = V1/sum(V1))



ggplot(water_final, aes(factor(Target), dur)) + geom_boxplot()

water_final$Target

ggplot(water_final, aes(dur, fill=factor(Target))) + geom_histogram()
names(water_final)

mod2 <- glm(Target ~ dur + late_night + weekend + na_count, data=water_final)
summary(mod2)
pred_val <- predict(mod2, type="response") > 0.5

table(pred_val, water_final$Target)



?predict.glm
mod1 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat1,
                    importance=TRUE,proximity=TRUE)




