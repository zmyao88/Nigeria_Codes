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
library(ROCR)
library(glmnet)
require(boot)

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
classify_result$Target <- 1
clean(classify_result, "Target", 
      which(Re_classify == 'tap'& water_scheme_type == 'outlet'), 0)

clean(classify_result, "Target", 
      which(Re_classify == 'handpump'& (lift_mechanism == 'manual_power' | lift_mechanism == 'hand_pump')), 0)

clean(classify_result, "Target", 
      which( (Re_classify %in% c('one_thousand_overhead' , 'ten_thousand_overhead') ) & 
                 water_source_type == 'borehole_tube_well' ), 0)

clean(classify_result, "Target", 
      which( (Re_classify == "rainwater" ) & 
                 water_source_type == 'rainwater_harvesting_scheme' ), 0)

clean(classify_result, "Target", 
      which( (Re_classify == 'unimproved') & 
                 water_scheme_type != "outlet" & !(water_source_type %in% c("borehole_tube_well", "rainwater_harvesting_scheme"))), 0)
detach()



# Water Survey data cleaning
water_survey <- subset(water_survey, select=-c(X, phonenumber, photo, gps, fh_photo_url, fh_photo_url_med, fh_photo_url_sml, unique_lga,
                                      water_scheme_type, water_source_type, lift_mechanism))

# Fix data type
water_survey$deviceid <- as.character(format(water_survey$deviceid, scientific = FALSE))
water_survey$start <- time_fixer(water_survey$start)
water_survey$end <- time_fixer(water_survey$end)

# Adding new features
water_survey$dur <- interval(water_survey$start, water_survey$end) / duration(60, "minutes")
water_survey$late_night <- (hour(water_survey$end) > 22 | hour(water_survey$end) <5)
water_survey$weekend <- (wday(water_survey$end) >= 6)

#counting the NAs
water_final <- merge(classify_result, water_survey, by="uuid")
water_final$na_count <- rowSums(is.na(water_final),1)


water_final$dur<- as.numeric(water_final$dur)



water_final$Target <- factor(water_final$Targe)


####Adding new features water_shceme_type == 'outlet', 
# water_final$precise_gps <- ifelse(water_final$X_gps_precision > 5, ">=6",
#                                 ifelse(water_final$X_gps_precision == 5, "5", 
#                                           ifelse(water_final$X_gps_precision == 4, "4",
#                                              ifelse(water_final$X_gps_precision == 3, "3",
#                                                     ifelse(water_final$X_gps_precision == 2, "2", "<=1")))))


water_final$precise_gps <- factor(ifelse(water_final$X_gps_precision > 5, ">5", "<=5"))
water_final$late_night <- factor(water_final$late_night)
water_final$weekend <- factor(water_final$weekend)
water_final$outlet <- factor(water_final$water_scheme_type == "outlet")
                                         
                                         
                                         




############################
# simple viz and tabulation#
############################                                         
err_rt <- ddply(water_final, .(deviceid), summarize, 
                err_rate = length(which(Target==0))/ length(Target),
                n_rec = length(Target),
                n_err = length(which(Target==0)),
                n_outlet = length(which(water_scheme_type == "outlet")))
plot(err_rt)



#jittered point showed someone did game the system by submit water_scheme_type == "outlet"
ggplot(water_final, aes(Target, water_scheme_type == "outlet")) + geom_jitter(alpha=0.5)


#Duraiton of the survey seemed indentical across groups, 
ggplot(water_final, aes(factor(Target), dur)) + geom_boxplot() + ylim(0,250)


ggplot(water_final, aes(dur)) + geom_histogram() + facet_wrap(~Target) 
ggplot(water_final, aes(dur)) + geom_histogram() + facet_wrap(~Target) +  xlim(0, 5000)
ggplot(water_final, aes(dur)) + geom_histogram() + facet_wrap(~Target) +  xlim(0, 500)
ggplot(water_final, aes(dur)) + geom_histogram() + facet_wrap(~Target) +  xlim(0, 50)

ggplot(water_final, aes(dur)) + geom_density() + facet_wrap(~Target) +  xlim(0, 5000)
ggplot(water_final, aes(dur)) + geom_density() + facet_wrap(~Target) +  xlim(0, 1200)
ggplot(water_final, aes(dur)) + geom_density() + facet_wrap(~Target) +  xlim(0, 50)
ggplot(water_final, aes(dur, fill=factor(Target))) + geom_bar(position="identity", alpha=0.5) +  xlim(0, 250)
# + facet_wrap(~Target) +  xlim(0, 250)



er <- water_final[water_final$dur >= 250,]
ggplot(er, aes(dur, fill=factor(Target))) + geom_histogram(alpha=0.5, position="identity")
table(er$Target)


#Checking gps precision 
plot(X_gps_precision~ Target, data=water_final )
ggplot(water_final, aes(factor(Target), X_gps_precision)) + geom_jitter(alpha=0.5) + ylim(0,100)



#NA_counts vs outlet
ggplot(water_final, aes(water_scheme_type == "outlet", na_count, color=factor(Target))) + geom_jitter(alpha =0.3)

#discrestize duration 

gps <- ddply(water_final, .(Target), summarize, nrec = length(X_gps_precision),
                                                gps6 = length(which(X_gps_precision > 5)),
                                                gps5 = length(which(X_gps_precision == 5)),
                                                gps4 = length(which(X_gps_precision == 4)),
                                                gps3 = length(which(X_gps_precision == 3)),
                                                gps2 = length(which(X_gps_precision == 2)),
                                                gps1 = length(which(X_gps_precision == 1)),
                                                gps0 = length(which(X_gps_precision <= 0)))

gps <- water_final
gps$precise <- ifelse(gps$X_gps_precision > 5, ">=6",
       ifelse(gps$X_gps_precision == 5, "5", 
              ifelse(gps$X_gps_precision == 4, "4",
                    ifelse(gps$X_gps_precision == 3, "3",
                           ifelse(gps$X_gps_precision == 2, "2", "<=1")))))

ggplot(gps, aes(precise, ..count.., fill=factor(Target))) + geom_bar(position='identity', alpha=0.5)


View(gps[,3:9]/gps[,2]*100)

names(water_final)




#fit logistic regression
train_dat <- subset(water_final, select=c(Target, dur, late_night, weekend, na_count, precise_gps,outlet))
# train_dat$deviceid <- factor(train_dat$deviceid)
train_dat <- na.omit(train_dat)


sample_vec <- sample(1:19299)
valid_dat <- train_dat[sample_vec[1:5789],]
train_dat <- train_dat[sample_vec[5790:19299],]

#train logistic
mod2 <- glm(Target ~ ., data=train_dat, family="binomial")
summary(mod2)
mod2

pred_val <- predict(mod2, newdata=valid_dat)
pred_05 <- pred_val > 0.5

table(pred_05,  valid_dat$Target)/ 5789

#ROc curve for better evaluation
pred <- prediction(pred_val, valid_dat$Target)
perf <- performance(pred, "tpr", "fpr")
auc_perf <- performance(pred, "auc")
plot(perf)

##cv
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

cv.err <- cv.glm(train_dat, mod2, cost, K=10)$delta


## glmnet logistic
#some data preparation
train_dat <- subset(water_final, select=c(Target, dur, late_night, weekend, na_count, precise_gps,outlet))
train_dat <- na.omit(train_dat)
y_trb <- water_final$Target
x_tr <- subset(water_final, select=c(dur, late_night, weekend, na_count, precise_gps,outlet))

# x_tr$deviceid <- factor(x_tr$deviceid)

xplot(x_tr)
model.matrix()

mod3 <- glmnet(y=y_trb, x=x_tr, family='binomial', standardize=T,alpha=0.25)









#fit random forrest
dat1 <- subset(water_final, select=c("Target","dur", "late_night", "weekend", 'na_count', 'precise_gps', 'outlet', 'deviceid'))
# dat1 <- dat1[!is.na(dat1$outlet),]
dat1 <- na.omit(dat1)
colwise(class)(dat1)


mod1 <-randomForest(factor(Target)~dur + late_night + weekend + na_count + precise_gps + outlet, data=dat1,
                    importance=TRUE,proximity=TRUE)


####SVM
mod3 <- svm(Target~dur + late_night + weekend + na_count + precise_gps + outlet, data=dat1)
summary(mod3)
table(predict(mod3), dat1$Target)/19299



tab <- table(water_final$Target, water_final$late_night)
addmargins(tab)
prop.table(tab,margin=1)


prop.table(table(water_final$weekend, water_final$Target), margin=1)

t1 <- ddply(water_final, .(late_night, weekend, Target), nrow)
t1$pct <- t1$V1 / nrow(water_final)
t1 <- ddply(t1, .(late_night, weekend), transform, group_pct = V1/sum(V1))



ggplot(water_final, aes(factor(Target), dur)) + geom_boxplot()

water_final$Target


names(water_final)



# write.csv(water_final, 'water_reclassify_master.csv', row.names=F)

?predict.glm
mod1 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat1,
                    importance=TRUE,proximity=TRUE)




