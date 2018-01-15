#****************************************************************************************************************#
#*                    CODE TO RUN AND BUILD AND MIXED MARKETING MODEL                                           *#
#*  OWNER: ANUBHAV DIKSHIT                                                                                      *#
#*  EDITED: ANUBAV DIKSHIT                                                                                      *# 
#****************************************************************************************************************#

options(scipen=999) # disable scientific notation
rm(list=ls()) # clear workspace
cat("\014") #clear screen
gc()

setwd("C:/Users/anubh/OneDrive/Documents/GitHub/Experiments_with_R/Mixed_Marketing_Model")

#****************************************************************************************************************#
#*                                      DEFINING THE LIBRARIES                                                  *#
#****************************************************************************************************************#

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
library("pacman")

pacman::p_load(data.table, dplyr, stringr, reshape2, lubridate, geosphere, caret, car, TTR)

#****************************************************************************************************************#
#*                                   LOADING INPUT FILES                                                        *#
#****************************************************************************************************************#

ads = fread("Input/final_ads.csv", header = TRUE)

#****************************************************************************************************************#
#*                                   FIXING DATES IN THE DATASET                                                *#
#****************************************************************************************************************#


ads$date <- as.Date(ads$date, format = "%d-%m-%Y")
ads$year_week <- paste(year(ads$date), week(all_promo_sends$date), sep = " - ")


ads_2016 = ads[ads$year_range == 2016 | ads$year_range == 2017, c("kiosk_id",
                                                                  "kiosk_rent_week",
                                                                  "weekly_units_per_kiosk",
                                                                  "holidays",
                                                                  "sem_seo_display_spend_per_kiosk_per_week",
                                                                  "week_rental_seasonality",
                                                                  "social_spend_per_kiosk_per_week",
                                                                  "content_strength",
                                                                  "inventory_strength",
                                                                  "kiosk_indoor_outdoor",
                                                                  "all_kiosk_in_half_and_five_mile_cumulative",
                                                                  "mean_household_income",
                                                                  "lag_weekly_units_per_kiosk",
                                                                  "kiosk_disc_capacity",
                                                                  "inventory_skew",
                                                                  "KioskAge_days",
                                                                  "incart_qualifying_redem_volume_per_kiosk_per_week",
                                                                  "out_redem_impressions_per_kiosk_per_week",
                                                                  "pr_spend_per_kiosk_per_week",
                                                                  "incart_dummy",
                                                                  "out_dummy",
                                                                  "sends_125_per_week",
                                                                  "quarters",
                                                                  "year_range",
                                                                  "Email_promo_bucket_125",  
                                                                  "Email_promo_bucket_150",  
                                                                  "Email_promo_bucket_25",     
                                                                  "Email_promo_bucket_50",   
                                                                  "Email_promo_bucket_75",   
                                                                  "Email_promo_bucket_DL",   
                                                                  "Email_promo_bucket_FREE", 
                                                                  "Email_promo_bucket_GAME", 
                                                                  "Email_promo_bucket_NoOFF",
                                                                  "Email_promo_bucket_R1G1", 
                                                                  "Push_promo_bucket_125",   
                                                                  "Push_promo_bucket_50",    
                                                                  "Push_promo_bucket_75",    
                                                                  "Push_promo_bucket_DL",    
                                                                  "Push_promo_bucket_FREE",  
                                                                  "Push_promo_bucket_GAME",  
                                                                  "Push_promo_bucket_NoOFF", 
                                                                  "Push_promo_bucket_R1G1",  
                                                                  "SMS_promo_bucket_125",    
                                                                  "SMS_promo_bucket_150",    
                                                                  "SMS_promo_bucket_50",     
                                                                  "SMS_promo_bucket_75",     
                                                                  "SMS_promo_bucket_DL",     
                                                                  "SMS_promo_bucket_FREE",   
                                                                  "SMS_promo_bucket_GAME",   
                                                                  "SMS_promo_bucket_NoOFF",  
                                                                  "SMS_promo_bucket_R1G1",
                                                                  "holidays", 
                                                                  "kiosk_indoor_outdoor", 
                                                                  "kiosk_disc_capacity", 
                                                                  "incart_dummy", 
                                                                  "out_dummy", 
                                                                  "week", 
                                                                  "quarters", 
                                                                  "year_range")]

ads_2016$email_125_sends_lessthan6mil = ifelse(ads_2016$sends_125_per_week <= 6000000, ads_2016$sends_125_per_week, 0)
ads_2016$email_125_sends_between6and12mil = ifelse(ads_2016$sends_125_per_week > 6000000 & ads_2016$sends_125_per_week <= 12000000, ads_2016$sends_125_per_week, 0)
ads_2016$email_125_sends_greaterthan12mil = ifelse(ads_2016$sends_125_per_week > 12000000, ads_2016$sends_125_per_week, 0)

# creating the seasonal * term
ads_2016$email_125_sends_lessthan6mil_seasonal = ads_2016$email_125_sends_lessthan6mil * ads_2016$week_rental_seasonality
ads_2016$email_125_sends_between6and12mil_seasonal = ads_2016$email_125_sends_between6and12mil * ads_2016$week_rental_seasonality
ads_2016$email_125_sends_greaterthan12mil_seasonal = ads_2016$email_125_sends_greaterthan12mil * ads_2016$week_rental_seasonality

ads_2016$Push_promo_bucket_R1G1_seasonal = ads_2016$Push_promo_bucket_R1G1 * ads_2016$week_rental_seasonality
ads_2016$Email_promo_bucket_R1G1_seasonal = ads_2016$Email_promo_bucket_R1G1 * ads_2016$week_rental_seasonality
ads_2016$SMS_promo_bucket_R1G1_seasonal = ads_2016$SMS_promo_bucket_R1G1 * ads_2016$week_rental_seasonality

ads_2016$email_125_sends_lessthan6mil_seasonal = ads_2016$email_125_sends_lessthan6mil * ads_2016$week_rental_seasonality
ads_2016$email_125_sends_between6and12mil_seasonal = ads_2016$email_125_sends_between6and12mil * ads_2016$week_rental_seasonality
ads_2016$email_125_sends_greaterthan12mil_seasonal = ads_2016$email_125_sends_greaterthan12mil * ads_2016$week_rental_seasonality

ads_2016$Email_promo_bucket_lessthandl = rowSums(ads_2016[, c("Email_promo_bucket_25", "Email_promo_bucket_50", "Email_promo_bucket_75")])

# merging retail redemptions
ads_2016 = merge(ads_2016, retail_promo_redemptions_2016, by.x = c("kiosk_id", "week"), by.y = c("kioskclient_id", "week_num"), all.x = T)


temp <- ads_2016[,c("Push_promo_bucket_R1G1", "Email_promo_bucket_R1G1", "SMS_promo_bucket_R1G1", "retail_promo_redemptions", "year_range")]
temp <- temp[temp$year_range == "2016",]

temp <- na.omit(temp)

correlation <- cor(temp[,c("Push_promo_bucket_R1G1", "Email_promo_bucket_R1G1", "SMS_promo_bucket_R1G1", "retail_promo_redemptions")])

# creating log variables
ads_2016_log = ads_2016[, c("weekly_units_per_kiosk",
                            "sem_seo_display_spend_per_kiosk_per_week",
                            "week_rental_seasonality",
                            "social_spend_per_kiosk_per_week",
                            "content_strength",
                            "inventory_strength",
                            "all_kiosk_in_half_and_five_mile_cumulative",
                            "mean_household_income",
                            "lag_weekly_units_per_kiosk",
                            "inventory_skew",
                            "KioskAge_days",
                            "incart_qualifying_redem_volume_per_kiosk_per_week",
                            "out_redem_impressions_per_kiosk_per_week",
                            "pr_spend_per_kiosk_per_week",
                            "email_125_sends_lessthan6mil",
                            "email_125_sends_between6and12mil",
                            "email_125_sends_greaterthan12mil",
                            "email_125_sends_lessthan6mil_seasonal",
                            "email_125_sends_between6and12mil_seasonal",
                            "email_125_sends_greaterthan12mil_seasonal",                           
                            "Email_promo_bucket_125",  
                            "Email_promo_bucket_150",  
                            "Email_promo_bucket_25",     
                            "Email_promo_bucket_50",   
                            "Email_promo_bucket_75",   
                            "Email_promo_bucket_DL",   
                            "Email_promo_bucket_FREE", 
                            "Email_promo_bucket_GAME", 
                            "Email_promo_bucket_NoOFF",
                            "Email_promo_bucket_R1G1", 
                            "Push_promo_bucket_125",   
                            "Push_promo_bucket_50",    
                            "Push_promo_bucket_75",    
                            "Push_promo_bucket_DL",    
                            "Push_promo_bucket_FREE",  
                            "Push_promo_bucket_GAME",  
                            "Push_promo_bucket_NoOFF", 
                            "Push_promo_bucket_R1G1",  
                            "SMS_promo_bucket_125",    
                            "SMS_promo_bucket_150",    
                            "SMS_promo_bucket_50",     
                            "SMS_promo_bucket_75",     
                            "SMS_promo_bucket_DL",     
                            "SMS_promo_bucket_FREE",   
                            "SMS_promo_bucket_GAME",   
                            "SMS_promo_bucket_NoOFF",  
                            "SMS_promo_bucket_R1G1",
                            "Email_promo_bucket_lessthandl",
                            "retail_promo_redemptions",
                            "Push_promo_bucket_R1G1_seasonal",
                            "Email_promo_bucket_R1G1_seasonal",
                            "SMS_promo_bucket_R1G1_seasonal")]

ads_2016_log = as.data.frame(apply(ads_2016_log, 2, FUN = log))

is.nan.data.frame = function(x) do.call(cbind, lapply(x, is.nan))
ads_2016_log[is.nan.data.frame(ads_2016_log)] = 0

is.inf.data.frame = function(x) do.call(cbind, lapply(x, is.infinite))
ads_2016_log[is.inf.data.frame(ads_2016_log)] = 0


ads_2016_log$Email_promo_bucket_125_dummy <- ifelse(ads_2016_log$Email_promo_bucket_125 > 0, 1, 0)
ads_2016_log$SMS_promo_bucket_R1G1_dummy <- ifelse(ads_2016_log$SMS_promo_bucket_R1G1 > 0, 1, 0)
ads_2016_log$Push_promo_bucket_R1G1_dummy <- ifelse(ads_2016_log$Push_promo_bucket_R1G1 > 0, 1, 0)
ads_2016_log$Email_promo_bucket_R1G1_dummy <- ifelse(ads_2016_log$Email_promo_bucket_R1G1 > 0, 1, 0)

# merging dataset

ads_final = cbind(ads_2016_log, ads_2016[, c("holidays", "kiosk_indoor_outdoor", "kiosk_disc_capacity", "incart_dummy", "out_dummy", 
                                             "week", "year_range")])

ads_final$kiosk_indoor_outdoor = as.factor(ads_final$kiosk_indoor_outdoor)
ads_final$kiosk_disc_capacity = as.factor(as.character(ads_final$kiosk_disc_capacity))
ads_final$holidays = as.factor(ifelse(ads_final$holidays > 0, "1", "0"))
ads_final$incart_dummy = as.factor(ads_final$incart_dummy)
ads_final$out_dummy = as.factor(ads_final$out_dummy)

# making these as factors

ads_final$Email_promo_bucket_125_dummy = as.factor(ads_final$Email_promo_bucket_125_dummy)
ads_final$SMS_promo_bucket_R1G1_dummy = as.factor(ads_final$SMS_promo_bucket_R1G1_dummy)
ads_final$Push_promo_bucket_R1G1_dummy = as.factor(ads_final$Push_promo_bucket_R1G1_dummy)
ads_final$Email_promo_bucket_R1G1_dummy = as.factor(ads_final$Email_promo_bucket_R1G1_dummy)

ads_final = merge(ads_final, incart, by.x = "week", by.y = "weeknum", all.x = T)
ads_final[is.na(ads_final)] = 0

ads_final$retail_redemptions_dummy = as.factor(ifelse(ads_final$retail_promo_redemptions > 0, 1, 0))

# cleaning missing or unusual values

ads_final[is.nan.data.frame(ads_final)] = 0

ads_final[is.inf.data.frame(ads_final)] = 0

ads_final[is.na(ads_final)] = 0


rm(all_promo_sends, holidays_as_flag, holidays_as_flag_rollup, incart, incart_dummy, retail_redemptions_dummy, retail_promo_redemptions_2016)

# "Push_promo_bucket_R1G1_seasonal", "Email_promo_bucket_R1G1_seasonal","SMS_promo_bucket_R1G1_seasonal"

# running model

model = lm(weekly_units_per_kiosk ~ 
             Email_promo_bucket_DL +
             Email_promo_bucket_FREE +
             Email_promo_bucket_NoOFF +
             SMS_promo_bucket_FREE +
             Push_promo_bucket_FREE +
             sem_seo_display_spend_per_kiosk_per_week +
             social_spend_per_kiosk_per_week +
             lag_weekly_units_per_kiosk +
             Email_promo_bucket_lessthandl +
             kiosk_indoor_outdoor +
             content_strength +
             week_rental_seasonality +
             KioskAge_days +
             incart_qualifying_redem_volume_per_kiosk_per_week +
             all_kiosk_in_half_and_five_mile_cumulative +
             # mean_household_income +
             Push_promo_bucket_NoOFF +
             out_redem_impressions_per_kiosk_per_week +
             SMS_promo_bucket_NoOFF +
             email_125_sends_lessthan6mil_seasonal +
             #email_125_sends_between6and12mil_seasonal +
             email_125_sends_greaterthan12mil_seasonal +
             SMS_promo_bucket_R1G1+
             Email_promo_bucket_R1G1 +
             retail_redemptions_dummy
        , data = ads_final)


summary(model)
as.data.frame(vif(model))


stepwise <- step(model)
summary(stepwise)

# # predict on new data
# predict_bucket <- predict(stepwise, ads_final_new_2017)
# 
# 
# # Accuracy Estimation
# accuracy <- cbind(predict_bucket, ads_final_new_2017$weekly_units_per_kiosk)
# colnames(accuracy) <- c("predicted", "actual")
# 
# accuracy <- as.data.frame(accuracy)
# 
# #anti log
# accuracy$Predicted_exp <- exp(accuracy$predicted)
# accuracy$Observed_exp <- exp(accuracy$actual)
# 
# # error calculation
# accuracy$error <- (accuracy$Observed_exp - accuracy$Predicted_exp)/(accuracy$Observed_exp)
# 
# # Mean Absoulte Error Percentage
# accuracy_2017 <- (1 - mean(abs(accuracy$error))) * 100

#****************************************************************************************************************#
#*                                       CALCULATING THE CONTRIBUTION                                           *#
#****************************************************************************************************************#

#Model summary as data frame
coeff_model <- coef(stepwise)
coeff_model <- as.data.frame(cbind(row.names(coeff_model), coeff_model))
coeff_model$variable_name <- rownames(coeff_model)

# # drop row names
row.names(coeff_model) <- NULL

# # rearranging the columns
coeff_model_melt <- melt(coeff_model, id.var = "variable_name")
coeff_model_melt$variable <- NULL 

# # renaming colnames
setnames(coeff_model_melt, old = c("variable_name", "value"), new = c("variable", "beta"))
rm(coeff_model)

# # to calculate average of each variable in the model

df = ads_final[,!sapply(ads_final,class) %in% "character"]

column_names = mapply(function(a,b){
  if(a %>% class %in% c("numeric","integer")){
    paste0(b,"__",abs(mean(a, na.rm=T)))
  }else if(class(a)=="factor"){
    paste0(b,levels(a),"__1")
  }
}, df, names(df)) %>% unlist()#%>% class %>% print


# # each variable provides mean, for factor columns you get a one
df_average = data.frame(colname = column_names, stringsAsFactors = F) %>% tidyr::separate(col=colname,into=c("variable","value"),sep="__" )
rm(df)

# # calculation of contribution
# 
contribution_table <- left_join(coeff_model_melt, df_average, by = c("variable" = "variable"))
rm(coeff_model_melt, df_average)
# 
# # making intercept mean as 1
contribution_table[is.na(contribution_table)] <- 1
# 
#calculating beta * value, assuming the data is logged, using the same data given to model
# 
contribution_table$product <- as.numeric(contribution_table$beta) * as.numeric(contribution_table$value)
# 
setDT(contribution_table)
# 
contribution_table$total <- contribution_table[,sum((product))]
# 
contribution_table$percent_contribution <- (contribution_table$product)/(contribution_table$total)

write.csv(contribution_table, file = "C:/Users/anubh/Desktop/contribution.csv", row.names = FALSE)

#****************************************************************************************************************#
#*                           BUILDING A 10 FOLD MODEL ON DATA WITH NO OUTLIER REMOVED                            *#
#****************************************************************************************************************#

# Accuracy Determination  
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)

mmx_model <- train(weekly_units_per_kiosk ~ 
                     Email_promo_bucket_DL +
                     Email_promo_bucket_FREE +
                     Email_promo_bucket_NoOFF +
                     SMS_promo_bucket_FREE +
                     Push_promo_bucket_FREE +
                     sem_seo_display_spend_per_kiosk_per_week +
                     social_spend_per_kiosk_per_week +
                     lag_weekly_units_per_kiosk +
                     Email_promo_bucket_lessthandl +
                     kiosk_indoor_outdoor +
                     content_strength +
                     week_rental_seasonality +
                     KioskAge_days +
                     incart_qualifying_redem_volume_per_kiosk_per_week +
                     all_kiosk_in_half_and_five_mile_cumulative +
                     # mean_household_income +
                     Push_promo_bucket_NoOFF +
                     out_redem_impressions_per_kiosk_per_week +
                     SMS_promo_bucket_NoOFF +
                     email_125_sends_lessthan6mil_seasonal +
                     #email_125_sends_between6and12mil_seasonal +
                     email_125_sends_greaterthan12mil_seasonal +
                     SMS_promo_bucket_R1G1+
                     Email_promo_bucket_R1G1 +
                     retail_redemptions_dummy
                   ,
                   data = ads_final, trControl=train_control, method="lm", na.action = "na.omit")

results <- as.data.frame(cbind(mmx_model$pred$pred, mmx_model$pred$obs, mmx_model$pred$Resample))
colnames(results) <- c("Predicted", "Observed", "Fold")

# Accuracy Estimation
results$Predicted <- as.numeric(as.character(results$Predicted))
results$Observed <- as.numeric(as.character(results$Observed))

#anti log
results$Predicted_exp <- exp(results$Predicted)
results$Observed_exp <- exp(results$Observed)

# error calculation
results$error <- (results$Observed_exp - results$Predicted_exp)/(results$Observed_exp)

# Mean Absoulte Error Percentage
accuracy <- (1 - mean(abs(results$error))) * 100

#write.csv(results, "C:/Users/anubh/Desktop/results_of_mode.csv", row.names = FALSE)

cat("The Accuracy of model with no outlier treatment is: ", accuracy)

#****************************************************************************************************************#
#*                                    RESPONSE CURVE AUTOMATION MODULE                                          *#
#****************************************************************************************************************#

percentage = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99, 1)
response_curves = as.data.frame(percentage)

response_curves = apply(ads_2016[,c("weekly_units_per_kiosk",
                                    "sem_seo_display_spend_per_kiosk_per_week",
                                    "week_rental_seasonality",
                                    "social_spend_per_kiosk_per_week",
                                    "content_strength",
                                    "inventory_strength",
                                    "all_kiosk_in_half_and_five_mile_cumulative",
                                    "mean_household_income",
                                    "lag_weekly_units_per_kiosk",
                                    "inventory_skew",
                                    "KioskAge_days",
                                    "incart_qualifying_redem_volume_per_kiosk_per_week",
                                    "out_redem_impressions_per_kiosk_per_week",
                                    "pr_spend_per_kiosk_per_week",
                                    "email_125_sends_lessthan6mil",
                                    "email_125_sends_between6and12mil",
                                    "email_125_sends_greaterthan12mil",
                                    "email_125_sends_lessthan6mil_seasonal",
                                    "email_125_sends_between6and12mil_seasonal",
                                    "email_125_sends_greaterthan12mil_seasonal",                           
                                    "Email_promo_bucket_125",  
                                    "Email_promo_bucket_150",  
                                    "Email_promo_bucket_25",     
                                    "Email_promo_bucket_50",   
                                    "Email_promo_bucket_75",   
                                    "Email_promo_bucket_DL",   
                                    "Email_promo_bucket_FREE", 
                                    "Email_promo_bucket_GAME", 
                                    "Email_promo_bucket_NoOFF",
                                    "Email_promo_bucket_R1G1", 
                                    "Push_promo_bucket_125",   
                                    "Push_promo_bucket_50",    
                                    "Push_promo_bucket_75",    
                                    "Push_promo_bucket_DL",    
                                    "Push_promo_bucket_FREE",  
                                    "Push_promo_bucket_GAME",  
                                    "Push_promo_bucket_NoOFF", 
                                    "Push_promo_bucket_R1G1",  
                                    "SMS_promo_bucket_125",    
                                    "SMS_promo_bucket_150",    
                                    "SMS_promo_bucket_50",     
                                    "SMS_promo_bucket_75",     
                                    "SMS_promo_bucket_DL",     
                                    "SMS_promo_bucket_FREE",   
                                    "SMS_promo_bucket_GAME",   
                                    "SMS_promo_bucket_NoOFF",  
                                    "SMS_promo_bucket_R1G1",
                                    "Email_promo_bucket_lessthandl",
                                    "retail_promo_redemptions",
                                    "Push_promo_bucket_R1G1_seasonal",
                                    "Email_promo_bucket_R1G1_seasonal",
                                    "SMS_promo_bucket_R1G1_seasonal")], MARGIN = 2, FUN = function(x) ifelse( !all(is.na(x)), max(x, na.rm=T))*percentage) %>% as.data.frame()

response_curves_new = cbind(percentage, response_curves)

response_curves_mean = apply(ads_2016[,c("weekly_units_per_kiosk",
                                         "sem_seo_display_spend_per_kiosk_per_week",
                                         "week_rental_seasonality",
                                         "social_spend_per_kiosk_per_week",
                                         "content_strength",
                                         "inventory_strength",
                                         "all_kiosk_in_half_and_five_mile_cumulative",
                                         "mean_household_income",
                                         "lag_weekly_units_per_kiosk",
                                         "inventory_skew",
                                         "KioskAge_days",
                                         "incart_qualifying_redem_volume_per_kiosk_per_week",
                                         "out_redem_impressions_per_kiosk_per_week",
                                         "pr_spend_per_kiosk_per_week",
                                         "email_125_sends_lessthan6mil",
                                         "email_125_sends_between6and12mil",
                                         "email_125_sends_greaterthan12mil",
                                         "email_125_sends_lessthan6mil_seasonal",
                                         "email_125_sends_between6and12mil_seasonal",
                                         "email_125_sends_greaterthan12mil_seasonal",                           
                                         "Email_promo_bucket_125",  
                                         "Email_promo_bucket_150",  
                                         "Email_promo_bucket_25",     
                                         "Email_promo_bucket_50",   
                                         "Email_promo_bucket_75",   
                                         "Email_promo_bucket_DL",   
                                         "Email_promo_bucket_FREE", 
                                         "Email_promo_bucket_GAME", 
                                         "Email_promo_bucket_NoOFF",
                                         "Email_promo_bucket_R1G1", 
                                         "Push_promo_bucket_125",   
                                         "Push_promo_bucket_50",    
                                         "Push_promo_bucket_75",    
                                         "Push_promo_bucket_DL",    
                                         "Push_promo_bucket_FREE",  
                                         "Push_promo_bucket_GAME",  
                                         "Push_promo_bucket_NoOFF", 
                                         "Push_promo_bucket_R1G1",  
                                         "SMS_promo_bucket_125",    
                                         "SMS_promo_bucket_150",    
                                         "SMS_promo_bucket_50",     
                                         "SMS_promo_bucket_75",     
                                         "SMS_promo_bucket_DL",     
                                         "SMS_promo_bucket_FREE",   
                                         "SMS_promo_bucket_GAME",   
                                         "SMS_promo_bucket_NoOFF",  
                                         "SMS_promo_bucket_R1G1",
                                         "Email_promo_bucket_lessthandl",
                                         "retail_promo_redemptions",
                                         "Push_promo_bucket_R1G1_seasonal",
                                         "Email_promo_bucket_R1G1_seasonal",
                                         "SMS_promo_bucket_R1G1_seasonal")], MARGIN = 2, FUN = function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T))) %>% as.data.frame()


# reshaping and naming
response_curves_long <- melt(response_curves_new, id.vars = "percentage")
response_curves_long <- response_curves_long[,c("variable", "percentage", "value")]

model_x_beta <- inner_join(x = contribution_table[, c("variable", "beta")], 
                           y = response_curves_long, by = c("variable" = "variable"))

model_x_beta$flag <- "data"

# making rownames as column
response_curves_mean$variable <- rownames(response_curves_mean)
rownames(response_curves_mean) <- NULL
colnames(response_curves_mean) <- c("mean_x", "variable")

# inner join
response_curves_mean_mapped <- inner_join(x = model_x_beta, y = response_curves_mean, by = c("variable" = "variable"))
response_curves_mean_mapped <- response_curves_mean_mapped[response_curves_mean_mapped$percentage == 1.000,]

# data mapping 
response_curves_mean_mapped$percentage <- response_curves_mean_mapped$mean_x/response_curves_mean_mapped$value


response_curves_mean_mapped$value <- response_curves_mean_mapped$mean_x
response_curves_mean_mapped$mean_x <- NULL
response_curves_mean_mapped <- unique(response_curves_mean_mapped)
beta_x_mean <- rbind(model_x_beta, response_curves_mean_mapped)



# appending the mean values
response_curves_mean_mapped_append <- response_curves_mean_mapped
response_curves_mean_mapped_append$flag <- "mean"

response_curves_mean_mapped_append_final <- rbind(response_curves_mean_mapped_append, response_curves_mean_mapped)

# final appending in the dataset
beta_x_mean <- rbind(model_x_beta, response_curves_mean_mapped_append_final)
beta_x_mean$x_raised_beta <-  beta_x_mean$value^(beta_x_mean$beta)


rm(beta, response_curves, response_curves_long, response_curves_mean, 
   response_curves_mean_mapped, response_curves_mean_mapped_append, response_curves_mean_mapped_append_final, ads_marketing, email_125_sends_per_week)

fwrite(beta_x_mean, file = "C:/Users/anubh/Desktop/response_curve_final.csv", row.names = FALSE)


