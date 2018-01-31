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


ads = ads[ads$year_range == 2016, c("col1", "col2", "col3"]



model = lm(weekly_units_per_kiosk ~., data = ads)


summary(model)
as.data.frame(vif(model))


stepwise <- step(model)
summary(stepwise)

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

mmx_model <- train(weekly_units_per_kiosk ~.,
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

response_curves = apply(ads_2016[,c("col1", "col2")], MARGIN = 2, FUN = function(x) ifelse( !all(is.na(x)), max(x, na.rm=T))*percentage) %>% as.data.frame()

response_curves_new = cbind(percentage, response_curves)

response_curves_mean = apply(ads_2016[,c("col1", "col2")], MARGIN = 2, FUN = function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T))) %>% as.data.frame()


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


