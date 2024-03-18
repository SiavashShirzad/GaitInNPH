#Siavash Shirzadeh analysis forgait characteristics in NPH patients
install.packages("corrplot")
install.packages("Hmisc")
install.packages("corrplot")
install.packages(c("pROC"))
install.packages(c("ggplot2"))
install.packages(c("glm"))
install.packages(c("glmnet"))
install.packages(c("readxl"))
install.packages(c("dplyr"))
install.packages("pROC")


library(glm)
library(pROC)
library(ggplot2)
library(Hmisc)
library(readxl)
library(dplyr)
library(glmnet)
library(corrplot)


excel_file <- "Clinic Masterlist 2015-2022.xlsx"

## outliers Calculator and remover
remove_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm=TRUE)
  Q3 <- quantile(column, 0.75, na.rm=TRUE)
  IQR_value <- Q3 - Q1
  threshold <- 1.5
  
  outliers <- column < (Q1 - threshold * IQR_value) | column > (Q3 + threshold * IQR_value)
  
  column[outliers] <- NA
  return(column)
}

##Simple first-line analysis of Gait features in NPH Patients

demographic_data = read_excel(excel_file, sheet = "NPH- all demographics")[-324, ]
sex_shunt_table = table(demographic_data$Sex, demographic_data$`Shunt? (0=no, 1=yes)`)
sex_shunt_table_percent = prop.table(sex_shunt_table) * 100
result_table_sex_shunt <- data.frame(
  Count = sex_shunt_table,
  Percentage = sex_shunt_table_percent
)
result_table_sex_shunt

result_summary_Age_bySex <- demographic_data %>%
  group_by(Sex) %>%
  summarise(
    Mean_Age = mean(`Age at LP/ELD`, na.rm = TRUE),
    Std_Age = sd(`Age at LP/ELD`, na.rm = TRUE)
  )
result_summary_Age_bySex

sex_counts <- table(demographic_data$Sex)
sex_percentages <- prop.table(sex_counts) * 100
result_table_sex_percentages <- data.frame(
  Count = sex_counts,
  Percentage = sex_percentages
)
result_table_sex_percentages

## Determining the Domains and patients status in each domain

baseline_NPH_df = read_excel(excel_file, sheet = "NPH-baseline only")
colnames(baseline_NPH_df[66:72]) = baseline_NPH_df[1,66:72]
baseline_NPH_df_test_Ev = baseline_NPH_df[-1, 66:72] 
colnames(baseline_NPH_df_test_Ev) = baseline_NPH_df[1,66:72]
baseline_NPH_df_test_Ev = baseline_NPH_df_test_Ev[, -2]

combined_df_Normal_Abnormal <- data.frame(t(data.frame(
  TUG = as.vector(table(baseline_NPH_df_test_Ev$TUG)),
  MW10 = as.vector(table(baseline_NPH_df_test_Ev$'10MW')),
  BT4S = as.vector(table(baseline_NPH_df_test_Ev$'4SBT')),
  STS30 = as.vector(table(baseline_NPH_df_test_Ev$`30 STS`)),
  MW2 = as.vector(table(baseline_NPH_df_test_Ev$'2MW'))
)))
colnames(combined_df_Normal_Abnormal) = c("Normal", "Abnormal")
combined_df_Normal_Abnormal = cbind(combined_df_Normal_Abnormal, Domain = c("Speed", "Speed", "Balance", "Balance", "Endurance"))
Abnormal_Percentage_compbined_Df = combined_df_Normal_Abnormal$Abnormal/(combined_df_Normal_Abnormal$Abnormal+combined_df_Normal_Abnormal$Normal)*100
combined_df_Normal_Abnormal = cbind(combined_df_Normal_Abnormal, Abnormal_Percentage = Abnormal_Percentage_compbined_Df)
combined_df_Normal_Abnormal



## combination of domains in NPH patients

speed_domain_abnormals = data.frame(lapply(data.frame(cbind(baseline_NPH_df_test_Ev$TUG,baseline_NPH_df_test_Ev$'10MW')), as.integer))
balance_domain_abnormals = data.frame(lapply(data.frame(cbind(baseline_NPH_df_test_Ev$'4SBT',baseline_NPH_df_test_Ev$`30 STS`)), as.integer))
endurance_domain_abnormals = cbind(baseline_NPH_df_test_Ev$'2MW')

baseline_NPH_df_test_Ev = data.frame(lapply(baseline_NPH_df_test_Ev, as.integer))[,-4]
baseline_NPH_df_test_Ev$TUG[is.na(baseline_NPH_df_test_Ev$TUG)] <- baseline_NPH_df_test_Ev$X10MW[is.na(baseline_NPH_df_test_Ev$TUG)]
baseline_NPH_df_test_Ev$X10MW[is.na(baseline_NPH_df_test_Ev$X10MW)] <- baseline_NPH_df_test_Ev$TUG[is.na(baseline_NPH_df_test_Ev$X10MW)]
baseline_NPH_df_test_Ev$X4SBT[is.na(baseline_NPH_df_test_Ev$X4SBT)] <- baseline_NPH_df_test_Ev$'X30.STS'[is.na(baseline_NPH_df_test_Ev$X4SBT)]
baseline_NPH_df_test_Ev$'X30.STS'[is.na(baseline_NPH_df_test_Ev$'X30.STS')] <- baseline_NPH_df_test_Ev$X4SBT[is.na(baseline_NPH_df_test_Ev$'X30.STS')]

nAOmitted_domainAdapted_baseline_NPH = na.omit(baseline_NPH_df_test_Ev)
nAOmitted_domainAdapted_baseline_NPH = cbind(nAOmitted_domainAdapted_baseline_NPH, speed_Domain = (nAOmitted_domainAdapted_baseline_NPH$TUG >0 | nAOmitted_domainAdapted_baseline_NPH$X10MW > 0))
nAOmitted_domainAdapted_baseline_NPH = cbind(nAOmitted_domainAdapted_baseline_NPH, balance_Domain = (nAOmitted_domainAdapted_baseline_NPH$X4SBT >0 | nAOmitted_domainAdapted_baseline_NPH$'X30.STS' > 0))
nAOmitted_domainAdapted_baseline_NPH = cbind(nAOmitted_domainAdapted_baseline_NPH, endurance_Domain = (nAOmitted_domainAdapted_baseline_NPH$X2MW >0))
dim(nAOmitted_domainAdapted_baseline_NPH)
domain_dinfo = t(data.frame(cbind(all_domains = sum(nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & nAOmitted_domainAdapted_baseline_NPH$balance_Domain & nAOmitted_domainAdapted_baseline_NPH$speed_Domain),
      speed_balance = sum(!nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & nAOmitted_domainAdapted_baseline_NPH$balance_Domain & nAOmitted_domainAdapted_baseline_NPH$speed_Domain),
      balance_endurance = sum(nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & nAOmitted_domainAdapted_baseline_NPH$balance_Domain & !nAOmitted_domainAdapted_baseline_NPH$speed_Domain),
      balance = sum(!nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & nAOmitted_domainAdapted_baseline_NPH$balance_Domain & !nAOmitted_domainAdapted_baseline_NPH$speed_Domain),
      endurance = sum(nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & !nAOmitted_domainAdapted_baseline_NPH$balance_Domain & !nAOmitted_domainAdapted_baseline_NPH$speed_Domain),
      speed_endurance = sum(nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & !nAOmitted_domainAdapted_baseline_NPH$balance_Domain & nAOmitted_domainAdapted_baseline_NPH$speed_Domain),
      speed = sum(!nAOmitted_domainAdapted_baseline_NPH$endurance_Domain & !nAOmitted_domainAdapted_baseline_NPH$balance_Domain & nAOmitted_domainAdapted_baseline_NPH$speed_Domain)
      )))
domain_dinfo = cbind(n_patients = domain_dinfo[,1], percent_abnormal = domain_dinfo[,1]/sum(domain_dinfo))
domain_dinfo

baseline_NPH_df_test_Ev_CATEGORICAL_WITH_FALLS = cbind(baseline_NPH_df_test_Ev,baseline_NPH_df[-1,18])
baseline_NPH_df_test_Ev_CATEGORICAL_WITH_FALLS$`Falls (Y/N)`[baseline_NPH_df_test_Ev_CATEGORICAL_WITH_FALLS$`Falls (Y/N)` == '999'] = NA
baseline_NPH_df_test_Ev_CATEGORICAL_WITH_FALLS

nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS = na.omit(baseline_NPH_df_test_Ev_CATEGORICAL_WITH_FALLS)
nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS = cbind(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS, speed_Domain = (nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS$TUG >0 | nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS$X10MW > 0))
nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS = cbind(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS, balance_Domain = (nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS$X4SBT >0 | nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS$'X30.STS' > 0))
nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS = cbind(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS, endurance_Domain = (nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS$X2MW >0))
nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS = data.frame(lapply(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS, as.numeric))

result_list <- lapply(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS[, -which(names(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS) == 'Falls..Y.N.')], 
                      function(column) {
                        contingency_table <- table(column, nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS$'Falls..Y.N.')
                        chi_square_test <- chisq.test(contingency_table)
                        return(list(column_name = names(column),
                                    chi_square_statistic = chi_square_test$statistic,
                                    p_value = chi_square_test$p.value))
                      })

# Displaying the results
result_dataframe <- data.frame(do.call(rbind, result_list))
print(result_dataframe)


# MultiLogistic Binary Domain Fall Prediction
nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS[, -c(1,2,3,4,5)]
Fall_Logistic_binary_Domain_model <- glm(Falls..Y.N. ~ ., data = nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS[, -c(1,2,3,4,5)], family = binomial)
summary(Fall_Logistic_binary_Domain_model)

# Single Logistic binary regression predicting fall using domains

# Initialize an empty dataframe to store results
results_df_single_Fall_Logistic_binary_Domain_model <- data.frame(variable = character(), coefficient = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
for (col in names(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS[, -c(1,2,3,4,5)])[-which(names(nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS[, -c(1,2,3,4,5)]) == "Falls..Y.N.")]) {
  model <- glm(Falls..Y.N. ~ nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS[[col]], data = nAOmitted_domainAdapted_baseline_NPH_CATEGORICAL_WITH_FALLS, family = binomial)
  coefficient <- coef(summary(model))[,1][2] # Coefficient for the predictor variable
  p_value <- coef(summary(model))[,4][2] # P-value for the predictor variable
  results_df_single_Fall_Logistic_binary_Domain_model <- rbind(results_df_single_Fall_Logistic_binary_Domain_model, data.frame(variable = col, coefficient = coefficient, p_value = p_value))
}
print(results_df_single_Fall_Logistic_binary_Domain_model)

# gait test abnormality vs fall stat
baseline_raw_data = data.frame(baseline_NPH_df[, 34:41])
colnames(baseline_raw_data) = baseline_raw_data[1,]
baseline_raw_data = data.frame(lapply(baseline_raw_data[-c(1),], as.numeric))
baseline_raw_data = data.frame(cbind(baseline_raw_data, fall = data.frame(baseline_NPH_df[-1, 18])))
baseline_raw_data$Falls..Y.N.[baseline_raw_data$Falls..Y.N. == '999'] = NA
baseline_raw_data[, -9] = data.frame(lapply(baseline_raw_data[, -9], as.numeric))
#baseline_raw_data[, -9] = data.frame(apply(baseline_raw_data[ ,-9], 2, remove_outliers))

boxplot(baseline_raw_data$TUG~ baseline_raw_data$'Falls..Y.N.')
boxplot(baseline_raw_data$X10MW..s.~ baseline_raw_data$'Falls..Y.N.')
boxplot(baseline_raw_data$X30.STS~ baseline_raw_data$'Falls..Y.N.')
boxplot(baseline_raw_data$X2MW~ baseline_raw_data$'Falls..Y.N.')

t_test_results <- list()
for (column_name in colnames(baseline_raw_data[, -c(3,4,5,6)])[colnames(baseline_raw_data) != 'Falls..Y.N.']) {
  tryCatch(
    {
      t_test_result <- t.test(baseline_raw_data[, column_name] ~ baseline_raw_data$'Falls..Y.N.')
      t_test_results[[column_name]] <- t_test_result
    }
  )
}
t_test_results
for (column_name in names(t_test_results)) {
  cat("Variable:", column_name, "\n")
  cat("Variable:", "variable", "\n")
  cat("t-statistic:", t_test_results[[column_name]]$statistic, "\n")
  cat("Degrees of Freedom:", t_test_results[[column_name]]$parameter, "\n")
  cat("P-value:", t_test_results[[column_name]]$p.value, "\n")
  cat("Confidence Interval:", t_test_results[[column_name]]$conf.int, "\n")
  cat("Mean Difference:", t_test_results[[column_name]]$estimate, "\n")
  cat("\n")
}

# Chi Square for 4 stage balance test :: Unnder 10s for each was considered abnormal

Balancetest_dataset = baseline_raw_data[, 3:6]
Balancetest_dataset[Balancetest_dataset != 10] = 0
Balancetest_dataset[Balancetest_dataset == 10] = 1
Balancetest_dataset = cbind(Balancetest_dataset, baseline_raw_data$Falls..Y.N.)
Balancetest_dataset = na.omit(Balancetest_dataset)
chisq_result1 <- chisq.test(Balancetest_dataset$X4SB.EO, Balancetest_dataset$`baseline_raw_data$Falls..Y.N.`)
chisq_result2 <- chisq.test(Balancetest_dataset$X4SB.Stag, Balancetest_dataset$`baseline_raw_data$Falls..Y.N.`)
chisq_result3 <- chisq.test(Balancetest_dataset$X4SB.Tand, Balancetest_dataset$`baseline_raw_data$Falls..Y.N.`)
chisq_result4 <- chisq.test(Balancetest_dataset$X4SB.OL, Balancetest_dataset$`baseline_raw_data$Falls..Y.N.`)
print(chisq_result1)
print(chisq_result2)
print(chisq_result3)
print(chisq_result4)

# Correlation between tests and N of falls

baseline_raw_data_corr = data.frame(baseline_NPH_df[, 34:41])
colnames(baseline_raw_data_corr) = baseline_raw_data_corr[1,]
baseline_raw_data_corr = data.frame(lapply(baseline_raw_data_corr[-c(1),], as.numeric))
baseline_raw_data_corr = data.frame(cbind(baseline_raw_data_corr, fall = data.frame(baseline_NPH_df[-1, 17])))
baseline_raw_data_corr$'Number.of.falls'[baseline_raw_data_corr$'Number.of.falls' == '999'] = NA
baseline_raw_data_corr$'Number.of.falls' = t(data.frame(lapply(baseline_raw_data_corr$'Number.of.falls', as.numeric)))
baseline_raw_data_corr = na.omit(baseline_raw_data_corr)
baseline_raw_data_corr = baseline_raw_data_corr[, -c(3,4,5, 6)]

correlation_results <- rcorr(as.matrix(baseline_raw_data_corr), type = "pearson")
correlation_matrix <- correlation_results$r
p_values_matrix <- correlation_results$P
correlation_matrix <- cor(baseline_raw_data_corr,  use = "complete.obs")
corrplot(correlation_matrix, type = "upper")
corrplot::corrplot(cor(correlation_matrix), type = "upper", addCoef.col = "black", p.mat = p_values_matrix, sig.level = 0.05, insig = "blank", add = TRUE)


correlation_coefficient <- cor(baseline_raw_data_corr$TUG, baseline_raw_data_corr$'Number.of.falls',  use = "complete.obs")
cor.test(baseline_raw_data_corr$TUG, baseline_raw_data_corr$'Number.of.falls')
plot( baseline_raw_data_corr$'Number.of.falls',baseline_raw_data_corr$TUG, main = "Scatter Plot with Correlation Line", 
     xlab = "Falls", ylab = "TUG", pch = 16, col = "blue")
abline(lm(baseline_raw_data_corr$TUG ~ baseline_raw_data_corr$'Number.of.falls'), col = "red", )
legend("topright", legend = paste("Correlation =", round(correlation_coefficient, 2)), col = "red", lty = 1)

correlation_coefficient <- cor(baseline_raw_data_corr$'X10MW..s.', baseline_raw_data_corr$'Number.of.falls',  use = "complete.obs")
cor.test(baseline_raw_data_corr$'X10MW..s.', baseline_raw_data_corr$'Number.of.falls')
plot( baseline_raw_data_corr$'Number.of.falls', baseline_raw_data_corr$'X10MW..s.',main = "Scatter Plot with Correlation Line", 
     xlab = "Falls", ylab = "10MW", pch = 16, col = "blue")
abline(lm(baseline_raw_data_corr$'X10MW..s.' ~ baseline_raw_data_corr$'Number.of.falls'), col = "red", )
legend("topright", legend = paste("Correlation =", round(correlation_coefficient, 2)), col = "red", lty = 1)

correlation_coefficient <- cor(baseline_raw_data_corr$X2MW, baseline_raw_data_corr$'Number.of.falls',  use = "complete.obs")
cor.test(baseline_raw_data_corr$X2MW, baseline_raw_data_corr$'Number.of.falls')
plot( baseline_raw_data_corr$'Number.of.falls', baseline_raw_data_corr$X2MW,main = "Scatter Plot with Correlation Line", 
      xlab = "Falls", ylab = "2MW", pch = 16, col = "blue")
abline(lm(baseline_raw_data_corr$X2MW ~ baseline_raw_data_corr$'Number.of.falls'), col = "red", )
legend("topright", legend = paste("Correlation =", round(correlation_coefficient, 2)), col = "red", lty = 1)

correlation_coefficient <- cor(baseline_raw_data_corr$X30.STS, baseline_raw_data_corr$'Number.of.falls',  use = "complete.obs")
cor.test(baseline_raw_data_corr$X30.STS, baseline_raw_data_corr$'Number.of.falls')
plot( baseline_raw_data_corr$'Number.of.falls', baseline_raw_data_corr$X30.STS,main = "Scatter Plot with Correlation Line", 
      xlab = "Falls", ylab = "30SS2S", pch = 16, col = "blue")
abline(lm(baseline_raw_data_corr$X30.STS ~ baseline_raw_data_corr$'Number.of.falls'), col = "red", )
legend("topright", legend = paste("Correlation =", round(correlation_coefficient, 2)), col = "red", lty = 1)

## Logistic Regression Models Predicting Fall

baseline_raw_regression_df = data.frame(lapply(na.omit(baseline_raw_data), as.numeric))
baseline_raw_regression_df <- baseline_raw_regression_df %>%
  mutate_at(vars(3:6), ~ ifelse(. < 10, 0, .))
baseline_raw_regression_df <- baseline_raw_regression_df %>%
  mutate_at(vars(3:6), ~ ifelse(. == 999, 0, .))
baseline_raw_regression_df <- baseline_raw_regression_df %>%
  mutate_at(vars(3:6), ~ ifelse(. == 10, 1, .))
Normalized_baseline_raw_regression_df= data.frame(scale(baseline_raw_regression_df[,-9]))
baseline_raw_regression_df[, 1:8] = Normalized_baseline_raw_regression_df
# Multivariate Logistic Regression Model No Lasso

logit_model <- glm(Falls..Y.N. ~., data = baseline_raw_regression_df, family = binomial)
predicted_probs <- predict(logit_model, newdata = baseline_raw_regression_df, type = "response")
roc_curve <- roc(baseline_raw_regression_df$Falls..Y.N., predicted_probs)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), col = "black", cex = 1.5)

coefficients <- coef(logit_model)
feature_names <- names(coefficients)[-1]
coef_df <- data.frame(feature = feature_names, coefficient = coefficients[-1])
coef_df <- coef_df[order(abs(coef_df$coefficient), decreasing = TRUE), ]
ggplot(coef_df, aes(x = reorder(feature, coefficient), y = coefficient, fill = coefficient > 0)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(title = "Feature Importance Plot for Logistic Regression",
       x = "Coefficient Value",
       y = "Feature") +
  theme_minimal()
summary(logit_model)

# Single Variate Logistic Regression Model Predicting Fall

singleFall_results_list <- list()
for (col_name in colnames(baseline_raw_regression_df)[-which(colnames(baseline_raw_regression_df) == "Falls..Y.N.")]) {
  formula <- as.formula(paste("Falls..Y.N. ~", col_name))
  singleFall_model <- glm(formula, data = baseline_raw_regression_df, family = binomial)
  singleFall_results <- summary(singleFall_model)$coefficients
  singleFall_results_list[[col_name]] <- singleFall_results
  print(singleFall_results_list)
}
singleFall_results_df <- do.call(rbind, singleFall_results_list)
singleFall_results_df

#LASSO Regression Feature Selection

baseline_raw_regression_df$Falls..Y.N. <- as.numeric(baseline_raw_regression_df$Falls..Y.N.)
X <- as.matrix(baseline_raw_regression_df[, -which(names(baseline_raw_regression_df) == "Falls..Y.N.")])
y <- baseline_raw_regression_df$Falls..Y.N.
lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
selected_features <- coef(lasso_model, s = "lambda.min")[-1, ]
selected_features <- names(selected_features[selected_features != 0])
print(selected_features)

# Multivariate Regression with selected Features
FeatureSelected_baseline_raw_regression_df = baseline_raw_regression_df[,c(selected_features, "Falls..Y.N.")]
FeatureSelected_baseline_raw_regression_df
FeatureSelected_logit_model <- glm(Falls..Y.N. ~., data = FeatureSelected_baseline_raw_regression_df, family = binomial)
FeatureSelected_predicted_probs <- predict(FeatureSelected_logit_model, newdata = FeatureSelected_baseline_raw_regression_df, type = "response")
FeatureSelected_roc_curve <- roc(FeatureSelected_baseline_raw_regression_df$Falls..Y.N., FeatureSelected_predicted_probs)
plot(FeatureSelected_roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
FeatureSelected_auc_value <- auc(FeatureSelected_roc_curve)
text(0.8, 0.2, paste("AUC =", round(FeatureSelected_auc_value, 2)), col = "black", cex = 1.5)

FeatureSelected_coefficients <- coef(FeatureSelected_logit_model)
FeatureSelected_feature_names <- names(FeatureSelected_coefficients)[-1]
FeatureSelected_coef_df <- data.frame(feature = FeatureSelected_feature_names, coefficient = FeatureSelected_coefficients[-1])
FeatureSelected_coef_df <- FeatureSelected_coef_df[order(abs(FeatureSelected_coef_df$coefficient), decreasing = TRUE), ]
ggplot(FeatureSelected_coef_df, aes(x = reorder(feature, coefficient), y = coefficient, fill = coefficient > 0)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(title = "Feature Importance Plot for Logistic Regression",
       x = "Coefficient Value",
       y = "Feature") +
  theme_minimal()
summary(FeatureSelected_logit_model)


# Pre vs Post Analysis

all_NPH_df = read_excel(excel_file, sheet = "NPH- all")[,c(36:51)]
pre_NPH_df = all_NPH_df[, c(1:8)]
post_NPH_df = all_NPH_df[, c(9:16)]

colnames(pre_NPH_df) = pre_NPH_df[1,]
colnames(post_NPH_df) = post_NPH_df[1,]
pre_NPH_df = pre_NPH_df[-1,]
post_NPH_df = post_NPH_df[-1,]
colnames(post_NPH_df) <- paste("post", colnames(post_NPH_df), sep = "_")

# Convert pre_post_TUG to numeric
pre_post_TUG <- data.frame(na.omit(cbind(pre_NPH_df$TUG, post_NPH_df$post_TUG)))
pre_post_TUG <- apply(pre_post_TUG, 2, function(x) as.numeric(as.character(x)))
pre_post_TUG = data.frame(pre_post_TUG)
# Convert pre_post_10MW to numeric
pre_post_10MW <- data.frame(na.omit(cbind(pre_NPH_df$"10MW", post_NPH_df$post_10MW)))
pre_post_10MW <- apply(pre_post_10MW, 2, function(x) as.numeric(as.character(x)))
pre_post_10MW = data.frame(pre_post_10MW)
# Convert pre_post_30S2S to numeric
pre_post_30S2S <- data.frame(na.omit(cbind(pre_NPH_df$"30 STS", post_NPH_df$"post_30 STS")))
pre_post_30S2S <- apply(pre_post_30S2S, 2, function(x) as.numeric(as.character(x)))
pre_post_30S2S = data.frame(pre_post_30S2S)
# Convert pre_post_2MW to numeric
pre_post_2MW <- data.frame(na.omit(cbind(pre_NPH_df$'2MW', post_NPH_df$post_2MW)))
pre_post_2MW <- apply(pre_post_2MW, 2, function(x) as.numeric(as.character(x)))
pre_post_2MW = data.frame(pre_post_2MW)

pre_post_TUG$diff_feature <- pre_post_TUG$X2 - pre_post_TUG$X1
pre_post_10MW$diff_feature <- pre_post_10MW$X2 - pre_post_10MW$X1
pre_post_30S2S$diff_feature <- pre_post_30S2S$X2 - pre_post_30S2S$X1
pre_post_2MW$diff_feature <- pre_post_2MW$X2 - pre_post_2MW$X1

# Perform paired t-tests
t_test_TUG <- t.test(pre_post_TUG$X1, pre_post_TUG$X2, paired=TRUE)
t_test_10MW <- t.test(pre_post_10MW$X1, pre_post_10MW$X2, paired=TRUE)
t_test_30S2S <- t.test(pre_post_30S2S$X1, pre_post_30S2S$X2, paired=TRUE)
t_test_2MW <- t.test(pre_post_2MW$X1, pre_post_2MW$X2, paired=TRUE)
t_test_TUG
t_test_10MW
t_test_30S2S
t_test_2MW

t_test_diff_TUG <- t.test(pre_post_TUG$diff_feature)
t_test_diff_10MW <- t.test(pre_post_10MW$diff_feature)
t_test_diff_30S2S <- t.test(pre_post_30S2S$diff_feature)
t_test_diff_2MW <- t.test(pre_post_2MW$diff_feature)
t_test_diff_TUG
t_test_diff_10MW
t_test_diff_30S2S
t_test_diff_2MW


ggplot(pre_post_TUG, aes(x = X1, y = diff_feature)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Change between Pre and Post-procedure Measurements",
       x = "Pre- TUG",
       y = "Change")
ggplot(pre_post_10MW, aes(x = X1, y = diff_feature)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Change between Pre and Post-procedure Measurements",
       x = "Pre- 10MW",
       y = "Change")
ggplot(pre_post_30S2S, aes(x = X1, y = diff_feature)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Change between Pre and Post-procedure Measurements",
       x = "Pre- S2S",
       y = "Change")
ggplot(pre_post_2MW, aes(x = X1, y = diff_feature)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Change between Pre and Post-procedure Measurements",
       x = "Pre- 2MW",
       y = "Change")


TUG_Density <- data.frame(Measure = c(rep("Pre", length(pre_post_TUG$X1)), rep("Post", length(pre_post_TUG$X2))),
                 Value = c(pre_post_TUG$X1, pre_post_TUG$X2))
ggplot(TUG_Density, aes(x = Value, fill = Measure)) +
  geom_density(alpha = 0.5) +
  labs(title = "Effect of Operation on Measurement",
       x = "TUG",
       y = "Density",
       fill = "Procedure")+
  xlim(0, 200)

MW10_Density <- data.frame(Measure = c(rep("Pre", length(pre_post_10MW$X1)), rep("Post", length(pre_post_10MW$X2))),
                          Value = c(pre_post_10MW$X1, pre_post_10MW$X2))
ggplot(MW10_Density, aes(x = Value, fill = Measure)) +
  geom_density(alpha = 0.5) +
  labs(title = "Effect of Operation on Measurement",
       x = "10MW",
       y = "Density",
       fill = "Procedure")+
  xlim(0, 100)

S2S_Density <- data.frame(Measure = c(rep("Pre", length(pre_post_30S2S$X1)), rep("Post", length(pre_post_30S2S$X2))),
                           Value = c(pre_post_30S2S$X1, pre_post_30S2S$X2))
ggplot(S2S_Density, aes(x = Value, fill = Measure)) +
  geom_density(alpha = 0.5) +
  labs(title = "Effect of Operation on Measurement",
       x = "S2S",
       y = "Density",
       fill = "Procedure")


MW2_Density <- data.frame(Measure = c(rep("Pre", length(pre_post_2MW$X1)), rep("Post", length(pre_post_2MW$X2))),
                          Value = c(pre_post_2MW$X1, pre_post_2MW$X2))
ggplot(MW2_Density, aes(x = Value, fill = Measure)) +
  geom_density(alpha = 0.5) +
  labs(title = "Effect of Operation on Measurement",
       x = "2MW",
       y = "Density",
       fill = "Procedure")





