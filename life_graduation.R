library(tidyverse)
library(reshape2)
library(RODBC)
library(gridExtra)
library(xlsx)
library(rlang)
library(limSolve)
library(rpart)
library(partykit)
library(plotly)
library(zoo)
library(maditr)

load("data_iiesp_2.Rda")

data_copy_2 <- data

data <- data_copy_2

# data$MORTALITY_ACTUAL_AMT <- ifelse(data$MORTALITY_ACTUAL_AMT > 1814000, 1814000, data$MORTALITY_ACTUAL_AMT)

# data <- data[data$MORTALITY_ACTUAL_AMT <= 5000000,]

data_copy <- data

data <- data_copy

data <- data %>% filter(POLICY_COVERAGE_YR > 25 & INSURED_ISSUE_AGE >= 18)

data_3 <- data

data_3$B_h_i <- data_3$MORTALITY_EXPECTED_AMT * data_3$AS_OF_TOTAL_FACE_AMT

data_3$C_h_i <- (data_3$MORTALITY_EXPECTED_AMT)^2

data_7 <- data_3 %>% group_by(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV) %>%
  summarize(actual_mortality_rate_amount = sum(MORTALITY_ACTUAL_AMT)/sum(MORTALITY_EXPOSURE_AMT),
            expected_mortality_rate_amount = sum(MORTALITY_EXPECTED_AMT)/sum(MORTALITY_EXPOSURE_AMT),
            A_E_ratio = sum(MORTALITY_ACTUAL_AMT) / sum(MORTALITY_EXPECTED_AMT))

######################

data_8_4 <- list()
data_11 <- list()
l <- 1
m <- 0
for (i in c('Female', 'Male')){
  for (j in c('NONTOBACCO', 'TOBACCO')){
    # data_7_a <- data_7 %>% filter(INSURED_GENDER == i & TOBACCO_CLS == j & ATTAINED_AGE_AT_POLICY_ANNIV <= 111 & A_E_ratio == 0)
    # data_7_a$attained_age_group <- 0
    
    data_8 <- data_7 %>% filter(INSURED_GENDER == i & TOBACCO_CLS == j & ATTAINED_AGE_AT_POLICY_ANNIV <= 111)
    
    cluster_vars <- data_8$expected_mortality_rate_amount
    nstart.val <- 20
    set.seed(123)
    km1 <- kmeans(cluster_vars,centers=1,nstart=nstart.val)
    km2 <- kmeans(cluster_vars,centers=2,nstart=nstart.val)
    km3 <- kmeans(cluster_vars,centers=3,nstart=nstart.val)
    km4 <- kmeans(cluster_vars,centers=4,nstart=nstart.val)
    km5 <- kmeans(cluster_vars,centers=5,nstart=nstart.val)
    km6 <- kmeans(cluster_vars,centers=6,nstart=nstart.val)
    km7 <- kmeans(cluster_vars,centers=7,nstart=nstart.val)
    km8 <- kmeans(cluster_vars,centers=8,nstart=nstart.val)
    km9 <- kmeans(cluster_vars,centers=9,nstart=nstart.val)
    km10 <- kmeans(cluster_vars,centers=10,nstart=nstart.val)
    km11 <- kmeans(cluster_vars,centers=11,nstart=nstart.val)
    km12 <- kmeans(cluster_vars,centers=12,nstart=nstart.val)
    
    var.exp <- data.frame(k = c(1:12),
                          bss_tss = c(km1$betweenss/km1$totss,
                                      km2$betweenss/km2$totss,
                                      km3$betweenss/km3$totss,
                                      km4$betweenss/km4$totss,
                                      km5$betweenss/km5$totss,
                                      km6$betweenss/km6$totss,
                                      km7$betweenss/km7$totss,
                                      km8$betweenss/km8$totss,
                                      km9$betweenss/km9$totss,
                                      km10$betweenss/km10$totss,
                                      km11$betweenss/km11$totss,
                                      km12$betweenss/km12$totss))
    
    print(ggplot(var.exp,aes(x=k,y=bss_tss))+ geom_point() + ggtitle(paste0(i, "_", j)))
    
    data_8$attained_age_group <- km5$cluster
    
    # data_8 <- rbind(data_8, data_7_a)
    
    data_8_2 <- data_8[c('ATTAINED_AGE_AT_POLICY_ANNIV', 'attained_age_group')]
    
    data_8_3 <- data_8[c('INSURED_GENDER', 'TOBACCO_CLS', 'ATTAINED_AGE_AT_POLICY_ANNIV', 'attained_age_group')]
    
    data_8_4[[l]] <- data_8_3
    
    data_9 <- data_3 %>% filter(INSURED_GENDER == i & TOBACCO_CLS == j & ATTAINED_AGE_AT_POLICY_ANNIV <= 111)
    
    data_9 <- left_join(data_9, data_8_2, by = 'ATTAINED_AGE_AT_POLICY_ANNIV')
    
    # mu <- sum(data_9$MORTALITY_ACTUAL_AMT)/sum(data_9$MORTALITY_EXPECTED_AMT)
    # 
    # T <- sum(data_9$MORTALITY_EXPECTED_AMT)
    
    data_10 <- data_9 %>% group_by(INSURED_GENDER, TOBACCO_CLS, attained_age_group) %>% 
      summarize(E_h = sum(MORTALITY_EXPECTED_AMT),
                m_h = sum(MORTALITY_ACTUAL_AMT) / sum(MORTALITY_EXPECTED_AMT),
                B_h = sum(B_h_i),
                C_h = sum(C_h_i),
                number_of_claims = sum(MORTALITY_ACTUAL_AMT > 0),
                number_of_records = sum(MORTALITY_ACTUAL_AMT >= 0))
    
    # sigma_square_nominator <- sum(data_10$E_h * ((data_10$m_h - mu)^2)) - mu * (sum(data_10$B_h / data_10$E_h) - sum(data_10$B_h) / T) + mu^2 * (sum(data_10$C_h / data_10$E_h) - sum(data_10$C_h) / T)
    # 
    # sigma_square_denominator <- T - sum((data_10$E_h)^2) / T - sum(data_10$C_h / data_10$E_h) + sum(data_10$C_h) / T
    # 
    # sigma_square <- sigma_square_nominator / sigma_square_denominator
    
    # data_10$credibility_amount <- data_10$E_h / (data_10$E_h + mu * data_10$B_h / sigma_square / data_10$E_h - (mu^2 + sigma_square) * data_10$C_h / sigma_square / data_10$E_h)
    
    data_10$credibility_amount <- data_10$E_h / (data_10$E_h + (1.09 * data_10$B_h - 1.204 * data_10$C_h) / (0.019604 * data_10$E_h))
    
    data_10$credibility_amount <- ifelse(data_10$credibility_amount > 1, 1, data_10$credibility_amount)
    
    data_10$final_ratio <- data_10$m_h * data_10$credibility_amount + (1 - data_10$credibility_amount)
    
    data_11[[l]] <- data_10
    
    l <- l + 1
  }
}

data_8_4 <- do.call(rbind, data_8_4)

data_11 <- do.call(rbind, data_11)

write.csv(data_11,  file = "final_data_kmeans_bm_grouped.csv", row.names = FALSE, na = '')

##############

data_5 <- read.csv('2015VBT.csv', stringsAsFactors = FALSE)

data_6 <- data_5 %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR > 25) %>% group_by(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV) %>% summarize(expected_mortality_rate_amount = mean(expected_mortality_rate_amount))

data_6_2 <- left_join(data_6, data_8_4, by = c("INSURED_GENDER", "TOBACCO_CLS", "ATTAINED_AGE_AT_POLICY_ANNIV"))

data_13 <- data_11

data_14 <- left_join(data_6_2, data_13, by = c("INSURED_GENDER", "TOBACCO_CLS", "attained_age_group"))

data_14$final_rate <- data_14$expected_mortality_rate_amount * data_14$final_ratio

data_14$final_rate <- ifelse(is.na(data_14$final_rate), data_14$expected_mortality_rate_amount, data_14$final_rate)

data_ultimate <- data_14

save(data_ultimate, file = "ungrouped_mortality_results_ultimate_4.Rda")

write.csv(data_ultimate,  file = "final_data_kmeans_bm_ungrouped.csv", row.names = FALSE, na = '')

########################################

data <- data_copy

data <- data %>% filter(POLICY_COVERAGE_YR <= 25 & INSURED_ISSUE_AGE >= 18)

data_3 <- data

data_3$B_h_i <- data_3$MORTALITY_EXPECTED_AMT * data_3$AS_OF_TOTAL_FACE_AMT

data_3$C_h_i <- (data_3$MORTALITY_EXPECTED_AMT)^2

data_3$duration_group <- ifelse(data_3$POLICY_COVERAGE_YR <= 5, '01-05',
                                ifelse(data_3$POLICY_COVERAGE_YR <= 10, '06-10', 
                                       ifelse(data_3$POLICY_COVERAGE_YR <= 15, '11-15',
                                              ifelse(data_3$POLICY_COVERAGE_YR <= 20, '16-20', 
                                                     ifelse(data_3$POLICY_COVERAGE_YR <= 25, '21-25', 'ultimate')))))

data_10_2_a <- list()
l <- 1
m <- 0
for (i in c('Female', 'Male')){
  for (j in c('NONTOBACCO', 'TOBACCO')){
    data_9 <- data_3 %>% filter(INSURED_GENDER == i & TOBACCO_CLS == j & INSURED_ISSUE_AGE <= 95)
    
    # mu <- sum(data_9$MORTALITY_ACTUAL_AMT)/sum(data_9$MORTALITY_EXPECTED_AMT)
    
    data_10 <- data_9 %>% group_by(INSURED_GENDER, TOBACCO_CLS, duration_group) %>% 
      summarize(E_h = sum(MORTALITY_EXPECTED_AMT),
                m_h = sum(MORTALITY_ACTUAL_AMT) / sum(MORTALITY_EXPECTED_AMT),
                B_h = sum(B_h_i),
                C_h = sum(C_h_i),
                number_of_claims = sum(MORTALITY_ACTUAL_AMT > 0),
                number_of_records = sum(MORTALITY_ACTUAL_AMT >= 0))
    
    data_10$credibility_amount <- data_10$E_h / (data_10$E_h + (1.09 * data_10$B_h - 1.204 * data_10$C_h) / (0.019604 * data_10$E_h))
    
    data_10$credibility_amount <- ifelse(data_10$credibility_amount > 1, 1, data_10$credibility_amount)
    
    data_10$final_ratio <- data_10$m_h * data_10$credibility_amount + (1 - data_10$credibility_amount)
    
    data_10_2_a[[l]] <- data_10
    l <- l + 1
  }
}

data_10_2 <- do.call(rbind, data_10_2_a)

data_10_2 <- data_10_2 %>% filter(final_ratio != 0)

write.csv(data_10_2,  file = "final_data_dgroup_bm_grouped.csv", row.names = FALSE, na = '')

data_5 <- read.csv('2015VBT.csv', stringsAsFactors = FALSE)

data_6 <- data_5 %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR <= 25)

data_6$duration_group <- ifelse(data_6$POLICY_COVERAGE_YR <= 5, '01-05',
                                ifelse(data_6$POLICY_COVERAGE_YR <= 10, '06-10', 
                                       ifelse(data_6$POLICY_COVERAGE_YR <= 15, '11-15',
                                              ifelse(data_6$POLICY_COVERAGE_YR <= 20, '16-20', 
                                                     ifelse(data_6$POLICY_COVERAGE_YR <= 25, '21-25', 'ultimate')))))

data_10_4 <- left_join(data_6, data_10_2, by = c("INSURED_GENDER", "TOBACCO_CLS", "duration_group"))

data_10_4$final_rate <- data_10_4$expected_mortality_rate_amount * data_10_4$final_ratio

data_10_4$final_rate <- ifelse(is.na(data_10_4$final_rate), data_10_4$expected_mortality_rate_amount, data_10_4$final_rate)

data_10_4$credibility_amount <- ifelse(is.na(data_10_4$credibility_amount), 0, data_10_4$credibility_amount)

data_select <- data_10_4

write.csv(data_select,  file = "final_data_dgroup_bm_ungrouped.csv", row.names = FALSE, na = '')

####################################
# load("ungrouped_mortality_results_ultimate_4.Rda")
save(data_ultimate, file = "data_1.Rda")
save(data_select, file = "data_2.Rda")

data_select <- data_select %>% arrange(INSURED_GENDER, TOBACCO_CLS, POLICY_COVERAGE_YR, INSURED_ISSUE_AGE)
data_ultimate <- data_ultimate %>% arrange(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV)

data_vbt <- read.csv('2015VBT.csv', stringsAsFactors = FALSE)
data_vbt_ultimate <- data_vbt %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR > 25) %>% group_by(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV) %>% summarize(expected_mortality_rate_amount = mean(expected_mortality_rate_amount))
data_vbt_select <- data_vbt %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR <= 25)
data_vbt_select <- data_vbt_select %>% arrange(INSURED_GENDER, TOBACCO_CLS, POLICY_COVERAGE_YR, INSURED_ISSUE_AGE)
data_vbt_ultimate <- data_vbt_ultimate %>% arrange(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV)

target_vbt <- data_vbt_ultimate$expected_mortality_rate_amount

target_vector <- data_ultimate$final_rate

n <- length(target_vector)

target_matrix <- diag(n)

weights_vector <- NULL

length_group <- 78
i = 1
sign_vbt <- sign(diff(target_vbt[(length_group*i - length_group + 1):(length_group*i)]))
sign_vbt[sign_vbt == 0] <- NA
sign_vbt <- na.locf(sign_vbt, na.rm = FALSE)
sign_vbt <- na.locf(sign_vbt, na.rm = FALSE, fromLast = TRUE)
sign_vbt[length_group] <- 0
sign_vbt_all <- sign_vbt
for (i in 2:4){
  sign_vbt <- sign(diff(target_vbt[(length_group*i - length_group + 1):(length_group*i)]))
  sign_vbt[sign_vbt == 0] <- NA
  sign_vbt <- na.locf(sign_vbt, na.rm = FALSE)
  sign_vbt <- na.locf(sign_vbt, na.rm = FALSE, fromLast = TRUE)
  sign_vbt[length_group] <- 0
  sign_vbt_all <- c(sign_vbt_all, sign_vbt)
}

temp_00 = c( -1, 1, rep( 0, n-2 ) )
inequality_constraints_matrix_00 = sign_vbt_all[1] * t(temp_00)
for( j in 1:(n-2) ){
  temp_00 = c( 0, temp_00[ 1:(n-1) ] )
  inequality_constraints_matrix_00 = rbind( inequality_constraints_matrix_00, sign_vbt_all[ j + 1 ]*temp_00 )
}
rownames(inequality_constraints_matrix_00) <- c(1:nrow(inequality_constraints_matrix_00))
inequality_constraints_matrix_00 <- inequality_constraints_matrix_00[-(length_group * (1:3)),]

temp_01 = c( -1, rep( 0, (length_group-1) ), 1, rep( 0, (n-length_group-1) ))
inequality_constraints_matrix_01 = t( temp_01 )
for( j in 1:(n-length_group-1) ){
  temp_01 = c( 0, temp_01[ 1:(n-1) ] )
  inequality_constraints_matrix_01 = rbind( inequality_constraints_matrix_01, temp_01 )
}
rownames(inequality_constraints_matrix_01) <- c(1:nrow(inequality_constraints_matrix_01))
inequality_constraints_matrix_01 <- inequality_constraints_matrix_01[-((length_group+1):(length_group*2)),]

temp_02 = c( -1, rep( 0, (length_group*2-1) ), 1, rep( 0, n-length_group*2-1 ))
inequality_constraints_matrix_02 = t( temp_02 )
for( j in 1:(n-length_group*2-1) ){
  temp_02 = c( 0, temp_02[ 1:(n-1) ] )
  inequality_constraints_matrix_02 = rbind( inequality_constraints_matrix_02, temp_02 )
}
rownames(inequality_constraints_matrix_02) <- c(1:nrow(inequality_constraints_matrix_02))

inequality_constraints_matrix <- rbind( inequality_constraints_matrix_00 , inequality_constraints_matrix_01 , inequality_constraints_matrix_02 )

inequality_constraints_vector = rep( 0, nrow( inequality_constraints_matrix ) )

data_ultimate$graduated_final_rate = lsei(	A = target_matrix,
                                           B = target_vector,
                                           E = matrix( nrow = 1, ncol = n, 0 ),
                                           F = 0,
                                           G = inequality_constraints_matrix,
                                           H = inequality_constraints_vector,
                                           Wa = weights_vector,
                                           type = 1, verbose = TRUE)$X

#################################

reference_data <- data_ultimate$graduated_final_rate

final_data <- list()

for (duration in 25:1){
  print(duration)
  data <- data_select[data_select$POLICY_COVERAGE_YR == duration,]
  data <- data %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE) %>% select(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, expected_mortality_rate_amount, number_of_claims, number_of_records, credibility_amount, final_rate)
  target_vbt <- data$expected_mortality_rate_amount
  target_vector <- data$final_rate
  n <- length(target_vector)
  target_matrix <- diag(n)
  weights_vector <- NULL
  
  length_group <- 78
  i = 1
  sign_vbt <- sign(diff(target_vbt[(length_group*i - length_group + 1):(length_group*i)]))
  sign_vbt[sign_vbt == 0] <- NA
  sign_vbt <- na.locf(sign_vbt, na.rm = FALSE)
  sign_vbt <- na.locf(sign_vbt, na.rm = FALSE, fromLast = TRUE)
  sign_vbt[length_group] <- 0
  sign_vbt_all <- sign_vbt
  for (i in 2:4){
    sign_vbt <- sign(diff(target_vbt[(length_group*i - length_group + 1):(length_group*i)]))
    sign_vbt[sign_vbt == 0] <- NA
    sign_vbt <- na.locf(sign_vbt, na.rm = FALSE)
    sign_vbt <- na.locf(sign_vbt, na.rm = FALSE, fromLast = TRUE)
    sign_vbt[length_group] <- 0
    sign_vbt_all <- c(sign_vbt_all, sign_vbt)
  }
  
  temp_00 = c( -1, 1, rep( 0, n-2 ) )
  inequality_constraints_matrix_00 = sign_vbt_all[1] * t(temp_00)
  for( j in 1:(n-2) ){
    temp_00 = c( 0, temp_00[ 1:(n-1) ] )
    inequality_constraints_matrix_00 = rbind( inequality_constraints_matrix_00, sign_vbt_all[ j + 1 ]*temp_00 )
  }
  rownames(inequality_constraints_matrix_00) <- c(1:nrow(inequality_constraints_matrix_00))
  inequality_constraints_matrix_00 <- inequality_constraints_matrix_00[-(length_group * (1:3)),]
  
  temp_01 = c( -1, rep( 0, (length_group-1) ), 1, rep( 0, (n-length_group-1) ))
  inequality_constraints_matrix_01 = t( temp_01 )
  for( j in 1:(n-length_group-1) ){
    temp_01 = c( 0, temp_01[ 1:(n-1) ] )
    inequality_constraints_matrix_01 = rbind( inequality_constraints_matrix_01, temp_01 )
  }
  rownames(inequality_constraints_matrix_01) <- c(1:nrow(inequality_constraints_matrix_01))
  inequality_constraints_matrix_01 <- inequality_constraints_matrix_01[-((length_group+1):(length_group*2)),]
  
  temp_02 = c( -1, rep( 0, (length_group*2-1) ), 1, rep( 0, n-length_group*2-1 ))
  inequality_constraints_matrix_02 = t( temp_02 )
  for( j in 1:(n-length_group*2-1) ){
    temp_02 = c( 0, temp_02[ 1:(n-1) ] )
    inequality_constraints_matrix_02 = rbind( inequality_constraints_matrix_02, temp_02 )
  }
  rownames(inequality_constraints_matrix_02) <- c(1:nrow(inequality_constraints_matrix_02))
  
  inequality_constraints_matrix <- rbind( inequality_constraints_matrix_00 , inequality_constraints_matrix_01 , inequality_constraints_matrix_02 )
  
  inequality_constraints_vector = rep( 0, nrow( inequality_constraints_matrix ) )
  
  inequality_constraints_matrix_03 <- -diag(n)
  
  inequality_constraints_vector_03 <- -reference_data
  
  inequality_constraints_matrix_04 <- -diag(n)
  
  inequality_constraints_matrix_04 <- inequality_constraints_matrix_04[-1,]
  
  inequality_constraints_vector_04 <- -reference_data
  
  inequality_constraints_vector_04 <- inequality_constraints_vector_04[-length(inequality_constraints_vector_04)]
  
  inequality_constraints_matrix <- rbind(inequality_constraints_matrix, inequality_constraints_matrix_03, inequality_constraints_matrix_04)
  
  inequality_constraints_vector <- c(inequality_constraints_vector, inequality_constraints_vector_03, inequality_constraints_vector_04)
  
  data$graduated_final_rate = lsei(	A = target_matrix,
                                    B = target_vector,
                                    E = matrix( nrow = 1, ncol = n, 0 ),
                                    F = 0,
                                    G = inequality_constraints_matrix,
                                    H = inequality_constraints_vector,
                                    Wa = weights_vector,
                                    type = 1, verbose = TRUE)$X
  
  reference_data <- data$graduated_final_rate
  
  final_data[[duration]] <- data
}

final_data_select <- do.call(rbind, final_data)

data_ultimate$graduated_final_rate <- round(data_ultimate$graduated_final_rate, 5)

final_data_ultimate <- data_ultimate

final_data_select$graduated_final_rate <- round(final_data_select$graduated_final_rate, 5)

final_data_select$difference_ratio <- final_data_select$graduated_final_rate / final_data_select$final_rate - 1

final_data_ultimate$difference_ratio <- final_data_ultimate$graduated_final_rate / final_data_ultimate$final_rate - 1

final_data_select$difference <- final_data_select$graduated_final_rate - final_data_select$final_rate

final_data_ultimate$difference <- final_data_ultimate$graduated_final_rate - final_data_ultimate$final_rate

write.csv(final_data_select,  file = "final_data_bm_ungrouped_select.csv", row.names = FALSE, na = '')

write.csv(final_data_ultimate,  file = "final_data_bm_ungrouped_ultimate.csv", row.names = FALSE, na = '')

########################################################################################################

final_data_select <- final_data_select %>% rename(VBT = expected_mortality_rate_amount, claim = number_of_claims, record = number_of_records, cred = credibility_amount, diff_ratio = difference_ratio, final = graduated_final_rate, diff = difference)

final_data_ultimate <- final_data_ultimate %>% rename(VBT = expected_mortality_rate_amount, claim = number_of_claims, record = number_of_records, cred = credibility_amount, diff_ratio = difference_ratio, final = graduated_final_rate, diff = difference)

####################################

data_vbt_ult <- data_vbt %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR > 25) %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, ATTAINED_AGE_AT_POLICY_ANNIV)

data_vbt_sel <- data_vbt %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR <= 25) %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, ATTAINED_AGE_AT_POLICY_ANNIV)

final_data_ultimate_2 <- final_data_ultimate %>% select(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV, final, cred, claim, record)

final_data_select_2 <- final_data_select %>% select(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, final, cred, claim, record)

data_vbt_ult_2 <- left_join(data_vbt_ult, final_data_ultimate_2, by = c('INSURED_GENDER', 'TOBACCO_CLS', 'ATTAINED_AGE_AT_POLICY_ANNIV'))

data_vbt_sel_2 <- left_join(data_vbt_sel, final_data_select_2, by = c('INSURED_GENDER', 'TOBACCO_CLS', 'INSURED_ISSUE_AGE', 'POLICY_COVERAGE_YR'))

data_vbt_graduated <- rbind(data_vbt_ult_2, data_vbt_sel_2)

data_vbt_graduated <- data_vbt_graduated %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, ATTAINED_AGE_AT_POLICY_ANNIV)

data_vbt_graduated$expected_mortality_rate_amount <- NULL

####################################
data <- data_copy

data_2 <- left_join(data, data_vbt_graduated, by = c('INSURED_GENDER', 'TOBACCO_CLS', 'INSURED_ISSUE_AGE', 'POLICY_COVERAGE_YR', 'ATTAINED_AGE_AT_POLICY_ANNIV')) %>% filter(INSURED_ISSUE_AGE >= 18)

data_2 <- data_2 %>% select(-c(MORTALITY_ACTUAL_CNT, MORTALITY_EXPECTED_CNT, MORTALITY_EXPOSURE_CNT, MORTALITY_VARIANCE_CNT))

data_2$new_expected_mortality_amount <- data_2$MORTALITY_EXPOSURE_AMT * data_2$final

data_2$new_variance_mortality_amount <- (data_2$new_expected_mortality_amount / data_2$AS_OF_TOTAL_FACE_AMT) * (1 - data_2$new_expected_mortality_amount / data_2$AS_OF_TOTAL_FACE_AMT) * (data_2$AS_OF_TOTAL_FACE_AMT^2)

data_2$duration <- ifelse(data_2$POLICY_COVERAGE_YR <= 25, data_2$POLICY_COVERAGE_YR, 26)

data_2$pre_2000 <- ifelse(data_2$ISSUE_YEAR < 2000, "pre_2000", "post_2000")

data_2$duration_group <- ifelse(data_2$duration <= 5, '01-05',
                                ifelse(data_2$duration <= 10, '06-10', 
                                       ifelse(data_2$duration <= 15, '11-15',
                                              ifelse(data_2$duration <= 20, '16-20', 
                                                     ifelse(data_2$duration <= 25, '21-25', 'ultimate')))))
wd <- getwd()
issue_age_group <- read.csv(file = paste0(wd, "/issue_age.csv"), stringsAsFactors = FALSE)
attained_age_group <- read.csv(file = paste0(wd, "/attained_age.csv"), stringsAsFactors = FALSE)

data_2 <- left_join(data_2, issue_age_group, by = 'INSURED_ISSUE_AGE')
data_2 <- left_join(data_2, attained_age_group, by = 'ATTAINED_AGE_AT_POLICY_ANNIV')

data_2 <- data_2 %>% select(-PLAN_CD, -POLICY_NO, -COMPANY_NO, -BASE_PLAN_IND, -NUM_OF_NONTOBACCO_CLS, -NUM_OF_TOBACCO_CLS, -CURRENT_LEVEL_TERM, -UNISEX_IND, -SPECIAL_ISSUE_ID, -PREMIUM_MODE, -DEATH_BENEFIT_OPTION, -COVERAGE_CD, -CLIENT_ID, -XF_PLAN)

data_3 <- data_2 %>% group_by(duration) %>% summarize(
  AE_ratio =  sum(MORTALITY_ACTUAL_AMT)/sum(MORTALITY_EXPECTED_AMT),
  new_AE_ratio = sum(MORTALITY_ACTUAL_AMT)/sum(new_expected_mortality_amount),
  number_of_claims = sum(MORTALITY_ACTUAL_AMT > 0),
  number_of_records = sum(MORTALITY_ACTUAL_AMT >= 0),
  average_credibility = mean(cred),
  average_claim = mean(claim),
  average_record = mean(record))

data_4 <- data_2 %>% summarize(
  AE_ratio = sum(MORTALITY_ACTUAL_AMT)/sum(MORTALITY_EXPECTED_AMT),
  new_AE_ratio = sum(MORTALITY_ACTUAL_AMT)/sum(new_expected_mortality_amount),
  number_of_claims = sum(MORTALITY_ACTUAL_AMT > 0),
  number_of_records = sum(MORTALITY_ACTUAL_AMT >= 0),
  average_credibility = mean(cred),
  average_claim = mean(claim),
  average_record = mean(record))

data_5 <- data_2 %>% group_by(duration_group) %>% summarize(
  AE_ratio = sum(MORTALITY_ACTUAL_AMT)/sum(MORTALITY_EXPECTED_AMT),
  new_AE_ratio = sum(MORTALITY_ACTUAL_AMT)/sum(new_expected_mortality_amount),
  number_of_claims = sum(MORTALITY_ACTUAL_AMT > 0),
  number_of_records = sum(MORTALITY_ACTUAL_AMT >= 0),
  average_credibility = mean(cred),
  average_claim = mean(claim),
  average_record = mean(record))

data_2$MORTALITY_ACTUAL_AMT <- as.character(data_2$MORTALITY_ACTUAL_AMT)

write.csv(data_2,  file = "final_data_dgroup_bm_mu.csv", row.names = FALSE, na = '')
