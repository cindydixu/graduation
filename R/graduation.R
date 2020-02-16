#' graduation
#'
#' This function loads raw mortality tables in select and ultimate periods,
#' and then do the graudation based on the trends in the standard table.
#' Finally output graduated mortality table
#'
#' @param data_select raw mortality table in select period
#' @param data_ultimate raw mortality table in ultimate period
#' @param data_vbt standard mortality table
#' @return graduated mortality table
#' @export

graduation <- function(data_select, data_ultimate, data_vbt){
  data_select <- data_select %>% arrange(INSURED_GENDER, TOBACCO_CLS, POLICY_COVERAGE_YR, INSURED_ISSUE_AGE)
  data_ultimate <- data_ultimate %>% arrange(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV)
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
  reference_data <- data_ultimate$graduated_final_rate
  final_data <- list()
  for (duration in 25:1){
    print(duration)
    data <- data_select[data_select$POLICY_COVERAGE_YR == duration,]
    data <- data %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE)
    target_vbt <- data_vbt_select[data_vbt_select$POLICY_COVERAGE_YR == duration,]$expected_mortality_rate_amount
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
  data_vbt_ult <- data_vbt %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR > 25) %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, ATTAINED_AGE_AT_POLICY_ANNIV)
  data_vbt_sel <- data_vbt %>% filter(INSURED_ISSUE_AGE >= 18 & POLICY_COVERAGE_YR <= 25) %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, ATTAINED_AGE_AT_POLICY_ANNIV)
  final_data_ultimate_2 <- final_data_ultimate %>% select(INSURED_GENDER, TOBACCO_CLS, ATTAINED_AGE_AT_POLICY_ANNIV, graduated_final_rate, credibility_amount)
  final_data_select_2 <- final_data_select %>% select(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, graduated_final_rate, credibility_amount)
  data_vbt_ult_2 <- left_join(data_vbt_ult, final_data_ultimate_2, by = c('INSURED_GENDER', 'TOBACCO_CLS', 'ATTAINED_AGE_AT_POLICY_ANNIV'))
  data_vbt_sel_2 <- left_join(data_vbt_sel, final_data_select_2, by = c('INSURED_GENDER', 'TOBACCO_CLS', 'INSURED_ISSUE_AGE', 'POLICY_COVERAGE_YR'))
  data_vbt_graduated <- rbind(data_vbt_ult_2, data_vbt_sel_2)
  data_vbt_graduated <- data_vbt_graduated %>% arrange(INSURED_GENDER, TOBACCO_CLS, INSURED_ISSUE_AGE, POLICY_COVERAGE_YR, ATTAINED_AGE_AT_POLICY_ANNIV)
  data_vbt_graduated$expected_mortality_rate_amount <- NULL
  data_vbt_graduated
}