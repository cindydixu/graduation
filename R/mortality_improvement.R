#' mortality_improvement
#'
#' This function loads claims table with mortality actual amount larger than zero,
#' and then calculate improvement factors for each of claims.
#' Finally claims table with calculated improvement factors.
#'
#' @param claims claims table with mortality actual amount larger than zero
#' @param vbt_year The year of vbt table
#' @param data_vbt The month of vbt table
#' @param factor The factor is applied to the mortality improvement rates
#' @return claims table with calculated improvement factors
#' @export

mortality_improvement <- function(claims, vbt_year, vbt_month, factor){
  claims_2 <- claims %>% select(keys, ATTAINED_AGE_AT_POLICY_ANNIV, INSURED_GENDER, AS_OF_MNTH, AS_OF_YR, ISSUE_MONTH)
  claims_2$pre_vbt <- ifelse(claims_2$AS_OF_YR < vbt_year, "Y", ifelse(claims_2$AS_OF_YR == vbt_year & claims_2$AS_OF_MNTH < vbt_month, "Y", "N"))
  claims_3 <- list()
  for (i in c(1:nrow(claims_2))) {
    print(i)
    if (claims_2$pre_vbt[i] == "N"){
      claim <- claims_2[i,]
      payment_date <- as.yearmon(paste(claim$AS_OF_YR, claim$AS_OF_MNTH), "%Y %m")
      end_period_year <- ifelse(claim$AS_OF_MNTH >= claim$ISSUE_MONTH, claim$AS_OF_YR, claim$AS_OF_YR - 1)
      end_period_date <- as.yearmon(paste(end_period_year, claim$ISSUE_MONTH), "%Y %m")
      vbt_date <- as.yearmon(paste(vbt_year, vbt_month), "%Y %m")
      start_period_year <- ifelse(vbt_month <= claim$ISSUE_MONTH, vbt_year, vbt_year + 1)
      start_period_date <- as.yearmon(paste(start_period_year, claim$ISSUE_MONTH), "%Y %m")
      if (end_period_date <= vbt_date){
        attained_age <- claim$ATTAINED_AGE_AT_POLICY_ANNIV
        start_date <- vbt_date
        end_date <- payment_date
      } else{
        attained_age <- claim$ATTAINED_AGE_AT_POLICY_ANNIV
        start_date <- end_period_date
        end_date <- payment_date
        year <- end_period_year
        while (year != start_period_year){
          attained_age_next <- attained_age[length(attained_age)] - 1
          attained_age <- c(attained_age, attained_age_next)
          end_date_next <- start_date[length(start_date)]
          end_date <- c(end_date, end_date_next)
          year <- year - 1
          start_date_next <- as.yearmon(paste(year, claim$ISSUE_MONTH), "%Y %m")
          start_date <- c(start_date, start_date_next)
        }
        attained_age_next <- attained_age[length(attained_age)] - 1
        attained_age <- c(attained_age, attained_age_next)
        end_date <- c(end_date, start_period_date)
        start_date <- c(start_date, vbt_date)
      }
      claim_data <- data.frame(attained_age, start_date, end_date)
      claim_data$duration <- (claim_data$end_date - claim_data$start_date) * 12
      claim_data$gender <- claim$INSURED_GENDER
      claim_data <- merge(claim_data, improvement_factor, by = c("attained_age", "gender"), all.x = TRUE)
      claim_data$calculated_improvement_factor <- (1 - claim_data$Improvement_factor / 24)^claim_data$duration
      claim$total_improvement_factor <- 1 / prod(claim_data$calculated_improvement_factor)
    } else{
      claim <- claims_2[i,]
      payment_date <- as.yearmon(paste(claim$AS_OF_YR, claim$AS_OF_MNTH), "%Y %m")
      end_period_year <- ifelse(claim$AS_OF_MNTH <= claim$ISSUE_MONTH, claim$AS_OF_YR, claim$AS_OF_YR + 1)
      end_period_date <- as.yearmon(paste(end_period_year, claim$ISSUE_MONTH), "%Y %m")
      vbt_date <- as.yearmon(paste(vbt_year, vbt_month), "%Y %m")
      start_period_year <- ifelse(vbt_month >= claim$ISSUE_MONTH, vbt_year, vbt_year - 1)
      start_period_date <- as.yearmon(paste(start_period_year, claim$ISSUE_MONTH), "%Y %m")
      if(end_period_date >= vbt_date){
        attained_age <- claim$ATTAINED_AGE_AT_POLICY_ANNIV
        end_date <- vbt_date
        start_date <- payment_date
      } else{
        attained_age <- claim$ATTAINED_AGE_AT_POLICY_ANNIV
        end_date <- end_period_date
        start_date <- payment_date
        year <- end_period_year
        while (year != start_period_year){
          attained_age_next <- attained_age[length(attained_age)] + 1
          attained_age <- c(attained_age, attained_age_next)
          start_date_next <- end_date[length(end_date)]
          start_date <- c(start_date, start_date_next)
          year <- year + 1
          end_date_next <- as.yearmon(paste(year, claim$ISSUE_MONTH), "%Y %m")
          end_date <- c(end_date, end_date_next)
        }
        attained_age_next <- attained_age[length(attained_age)] + 1
        attained_age <- c(attained_age, attained_age_next)
        end_date <- c(end_date, vbt_date)
        start_date <- c(start_date, start_period_date)
      }
      claim_data <- data.frame(attained_age, start_date, end_date)
      claim_data$duration <- (claim_data$end_date - claim_data$start_date) * 12
      claim_data$gender <- claim$INSURED_GENDER
      claim_data <- merge(claim_data, improvement_factor, by = c("attained_age", "gender"), all.x = TRUE)
      claim_data$calculated_improvement_factor <- (1 - claim_data$Improvement_factor / 12 * factor)^claim_data$duration
      claim$total_improvement_factor <- prod(claim_data$calculated_improvement_factor)
    }
    claims_3[[i]] <- claim
  }
  claims_3 <- do.call(rbind, claims_3)
  claims_3
}
