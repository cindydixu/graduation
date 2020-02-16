#' wear_off
#'
#' This function loads transformed mortality table,
#' and then find out the wear off sequence of mortality rates given an insure age,
#' an age_basis, a gender, and a tobacco_status
#'
#' @param table transformed mortality table
#' @param age_basis age basis which can be "ANB" or "ALB"
#' @param gender gender which can be "Male" or "Female"
#' @param tobacco_status tobacco_status which can be "Nontobacco" or "Tobacco"
#' @param insure_age insure_age which can be from 18 to 95

#' @return an vector of wear off mortality rates
#' @export

wear_off <- function(table, age_basis, gender, tobacco_status, insure_age){
  duration <- 1
  mortality_rate <- c()
  i <- 1
  while (duration <= 26 & insure_age >= 18){
    mortality_rate[i] <- table$mortality_rate[table$age_basis == age_basis & table$gender == gender & table$tobacco_status == tobacco_status & table$insure_age == insure_age & table$duration == duration]
    i <- i + 1
    duration <- duration + 1
    insure_age <- insure_age - 1
  }
  mortality_rate
}

