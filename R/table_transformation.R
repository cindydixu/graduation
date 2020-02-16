#' table_transformation
#'
#' This function loads mortality table downloaded from SOA website,
#' and then transfer it to a long table ready to analyze
#'
#' @param table mortality table downloaded from SOA website
#' @return long mortality table ready to analyze
#' @export

table_transformation <- function(table){
  table_new <- list()
  i <- 1
  for (name in names(table)){
    class <- substr(name, 6,8)
    age_basis <- substr(name, 10,12)
    gender <- ifelse(substr(class, 1, 1) == "M", "Male", "Female")
    tobacco_status <- ifelse(substr(class, 2, 3) == "SM", "Tobacco", "Nontobacco")
    subtable <- table[[name]]
    subtable_new_all <- list()
    j <- 1
    for (k in c(min(subtable$Iss..Age):max(subtable$Iss..Age))){
      insure_age <- k
      duration <- c(1:(120+1-insure_age))
      mortality_rate <- c(as.numeric(subtable[subtable$Iss..Age == insure_age, c(2:26)]), as.numeric(subtable[subtable$Iss..Age >= insure_age, "Ult."]))
      subtable_new <- data.frame(duration, mortality_rate)
      subtable_new$insure_age <- insure_age
      subtable_new$tobacco_status <- tobacco_status
      subtable_new$gender <- gender
      subtable_new$age_basis <- age_basis
      subtable_new <- subtable_new[c("age_basis", "gender", "tobacco_status", "insure_age", "duration", "mortality_rate")]
      subtable_new_all[[j]] <- subtable_new
      j <- j + 1
    }
    table_new[[i]] <- do.call(rbind, subtable_new_all)
    i <- i + 1
  }
  table_new <- do.call(rbind, table_new)
  table_new
}
  

