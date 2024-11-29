tis_by_class <- function(df, class) {
  class_col <- paste0("class_", tolower(class))
  
  df <- df %>%
    mutate(
      # Step 1: Calculate prescribed daily dose
      dose1 = ifelse(!is.na(strength1), strength1 * rxamt / rxsup, NA),
      dose2 = ifelse(!is.na(strength2), strength2 * rxamt / rxsup, NA),
      dose3 = ifelse(!is.na(strength3), strength3 * rxamt / rxsup, NA),
      
      # Step 2: Calculate TIS score using the prescribed daily dose and maximum daily dose
      score1 = ifelse(!is.na(dose1) & !is.na(maxdose1), dose1 / maxdose1, NA),
      score2 = ifelse(!is.na(dose2) & !is.na(maxdose2), dose2 / maxdose2, NA),
      score3 = ifelse(!is.na(dose3) & !is.na(maxdose3), dose3 / maxdose3, NA),
      
      # Step 3: Calculate final TIS for this class
      TIS = case_when(
        !is.na(score1) & is.na(score2) & is.na(score3) ~ round(score1, 2),
        !is.na(score1) & !is.na(score2) & is.na(score3) ~ round(score1 + score2, 2),
        !is.na(score1) & !is.na(score2) & !is.na(score3) ~ round(score1 + score2 + score3, 2),
        TRUE ~ 0
      )
    )
  
  # Update the TIS column for the specified class
  df <- df %>%
    mutate(!!paste0("TIS_", tolower(class)) := ifelse(df[[class_col]] == 1, TIS, NA)) %>%
    select(-TIS)  # Remove the temporary TIS column
  
  return(df)
}
