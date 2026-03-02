#' Calculate Pro-rated PHQ-9 Total Score
#'
#' @description
#' Calculates the total PHQ-9 score with pro-rating for missing items. 
#' If 1 or 2 items are missing, the score is pro-rated: (Sum of valid items / N valid items) * 9.
#' If 3 or more items are missing, the total score is set to NA.
#'
#' @param item_scores A numeric vector of the 9 PHQ-9 item scores (0-3).
#' @return A numeric value representing the total score, or NA.
#' @export
#'
#' @examples
#' phq9_total_score(c(1, 2, 0, 1, NA, 3, 2, 1, 0)) # Pro-rates
#' phq9_total_score(c(1, 2, 0, NA, NA, NA, 2, 1, 0)) # Returns NA
phq9_total_score <- function(item_scores) {
  if (length(item_scores) != 9) {
    warning("PHQ-9 requires exactly 9 items.")
    return(NA_real_)
  }
  
  n_missing <- sum(is.na(item_scores))
  n_valid <- 9 - n_missing
  
  if (n_missing >= 3) {
    return(NA_real_)
  } else if (n_missing > 0) {
    # Pro-rate and round to nearest integer 
    return(round((sum(item_scores, na.rm = TRUE) / n_valid) * 9))
  } else {
    return(sum(item_scores))
  }
}
