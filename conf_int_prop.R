#' Confidence Interval
#'
#' @param x Response Variable
#'
#' @return tibble with the mean, standard deviation, lower and upper bounds
#' @export
#'
#' @examples
conf_int_prop <- function(x){
  y <- sum(na.omit(x)=='1')
  # find n
  n <- length(x)
  # find proportion of malignant masses
  p <- y/n
  # Find interval bounds
  lower <- p-qnorm(0.975)*sqrt(p*(1-p)/n)
  upper <- p+qnorm(0.975)*sqrt(p*(1-p)/n)
  upper <- ifelse(upper > 1, 1, upper)
  # Return confidence interval upper and lower bounds
  return(data.frame(MalignantCases = y, TotalCases = n, Lower = lower,Proportion = p,Upper = upper))
}

