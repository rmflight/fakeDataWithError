#' add uniform noise
#'
#' @param n_rep number of replicates
#' @param value the value to use
#' @param sd the error to add
#' @param use_zero should stuff be reported using zero (used when doing two types of noise)
#'
#' @export
#' @return matrix
add_uniform_noise <- function(n_rep, value, sd, use_zero = FALSE){
  n_value <- length(value)

  n_sd <- n_rep * n_value

  out_sd <- rnorm(n_sd, 0, sd)
  out_sd <- matrix(out_sd, nrow = n_value, ncol = n_rep)

  if (!use_zero){
    tmp_value <- matrix(value, nrow = n_value, ncol = n_rep, byrow = FALSE)
    out_value <- tmp_value + out_sd
  } else {
    out_value <- out_sd
  }

  return(out_value)
}

#' add proportional noise
#'
#' @param n_rep number of replicates
#' @param value the values to add error to
#' @param sd the noise
#' @param use_zero should values be returned with zeros
#'
#' @export
#' @return matrix
add_prop_noise <- function(n_rep, value, sd, use_zero = FALSE){
  n_value <- length(value)

  out_value <- matrix(0, length(value), n_rep)
  if (use_zero){
    use_value <- rep(0, n_value)
  } else {
    use_value <- value
  }

  for (i_value in seq(1, n_value)){
    out_value[i_value, ] <- rnorm(n_rep, use_value[i_value], value[i_value]*sd)
  }

  return(out_value)
}


#' add additive and proportional noise
#'
#' @param n_rep number of replicates
#' @param value the initial values
#' @param u_sd uniform or additive error
#' @param p_sd proportional error
#'
#' @export
#' @return matrix
add_prop_uniform <- function(n_rep, value, u_sd, p_sd){
  u_noise <- add_uniform_noise(n_rep, value, u_sd, use_zero = TRUE)
  p_noise <- add_prop_noise(n_rep, value, p_sd, use_zero = TRUE)

  tmp_value <- matrix(value, length(value), n_rep, byrow = FALSE)
  out_value <- tmp_value + u_noise + p_noise

  return(out_value)
}
