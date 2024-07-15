#' Calculate evaluation statistics from numerical vectors
#'
#' @description Calculate statistical indexes (Number of pairs, observation average, model average, correlation, Index Of Agreement, Factor of 2, Root Mean Square Error, Mean Bias, Mean error, Normalized Mean Bias, and Normalized Mean Bias) for model evaluation
#'
#' @param model numeric vector with paired model data
#' @param observation numeric vector with paired observation data
#' @param threshold reference value
#' @param cutoff (optionally the maximum) valid value for observation
#' @param nobs minimum number of observations
#' @param rname row name
#' @param to.plot TRUE to plot a scatterplot
#' @param verbose display additional information
#' @param ...
#'
#' @note the option wd = TRUE apply a rotation of 360 on model wind direction to minimize the angular difference.
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#' model <- 0.02 * 1:100
#' data  <- abs(rnorm(100,0.01))
#' cate(model = model, observation = data, threshold = 1,
#'      to.plot = TRUE, rname = 'example')
#'

cate <- function(model, observation, threshold,
                 cutoff = NA, nobs = 8,
                 rname, to.plot = F, verbose = T, ...){

  if(length(model) != length(observation))
    stop("model and observation need to have the same length!") # nocov

  NA_mod <- is.na(model)
  NA_obs <- is.na(observation)

  model       <- model[      !NA_obs & !NA_mod]
  observation <- observation[!NA_obs & !NA_mod]

  if(!is.na(cutoff[1])){
    cat('using',cutoff[1],'for min cutoff\n')

    model        <- model[observation >= cutoff[1]]
    observation  <- observation[observation >= cutoff[1]]

    cat(length(model),'values left\n')

    if(length(model) < nobs){
      RESULT <- stat((1:199)/100,(1:199)/100)
      RESULT$n = 0
      return(RESULT)
    }
  }
  if(length(cutoff)>1){
    cat('using',cutoff[2],'for max cutoff\n')

    model       <- model[observation < cutoff[2]]
    observation <- observation[observation < cutoff[2]]

    cat(length(model),'values left\n')
    if(length(model) < nobs){
      RESULT <- stat((1:199)/100,(1:199)/100)
      RESULT$n = 0
      return(RESULT)
    }
  }

  if(to.plot){
    if(!missing(rname)){
      plot(observation,model, col = 'blue', pch = 19, main = rname, ...)
    }else{
      plot(observation,model, col = 'blue', pch = 19, ...)
    }
    abline(h = threshold, col = 'gray',lty = 3)
    abline(v = threshold, col = 'gray',lty = 3)
  }

  a     = sum(model >  threshold & observation <= threshold)
  b     = sum(model >  threshold & observation >  threshold)
  c     = sum(model <= threshold & observation <= threshold)
  d     = sum(model <= threshold & observation >  threshold)

  table_stats <- as.data.frame(cbind(n    = length(observation),
                                     Obs  = mean(observation, na.rm = TRUE),
                                     Sim  = mean(model, na.rm = TRUE),
                                     Acc	= (b + c) / (a+b+c+d),
                                     CSI  = b / (a+b+d),
                                     POD  = (a+b) / (b+b),
                                     Bias	= (a+b) / (b+d),
                                     FAR	= a / (a+b)
                                     ))

  if(!missing(rname))
    row.names(table_stats) <- rname
  return(table_stats)
}
