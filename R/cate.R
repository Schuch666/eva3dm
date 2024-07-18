#' Calculate categorical statistics from numerical vectors in related to a threshold
#'
#' @description Calculate traditional statistics from two numerical vectors in related to a threshold
#'
#' @param model numeric vector with paired model data
#' @param observation numeric vector with paired observation data
#' @param threshold reference value
#' @param cutoff (optionally the maximum) valid value for observation
#' @param nobs minimum number of observations
#' @param rname row name
#' @param to.plot TRUE to plot a scatter-plot
#' @param col color for points
#' @param pch pch of points
#' @param lty lty of threshold lines
#' @param lcol col of threshold lines
#' @param verbose display additional information
#' @param ... arguments passed to plot
#'
#' @return a data.frame including: Accuracy (A); Critical Success Index (CSI); Probability of Detection (POD); Bias(B); False Alarm Ratio (FAR); Heidke Skill Score (HSS); Pearce skill Score (PSS) in %.
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
                 rname, to.plot = F, col = '#4444bb', pch = 19,
                 lty = 3,lcol = '#333333',
                 verbose = T, ...){

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
  }
  if(length(cutoff)>1){
    cat('using',cutoff[2],'for max cutoff\n')

    model       <- model[observation < cutoff[2]]
    observation <- observation[observation < cutoff[2]]
    cat(length(model),'values left\n')
  }

  if(length(model) < nobs){
    warning('Valid number of observatios lesseer than nobs')
    RESULT     <- cate(NA,NA,1,nobs = 0)
    RESULT$n   = length(model)
    RESULT$Obs = mean(observation, na.rm = TRUE)
    RESULT$Sim = mean(model, na.rm = TRUE)
  }

  if(to.plot){
    range_all <- range(observation,model, na.rm = TRUE)
    if(!missing(rname)){
      plot(observation,model, col = col, pch = pch, main = rname,
           xlim = range_all, ylim = range_all, ...)
    }else{
      plot(observation,model, col = col, pch = pch,
           xlim = range_all, ylim = range_all, ...)
    }
    min_all <- min(observation, model, na.rm = TRUE)
    max_all <- max(observation, model, na.rm = TRUE)
    delta   <- 0.1*(max_all - min_all)
    lines(x = c(min_all-delta,max_all+delta),
          y = c(threshold,threshold),
          col = lcol,lty = lty)
    lines(x = c(threshold,threshold),
          y = c(min_all-delta,max_all+delta),
          col = lcol,lty = lty)
  }

  a = sum(model >  threshold & observation <= threshold)
  b = sum(model >  threshold & observation >  threshold)
  c = sum(model <= threshold & observation <= threshold)
  d = sum(model <= threshold & observation >  threshold)

  table_stats <- as.data.frame(cbind(n    = length(observation),
                                     Obs  = mean(observation, na.rm = TRUE),
                                     Sim  = mean(model, na.rm = TRUE),
                                     A   	= 100 * (b + c) / (a+b+c+d),
                                     CSI  = 100 * b / (a+b+d),
                                     POD  = 100 * b / (b+d),
                                     B	  = 100 * (a+b) / (b+d),
                                     FAR	= 100 * a / (a+b),
                                     HSS  = 200 * (b*c-a*d) / ((b+d)*(c+d)+(a+b)*(a+c)),
                                     PSS  = 100 * (b*c - a*d) / ((a+c) * (b+d))
                                     ))

  if(is.nan(table_stats$HSS))
    table_stats$HSS = 0
  if(is.nan(table_stats$PSS))
    table_stats$PSS = 0

  if(!missing(rname))
    row.names(table_stats) <- rname

  return(table_stats)
}
