#' Calculate categorical statistics in related to a threshold
#'
#' @description Calculate traditional statistics related to a threshold
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
#' @param lim limit for x and y
#' @param verbose display additional information
#' @param ... arguments passed to plot
#'
#' @return a data.frame including: Accuracy (A); Critical Success Index (CSI); Probability of Detection (POD); Bias(B); False Alarm Ratio (FAR); Heidke Skill Score (HSS); Pearce skill Score (PSS) in %. The number of valid observations (n), average of observations (Obs) and model (Mod) and the used threshold (thr) are also included for additional information.
#'
#' @references
#' Gandin, L. S., & Murphy, A. H. (1992). Equitable skill scores for categorical forecasts. Monthly weather review, 120(2), 361-370.
#'
#' @export
#'
#' @examples
#' data <- 0.02 * 1:100
#' set.seed(666)
#' model  <- abs(rnorm(100,0.01))
#'
#' oldpar <- par(pty="s")
#' cate(model = model, observation = data, threshold = 1,
#'      to.plot = TRUE, rname = 'example')
#' par(oldpar)
#'

cate <- function(model, observation, threshold,
                 cutoff = NA, nobs = 8,
                 rname, to.plot = FALSE,
                 col = '#4444bb', pch = 19,
                 lty = 3,lcol = '#333333',lim,
                 verbose = TRUE , ...){

  if(length(model) != length(observation))
    stop("model and observation need to have the same length!") # nocov

  NA_mod <- is.na(model)
  NA_obs <- is.na(observation)

  model       <- model[      !NA_obs & !NA_mod]
  observation <- observation[!NA_obs & !NA_mod]

  if(!is.na(cutoff[1])){
    if(verbose) cat('using',cutoff[1],'for min cutoff\n')

    model        <- model[observation >= cutoff[1]]
    observation  <- observation[observation >= cutoff[1]]
    if(verbose) cat(length(model),'values left\n')
  }
  if(length(cutoff)>1){
    cat('using',cutoff[2],'for max cutoff\n')

    model       <- model[observation < cutoff[2]]
    observation <- observation[observation < cutoff[2]]
    if(verbose) cat(length(model),'values left\n')
  }

  if(to.plot){

    if(missing(lim))
      lim <- range(observation,model,threshold, na.rm = TRUE)
    if(!missing(rname)){
      plot(observation,model, col = col, pch = pch, main = rname,
           xlim = lim, ylim = lim, asp = 1, ...)
    }else{
      plot(observation,model, col = col, pch = pch,
           xlim = lim, ylim = lim, asp = 1, ...)
    }
    min_all <- min(observation, model,threshold, na.rm = TRUE)
    max_all <- max(observation, model,threshold, na.rm = TRUE)
    delta   <- 0.1*(max_all - min_all)
    lines(x = c(min_all-delta,max_all+delta),
          y = c(threshold,threshold),
          col = lcol,lty = lty)
    lines(x = c(threshold,threshold),
          y = c(min_all-delta,max_all+delta),
          col = lcol,lty = lty)
  }

  if(length(model) < nobs){
    table_stats     <- cate(NA,NA,1,nobs = 0)              # nocov
    table_stats$n   = length(model)                        # nocov
    table_stats$Obs = mean(observation, na.rm = TRUE)      # nocov
    table_stats$Sim = mean(model, na.rm = TRUE)            # nocov
  }else{
    a = sum(model >  threshold & observation <= threshold) # -1 # to match cascade script
    b = sum(model >  threshold & observation >  threshold) # -1 # to match cascade script
    c = sum(model <= threshold & observation <= threshold) # -1 # to match cascade script
    d = sum(model <= threshold & observation >  threshold) # -1 # to match cascade script

    table_stats <- as.data.frame(cbind(n    = length(observation),
                                       Obs  = mean(observation, na.rm = TRUE),
                                       Sim  = mean(model, na.rm = TRUE),
                                       thr  = threshold,
                                       A   	= 100 * (b + c) / (a+b+c+d),
                                       CSI  = 100 * b / (a+b+d),
                                       POD  = 100 * b / (b+d),
                                       B	  = 100 * (a+b) / (b+d),
                                       FAR	= 100 * a / (a+b),
                                       HSS  = 200 * (b*c-a*d) / ((b+d)*(c+d)+(a+b)*(a+c)),
                                       PSS  = 100 * (b*c - a*d) / ((a+c) * (b+d))
    ))

    if(is.nan(table_stats$HSS))
      table_stats$HSS = 0 # nocov
    if(is.nan(table_stats$PSS))
      table_stats$PSS = 0 # nocov
  }

  if(!missing(rname))
    row.names(table_stats) <- rname

  return(table_stats)
}
