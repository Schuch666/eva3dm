#' Calculate evaluation statistics from numerical vectors
#'
#' @description Calculate statistical indexes (Number of pairs, observation average, model average, correlation, Index Of Agreement, Factor of 2, Root Mean Square Error, Mean Bias, Mean error, Normalized Mean Bias, and Normalized Mean Bias) for model evaluation
#'
#' @param model numeric vector with paired model data
#' @param observation numeric vector with paired observation data
#' @param wd logical, set true to apply a rotation on wind direction, see notes
#' @param cutoff (optionally the maximum) valid value for observation
#' @param cutoff_NME (optionally the maximum) valid value for observation for NME, MFB and MFE
#' @param nobs minimum number of observations
#' @param rname row name
#' @param verbose display additional information
#'
#' @note the option wd = TRUE applies a rotation of 360 on model wind direction to minimize the angular difference.
#'
#' @return data.frame
#'
#' @references
#' Emery, C. and Tai., E. 2001. Enhanced Meteorological Modeling and Performance Evaluation
#' for Two Texas Ozone Episodes.
#'
#' Monk, K. et al. 2019. Evaluation of Regional Air Quality Models over Sydney and Australia:
#' Part 1—Meteorological Model Comparison. Atmosphere 10(7), p. 374. doi: 10.3390/atmos10070374.
#'
#' Ramboll. 2018. PacWest Newport Meteorological Performance Evaluation.
#'
#' Zhang, Y. et al. 2019. Multiscale Applications of Two Online-Coupled Meteorology-Chemistry
#' Models during Recent Field Campaigns in Australia, Part I: Model Description and WRF/Chem-ROMS
#' Evaluation Using Surface and Satellite Data and Sensitivity to Spatial Grid Resolutions.
#' Atmosphere 10(4), p. 189. doi: 10.3390/atmos10040189.
#'
#' Emery, C., Liu, Z., Russell, A.G., Odman, M.T., Yarwood, G. and Kumar, N. 2017.
#' Recommendations on statistics and benchmarks to assess photochemical model performance.
#' Journal of the Air & Waste Management Association 67(5), pp. 582–598.
#' doi: 10.1080/10962247.2016.1265027.
#'
#' Zhai, H., Huang, L., Emery, C., Zhang, X., Wang, Y., Yarwood, G., ... & Li, L. (2024).
#' Recommendations on benchmarks for photochemical air quality model applications
#' in China—NO2, SO2, CO and PM10. Atmospheric Environment, 319, 120290.
#'
#' @export
#'
#' @examples
#' model <- 1:100
#' data  <- model + rnorm(100,0.2)
#' stat(model = model, observation = data)
#'

stat <- function(model, observation,
                 wd = FALSE, cutoff = NA, cutoff_NME = NA, nobs = 8,
                 rname, verbose = TRUE){

  IOA <- function(sim,obs){
    Om          <- mean(obs, na.rm=TRUE)
    denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^2 )

    if (denominator != 0) {
      d <- 1 - ( sum( (obs - sim)^2 ) / denominator )
    } else {
      d <- NA # nocov
      warning("'sum((abs(sim-Om)+abs(obs-Om))^2)=0', it is not possible to compute 'IoA'") # nocov
    }
    return(d)
  }

  FA2 <- function(mod,obs) {
    ratio <- mod / obs
    ratio <- na.omit(ratio)
    len   <- length(ratio)
    if (len > 0) {
      res <- length(which(ratio >= 0.5 & ratio <= 2)) / len
    } else {
      res <- NA # nocov
    }
    return(res)
  }

  correlation <- function(x,y){
    r <- sum((x - sum(x) / length(x)) * (y - sum(y) / length(y))) /
      sqrt(sum((x - sum(x) / length(x))^2) * sum((y - sum(y) / length(y))^2))
    return(r)
  }

  MFBE_cutoff <- function(mo,ob,nobs,cutoff = cutoff_NME){

    if(!is.na(cutoff[1])){
      cat('using',cutoff[1],'for min NME cutoff\n')

      mo  <- mo[ob >= cutoff[1]]
      ob  <- ob[ob >= cutoff[1]]

      cat(length(mo),'values left\n')

      if(length(mo) < nobs){
        RESULT <- stat((1:19)/10,(1:19)/10) # nocov
        RESULT$n = 0                        # nocov
        return(RESULT)                      # nocov
      }
    }
    if(length(cutoff)>1){
      cat('using',cutoff[2],'for max NME cutoff\n')

      mo  <- mo[ob < cutoff[2]]
      ob  <- ob[ob < cutoff[2]]

      cat(length(mo),'values left\n')
      if(length(mo) < nobs){
        RESULT <- stat((1:19)/10,(1:19)/10) # nocov
        RESULT$n = 0                        # nocov
        return(RESULT)                      # nocov
      }
    }
    Obs	  = mean(observation, na.rm = TRUE)
    MAGE	= mean(abs(model-observation))
    NME	  = 100 * MAGE / Obs
    return(NME)
  }

  wind_direction <- function(obs,mod, verbose = F){
    for(i in 1:length(mod)){
      diff_value <- mod[i] - obs[i]
      if(abs(diff_value)>180){
        temp_value <- 360 - abs(diff_value)
        if(diff_value < 0){
          previous <- mod[i]
          mod[i] <- obs[i] + temp_value
          if(verbose)
            cat(paste("model WD was changed from",previous,"to", mod[i],'\n')) # nocov
        } else {
          previous <- mod[i]
          mod[i] <- obs[i] - temp_value
          if(verbose)
            cat(paste("model WD was changed from",previous,"to", mod[i],'\n')) # nocov
        }
      }
    }
    return(mod)
  }

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
      RESULT <- stat((1:19)/10,(1:19)/10) # nocov
      RESULT$n = 0                        # nocov
      return(RESULT)                      # nocov
    }
  }
  if(length(cutoff)>1){
    cat('using',cutoff[2],'for max cutoff\n')

    model       <- model[observation < cutoff[2]]
    observation <- observation[observation < cutoff[2]]

    cat(length(model),'values left\n')
    if(length(model) < nobs){
      RESULT <- stat((1:19)/10,(1:19)/10) # nocov
      RESULT$n = 0                        # nocov
      return(RESULT)                      # nocov
    }
  }

  if(wd){
    model <- wind_direction(obs = observation, mod = model)
  }

  Obs	  = mean(observation, na.rm = TRUE)
  Mod	  = mean(model, na.rm = TRUE)
  MB	  = mean(model-observation)
  MAGE	= mean(abs(model-observation))
  MNB	  = mean((model-observation)/observation)
  MNGE	= mean(abs(model-observation)/observation)
  NMB	  = 100 * (Mod-Obs)/Obs
  s     = model + observation
  # MFB   = 200 * mean(model/s - observation/s )
  # MFE   = 200 * mean(abs( model/s - observation/s ))

  if(is.na(cutoff_NME[[1]])){
    NME	  = 100 * MAGE / Obs
  }else{
    NME = MFBE_cutoff(mo     = model,
                      ob     = observation,
                      nobs   = nobs,
                      cutoff = cutoff_NME)
  }

  table_stats <- as.data.frame(cbind(n    = length(observation),
                                     Obs  = Obs,
                                     Sim  = Mod,
                                     r    = correlation(model,observation),
                                     IOA  = IOA(model,observation),
                                     FA2  = FA2(model,observation),
                                     RMSE = sqrt(mean((observation - model)^2)),
                                     MB   = MB,
                                     ME   = MAGE,
                                     # GE   = MAGE,
                                     # `MFB (%)` = MFB,
                                     # `MFE (%)` = MFE,
                                     `NMB (%)` = NMB,
                                     `NME (%)` = NME))

  if(!missing(rname))
    row.names(table_stats) <- rname

  return(table_stats)
}
