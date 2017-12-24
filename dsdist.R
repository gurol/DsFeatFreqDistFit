#' @author Gurol Canbek, \email{gurol44@@gmail.com}
#' @references \url{http://malwarehouse.net}
#' @keywords distributions, log-normal, power law, Poisson, exponential, data sets, feature frequency
#' @title MalWareHouse - R Scripts for Distribution Fitting Testing
#' @date December 2017
#' @version 1.0
#' @note version history
#' December 2017
#' 1.0 The first version
#' @description R scripts for comprehensive set of distribution testing to fit
#' power law, log-normal, exponential, and Poisson statistical distribution
#' into the feature frequency distributions (the truth).

#' libraries
#' 
#' 
library(poweRlaw)
library(ggplot2)
library(magrittr)
library(dplyr)

source('utils.R', chdir=TRUE)

# Distribution colors
#               Power-law  Log-Normal Exponent   Poisson
cols_dist <- c('#fb8072', '#7f4e2C', '#80b1d3', '#49b960')
col_distpl <- 1
col_distln <- 2
col_distex <- 3
col_distpo <- 4

# df_ds_feat_space_names <- rclip()
# df_ds_feat_freqs <- rclip()
# df_ds_sample_sizes <- rclip(header=FALSE)
# ignore warning message: incomplete final line found by readTableHeader on 'pbpaste'
#
# Suggested plot file naming schema
# 6 inch x 8 inch
# Benign_DS5_VuongTest_fitted_powerlaw_vs_lognormal
# Benign_DS5_VuongTest_fitted_powerlaw_vs_poisson
# Benign_DS5_VuongTest_fitted_powerlaw_vs_exponential
# Benign_DS5_VuongTest_powerlaw_vs_fitted_lognormal
# Benign_DS5_fitted_powerlaw_against_others
# Benign_DS5_fitted_lognormal_against_others
# Benign_DS5_bootstrapt_powerlaw_fit
# Benign_DS5_bootstrapt_lognormal_fit
# Benign_DS5_LogLikelihoodRatioDistribution_fitted_powerlaw_vs_lognormal
# Benign_DS5_LogLikelihoodRatioDistribution_powerlaw_vs_fitted_lognormal
#
# save(df_ds_feat_freqs, df_ds_feat_space_names, df_ds_sample_sizes, file='Malign.RData')
# testPowerLawLogNormalHypotheses('Benign', df_ds_feat_freqs, df_ds_feat_space_names, df_ds_sample_sizes, no_sim_count=50)
testPowerLawLogNormalHypotheses<-function(
  class_name, df_ds_feat_freqs, df_ds_feat_space_names,
  df_ds_sample_sizes=NULL, no_sim_count=NULL)
{
  # Dimensions check
  stopifnot(nrow(df_ds_feat_freqs) == nrow(df_ds_feat_space_names))
  stopifnot(ncol(df_ds_feat_freqs) == ncol(df_ds_feat_space_names))
  stopifnot(ncol(df_ds_feat_freqs) == ncol(df_ds_sample_sizes))
  stopifnot(ncol(df_ds_feat_space_names) == ncol(df_ds_sample_sizes))
  
  for (ds in 1:length(df_ds_feat_freqs)){
    feat_freqs_or_counts <- df_ds_feat_freqs[[ds]]
    feat_freqs_or_counts <- feat_freqs_or_counts[!is.na(feat_freqs_or_counts)]
    
    feat_space_names <- df_ds_feat_space_names[[ds]]
    feat_space_names <- feat_space_names[!is.na(feat_freqs_or_counts)]
    
    if (is.null(df_ds_sample_sizes))
      ds_sample_size <- NULL
    else
      ds_sample_size <- df_ds_sample_sizes[, ds]
    
    ds_name <- gsub('featureFreq', '', colnames(df_ds_feat_freqs)[ds])
    bs_p <- testPowerLawLogNormalHypothesis(
      ds_name, class_name, feat_space_names, feat_freqs_or_counts,
      ds_sample_size,  no_sim_count=no_sim_count)
  }
}

# df_feat_freqs <- rclip(header=FALSE)
# testPowerLawLogNormalHypothesis(rclip(header=FALSE), 264303, 'DS0 (Benign)', no_sim_count=10)
# m_pl <- displ$new(as.integer(feat_counts[[1]]))
testPowerLawLogNormalHypothesis<-function(
  ds_name, ds_class_name, ds_feat_space_names, ds_feat_freqs_or_counts,
  ds_sample_size=NULL, threads=getNumberOfCPUCores(), no_sim_count=NULL)
{
  if (all(ds_feat_freqs_or_counts == floor(ds_feat_freqs_or_counts)))
    # All elements are integer so the variable holds feature counts
    feat_counts <- ds_feat_freqs_or_counts
  else
    # Elements are fractinal convert into counts by multiplying the frequency
    # by dataset sample size
    feat_counts <- round(ds_sample_size*ds_feat_freqs_or_counts)
  
  feature_space_size <- length(feat_counts)
  
  # Determine simulation counts
  simulation_counts <- 100
  if (is.null(no_sim_count)) {
    if (ds_sample_size > simulation_counts) {
      if (ds_sample_size < 1000)
        simulation_counts <- 1000
      else if (ds_sample_size < 5000)
        simulation_counts <- ds_sample_size
      else
        simulation_counts <- 5000
    }
    # else
    #   simulation_counts <- 100
  }
  else
    simulation_counts <- no_sim_count
  
  xlabel_ds_class <- paste0(ds_name, ' (', ds_class_name, ') feature counts')
  
  # No scientific notation (e.g. 1e+05) in axis in plots
  scipen_option <- getOption('scipen')
  options(scipen=999)
  
  ############ Power-law #######################################################
  # (Discrete) Power-law fit?
  fit_power_law <- displ$new(feat_counts)
  est <- estimate_xmin(fit_power_law)
  fit_power_law$setXmin(est)
  est <- estimate_pars(fit_power_law)
  fit_power_law$setPars(est)
  
  bs_power_law <- bootstrap_p(fit_power_law, xmax = 1e+06,
                              threads=threads, no_of_sims=simulation_counts)
  
  # Power-law, bootsptrap parameters
  xmin_pl <- fit_power_law$xmin
  ntail_pl <- get_ntail(fit_power_law, prop=FALSE)
  
  # (Discrete) Log-Normal fit?
  fit_log_normal_with_pl <- dislnorm$new(feat_counts)
  # Set Xmin from power-law (they should be the same in both distributions)
  # est <- estimate_xmin(fit_log_normal)
  fit_log_normal_with_pl$setXmin(xmin_pl)
  est <- estimate_pars(fit_log_normal_with_pl)
  fit_log_normal_with_pl$setPars(est)
  
  # Vuong's test,
  # A likelihood ratio test for model selection
  # using the Kullback-Leibler criteria
  compare_dist_pl_ln <- compare_distributions(
    fit_power_law, fit_log_normal_with_pl)
  plot(compare_dist_pl_ln,
       col=ifelse(compare_dist_pl_ln$ratio$ratio < -0.001, cols_dist[col_distln],
                  ifelse(compare_dist_pl_ln$ratio$ratio > 0.001,
                         cols_dist[col_distpl], 'black')),
       main=paste0('Vuong\'s test: Power Law* vs. Log-Normal (', ds_name, ')'),
       xlab=xlabel_ds_class)
  
  # (Discrete) Poisson fit?
  fit_poisson_with_pl <- dispois$new(feat_counts)
  # Set Xmin from power-law (they should be the same in both distributions)
  # est <- estimate_xmin(fit_poisson)
  fit_poisson_with_pl$setXmin(xmin_pl)
  est <- estimate_pars(fit_poisson_with_pl)
  fit_poisson_with_pl$setPars(est)
  
  compare_dist_pl_po <- compare_distributions(
    fit_power_law, fit_poisson_with_pl)
  plot(compare_dist_pl_po,
       col=ifelse(compare_dist_pl_po$ratio$ratio < -0.001, cols_dist[col_distpo],
                  ifelse(compare_dist_pl_po$ratio$ratio > 0.001,
                         cols_dist[col_distpl], 'black')),
       main=paste0('Vuong\'s Test: Power Law* vs. Poisson (', ds_name, ')'),
       xlab=xlabel_ds_class)
  
  # (Discrete) Exponential fit?
  fit_exponential_with_pl <- disexp$new(feat_counts)
  # Set Xmin from power-law (they should be the same in both distributions)
  # est <- estimate_xmin(fit_exponential)
  fit_exponential_with_pl$setXmin(xmin_pl)
  est <- estimate_pars(fit_exponential_with_pl)
  fit_exponential_with_pl$setPars(est)
  
  compare_dist_pl_ex <- compare_distributions(
    fit_power_law, fit_exponential_with_pl)
  plot(compare_dist_pl_ex,
       col=ifelse(compare_dist_pl_ex$ratio$ratio < -0.001, cols_dist[col_distex],
                  ifelse(compare_dist_pl_ex$ratio$ratio > 0.001,
                         cols_dist[col_distpl], 'black')),
       main=paste0('Vuong\'s Test: Power Law* vs. Exponential (', ds_name, ')'),
       xlab=xlabel_ds_class)
  
  ############ Log-Normal ######################################################
  # (Discrete) Log-Normal fit ? xmin is not set from power-law
  fit_log_normal_only <- dislnorm$new(feat_counts)
  est <- estimate_xmin(fit_log_normal_only)
  fit_log_normal_only$setXmin(est)
  est <- estimate_pars(fit_log_normal_only)
  fit_log_normal_only$setPars(est)
  
  bs_log_normal <- bootstrap_p(fit_log_normal_only, xmax = 1e+06,
                               threads=threads, no_of_sims=simulation_counts)
  
  # Log-Normal, bootsptrap parameters
  xmin_ln <- fit_log_normal_only$xmin
  ntail_ln <- get_ntail(fit_log_normal_only, prop=FALSE)
  
  # (Discrete) Power-law fit?
  fit_power_law_with_ln <- displ$new(feat_counts)
  # Set Xmin from Log-Normal (they should be the same in both distributions)
  # est <- estimate_xmin(fit_power_law_with_ln)
  fit_power_law_with_ln$setXmin(xmin_ln)
  est <- estimate_pars(fit_power_law_with_ln)
  fit_power_law_with_ln$setPars(est)
  
  # Vuong's test,
  # A likelihood ratio test for model selection
  # using the Kullback-Leibler criteria
  compare_dist_pl_ln_2 <- compare_distributions(
    fit_power_law_with_ln, fit_log_normal_only)
  plot(compare_dist_pl_ln_2,
       col=ifelse(compare_dist_pl_ln_2$ratio$ratio < -0.001,
                  cols_dist[col_distln],
                  ifelse(compare_dist_pl_ln_2$ratio$ratio > 0.001,
                         cols_dist[col_distpl], 'black')),
       main=paste0('Vuong\'s Test: Power Law vs. Log-Normal* (', ds_name, ')'),
       xlab=xlabel_ds_class)
  
  # (Discrete) Poisson fit?
  fit_poisson_with_ln <- dispois$new(feat_counts)
  # Set Xmin from Log-Normal (they should be the same in both distributions)
  # est <- estimate_xmin(fit_poisson_with_ln)
  fit_poisson_with_ln$setXmin(xmin_ln)
  est <- estimate_pars(fit_poisson_with_ln)
  fit_poisson_with_ln$setPars(est)
  
  # (Discrete) Exponential fit?
  fit_exponential_with_ln <- disexp$new(feat_counts)
  # Set Xmin from Log-Normal (they should be the same in both distributions)
  # est <- estimate_xmin(fit_exponential_with_ln)
  fit_exponential_with_ln$setXmin(xmin_ln)
  est <- estimate_pars(fit_exponential_with_ln)
  fit_exponential_with_ln$setPars(est)
  
  ntail_ratio_pl <- get_ntail(fit_power_law, prop=TRUE)
  ntail_ratio_ln <- get_ntail(fit_log_normal_only, prop=TRUE)
  
  # pl* vs. ln
  if (compare_dist_pl_ln$test_statistic < -0.0001)
    # Almost equal to double tilda ~
    closer_to_the_truth_pl <- 'Log-Normal ≈ the truth'
  else if (compare_dist_pl_ln$test_statistic > 0.0001)
    closer_to_the_truth_pl <- 'no better or worse fit'
  else
    closer_to_the_truth_pl <- 'Power Law ≈ the truth'
  
  # pl vs. ln*
  if (compare_dist_pl_ln_2$test_statistic < -0.0001)
    # Almost equal to double tilda ~
    closer_to_the_truth_ln <- 'Log-Normal ≈ the truth'
  else if (compare_dist_pl_ln_2$test_statistic > 0.0001)
    closer_to_the_truth_ln <- 'no better or worse fit'
  else
    closer_to_the_truth_ln <- 'Power Law ≈ the truth'
  
  # See (Clauset, Shalizi, and Newman, 2, pp.2) for typical values
  # See (Gillespie, 2015, pp.3) for moments in continous Power Law
  if (fit_power_law$pars > 1 && fit_power_law$pars <= 2) {
    pl_alpha_type <-
      '1 < typical <= 2'
    pl_alpha_moments <- 'all moments diverge 1:µ,2:σ2,3:skewness,4:kurtosis'
  }
  else if (fit_power_law$pars > 2 && fit_power_law$pars <= 3) {
    pl_alpha_type <-
      '2 < typical <= 3'
    pl_alpha_moments <- '1:µ,2:σ2 converge but >2nd moments diverge 3:skewness,4:kurtosis'
  }
  else {
    pl_alpha_type <-
      'atypical > 3'
    pl_alpha_moments <- '1:µ,2:σ2,3:skewness converge but >α moments diverge 4:kurtosis)'
  }
  
  print(paste(
    c('Dataset', 'Class', 'Feature count min', 'Feature space size',
      # Power-Law fit
      'xmin (pl)', 'Parameters (pl)[alpha])', 'Alpha Type', 'Alpha Moments',
      'ntail (fitted feature count) (pl)', 'ntail ratio (pl)',
      # Power-Law estimation
      'Bootstrap p-value (pl)', 'Bootstrap GoF (pl)',
      'Bootstrap Simulation Count (pl)', 'Simulation Time (pl)[minute]',
      'Package Version', 'GoF Distance Measure',
      # Comparison between Power-Law and other 3 ditributions
      'Power Law* vs. Log-Normal: Test Statistics',
      'pl* vs. ln: p-value (1-sided)', 'pl* vs. ln: p-value (2-sided)',
      'pl* vs. ln: result',
      'Power Law* vs. Poisson: Test Statistics',
      'pl* vs. po: p-value (1-sided)', 'pl* vs. po: p-value (2-sided)',
      'Power Law* vs. Exponential: Test Statistics',
      'pl* vs. ex: p-value (1-sided)', 'pl* vs. ex: p-value (2-sided)',
      # Log-Normal estimation
      'xmin (ln)', 'Parameters (ln)[mean])', 'Parameters (ln)[SD])',
      'ntail (fitted feature count) (ln)', 'ntail ratio (ln)',
      'Bootstrap p-value (ln)', 'Bootstrap GoF (ln)',
      'Bootstrap Simulation Count (ln)', 'Simulation Time (ln)[minute]',
      # Comparison between Power-Law and other 3 ditributions
      'Power Law vs. Log-Normal*: Test Statistics',
      'pl vs. ln*: p-value (1-sided)', 'pl. vs. ln*: p-value (2-sided)',
      'pl vs. ln*: result\n'),
    collapse='\t'))
  print(paste(
    ds_name, ds_class_name, min(feat_counts), feature_space_size,
    xmin_pl, fit_power_law$pars, pl_alpha_type, pl_alpha_moments,
    ntail_pl, ntail_ratio_pl,
    bs_power_law$p, bs_power_law$gof,
    simulation_counts, bs_power_law$sim_time,
    bs_power_law$package_version, bs_power_law$distance,
    compare_dist_pl_ln$test_statistic,
    compare_dist_pl_ln$p_one_sided, compare_dist_pl_ln$p_two_sided,
    closer_to_the_truth_pl,
    compare_dist_pl_po$test_statistic,
    compare_dist_pl_po$p_one_sided, compare_dist_pl_po$p_two_sided,
    compare_dist_pl_ex$test_statistic,
    compare_dist_pl_ex$p_one_sided, compare_dist_pl_ex$p_two_sided,
    
    xmin_ln, fit_log_normal_only$pars[1], fit_log_normal_only$pars[2],
    ntail_ln, ntail_ratio_ln,
    bs_log_normal$p, bs_log_normal$gof,
    simulation_counts, bs_log_normal$sim_time,
    compare_dist_pl_ln_2$test_statistic,
    compare_dist_pl_ln_2$p_one_sided, compare_dist_pl_ln_2$p_two_sided,
    closer_to_the_truth_ln,
    '[Estimation, simulations, and comparisons are completed]\n', sep='\t'))
  
  # Power-Law fitted and not-fitted features dump
  cat(paste('Fitted feature names (Power Law*) [>=xmin]: ',
            paste(ds_feat_space_names[feat_counts >= xmin_pl][1:ntail_pl],
                  collapse=', '), '\n'))
  print(paste(
    'Not fitted feature names (Power Law*) [<xmin]: ',
    paste(
      ds_feat_space_names[feat_counts < xmin_pl]
      [1:(feature_space_size-ntail_pl)],
      collapse=', '), '\n'))
  
  # Log-Normal fitted and not-fitted features dump
  print(paste('Fitted feature names (Log-Normal*) [>=xmin]: ',
              paste(ds_feat_space_names[feat_counts >= xmin_ln][1:ntail_ln],
                    collapse=', '), '\n'))
  print(paste(
    'Not fitted feature names (Log-Normal*) [<xmin]: ',
    paste(
      ds_feat_space_names[feat_counts < xmin_ln][1:(feature_space_size-ntail_ln)],
      collapse=', '), '\n'))
  
  options(scipen=scipen_option)
  
  # return from the function as below for the following error
  # Error in grid.Call.graphics(C_setviewport, vp, TRUE) : 
  #   non-finite location and/or size for viewport
  # Called from: grid.Call.graphics(C_setviewport, vp, TRUE)
  # return(list(bs_power_law, bs_log_normal))
  
  plot(fit_power_law,
       xlab=xlabel_ds_class,
       ylab='Rank',
       main=paste0(
         'Power Law against other fits for feature distribution of ',
         ds_name, ' (', ds_class_name, ')')
       # sub =  '[Power-Law Fit]')
  )
  lines(fit_power_law,
        col=cols_dist[col_distpl], lty=1, lwd=4)
  lines(fit_log_normal_with_pl,
        col=cols_dist[col_distln], lty=2, lwd=2)
  lines(fit_exponential_with_pl,
        col=cols_dist[col_distex], lty=3, lwd=2)
  lines(fit_poisson_with_pl,
        col=cols_dist[col_distpo], lty=4, lwd=2)
  abline(v=xmin_pl, lty=3)
  mtext(paste('α:', round(fit_power_law$pars, digits=2),
              paste0(' where ', pl_alpha_type,
                     ' \n(', pl_alpha_moments, ')'),
              '\nVuong\'s test statistics (pl* vs. ln):',
              round(compare_dist_pl_ln$test_statistic, digits=3),
              paste0('\n(', closer_to_the_truth_pl, ')'),
              '\np (1-sided):',
              round(compare_dist_pl_ln$p_one_sided, digits=2),
              '\np (2-sided):',
              round(compare_dist_pl_ln$p_two_sided, digits=2), ' '),
        adj=1, line=-6*0.75, side=3, cex=0.75)
  mtext(paste0('Xmin:', xmin_pl, ', ntail ratio (X>=Xmin):',
               round(ntail_ratio_pl, digits=2)),
        adj=1-ntail_ratio_pl, side=3, line=0, cex=0.75)
  legend(x='bottomleft', box.lty=0, col=cols_dist, lty=1:4, lwd=2,
         legend=c('Power Law*', 'Log-Normal', 'Exponential', 'Poisson'))
  
  plot(fit_log_normal_only,
       xlab=xlabel_ds_class,
       ylab='Rank',
       main=paste0(
         'Log-Normal against other fits for feature distribution of ',
         ds_name, ' (', ds_class_name, ')')
       # sub='[Log-Normal Fit]')
  )
  lines(fit_power_law_with_ln,
        col=cols_dist[col_distpl], lty=1, lwd=2)
  lines(fit_log_normal_only,
        col=cols_dist[col_distln], lty=2, lwd=4)
  lines(fit_exponential_with_ln,
        col=cols_dist[col_distex], lty=3, lwd=2)
  lines(fit_poisson_with_ln,
        col=cols_dist[col_distpo], lty=4, lwd=2)
  abline(v=xmin_ln, lty=3)
  mtext(paste('µ (mean):', round(fit_log_normal_only$pars[1], digits=2),
              ', σ (SD):', round(fit_log_normal_only$pars[2], digits=2),
              '\nVuong\'s test statistics:',
              round(compare_dist_pl_ln_2$test_statistic, digits=3),
              paste0('\n(', closer_to_the_truth_ln, ')'),
              '\np (1-sided):',
              round(compare_dist_pl_ln_2$p_one_sided, digits=2),
              '\np (2-sided):',
              round(compare_dist_pl_ln_2$p_two_sided, digits=2), ' '),
        adj=1, line=-5*0.75, side=3, cex=0.75)
  mtext(paste0('Xmin:', xmin_ln, ', ntail ratio (X>=Xmin):',
               round(ntail_ratio_ln, digits=2)),
        adj=1-ntail_ratio_ln, side=3, line=0, cex=0.75)
  legend(x='bottomleft', box.lty=0, col=cols_dist, lty=1:4, lwd=2,
         legend=c('Power Law', 'Log-Normal*', 'Exponential', 'Poisson'))
  
  plot(bs_power_law, main='bootstrapping hypothesis test: Power Law distribution is plausible?')
  plot(bs_log_normal, main='bootstrapping hypothesis test: Log-Normal distribution is plausible?')
  
  # reinstall gglot2 (install.packages('ggplot2')) for the following error
  # Error in grid.Call.graphics(C_setviewport, vp, TRUE) : 
  #   non-finite location and/or size for viewport
  # Called from: grid.Call.graphics(C_setviewport, vp, TRUE)
  
  # Comparison of likelihood ratios per feature
  # Based on N. Bertchinger
  # https://fias.uni-frankfurt.de/fileadmin/fias/bertschinger/CN/SolutionsWiSe1718_Ex2.pdf
  print(compare_dist_pl_ln$ratio %>%
          group_by(x, ratio) %>%
          summarize(cnt = n()) %>%
          ggplot(aes(x, ratio, color=ratio < 0, size=cnt)) +
          theme_bw() +
          geom_point(alpha=0.6) +
          # scale_color_discrete(name='Test results for distributions',
          #                      labels=c('Power Law*', 'Log-Normal')) +
          scale_color_manual(values=c(cols_dist[col_distpl],
                                      cols_dist[col_distln]),
                             name='Vuong\'s test on',
                             labels=c('Power Law* vs.', 'Log-Normal')) +
          labs(x=xlabel_ds_class,
               y=paste0('Log-likelihood ratios.',
                        ' Test statistics:',
                        round(compare_dist_pl_ln$test_statistic, digits=3),
                        '\n(', closer_to_the_truth_pl, ')',
                        ', p (1-sided):',
                        round(compare_dist_pl_ln$p_one_sided, digits=2),
                        ', p (2-sided):',
                        round(compare_dist_pl_ln$p_two_sided, digits=2)),
               size='Count') +
          scale_x_log10())
  
  print(compare_dist_pl_ln_2$ratio %>%
          group_by(x, ratio) %>%
          summarize(cnt = n()) %>%
          ggplot(aes(x, ratio, color=ratio < 0, size=cnt)) +
          theme_bw() +
          geom_point(alpha=0.6) +
          scale_color_manual(values=c(cols_dist[col_distpl],
                                      cols_dist[col_distln]),
                             name='Vuong\'s test on',
                             labels=c('Power Law vs.', 'Log-Normal*')) +
          labs(x=xlabel_ds_class,
               y=paste0('Log-likelihood ratios.',
                        ' Test statistics:',
                        round(compare_dist_pl_ln_2$test_statistic, digits=3),
                        '\n(', closer_to_the_truth_ln, ')',
                        ', p (1-sided):',
                        round(compare_dist_pl_ln_2$p_one_sided, digits=2),
                        ', p (2-sided):',
                        round(compare_dist_pl_ln_2$p_two_sided, digits=2)),
               size='Count') +
          scale_x_log10())
  
  options(scipen=scipen_option)
  
  return(list(bs_power_law, bs_log_normal))
}