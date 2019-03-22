## contains functions for multivariate outlier detection/removal
library(mvoutlier) # used for chi-square plots
library(robustbase)

rm.mcd_outlier.qq_chisq <- function(dat, adj.r.square.thresh = 0.9, plt = F, plot_title = ''){
  mcd <- covMcd(dat, nsamp = "deterministic")
  cm <- mcd$center
  S <- mcd$cov
  # Take square root to get the distance
  d <- sqrt(apply(dat, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))
  
  outlier <- logical(length(d))
  
  x <- sort(d^2)
  y <- qchisq(ppoints(length(d)),df = ncol(dat))
  model <- lm(y ~ x)
  m <- summary(model)$coefficients['x','Estimate']
  r <- summary(model)$adj.r.squared
  if (plt){
    qqplot(x,y)
    abline(model$coefficients)
    title(paste(plot_title,'\n','slope=',m,'\n','adj.r.squared=',r))
  }
  while ((r<adj.r.square.thresh) & (sum(!is.na(d))>2)){
    outlier[which.max(d)] <- T
    d[which.max(d)] <- NA
    x <- sort(d^2)
    
    y <- qchisq(ppoints(sum(! is.na(d))),df = ncol(dat))
    model <- lm(y ~ x)
    m <- summary(model)$coefficients['x','Estimate']
    r <- summary(model)$adj.r.squared
    if (plt){
      qqplot(x,y)
      abline(model$coefficients)
      title(paste(plot_title,'\n','slope=',m,'\n','adj.r.squared=',r))
    }
  }
  return(outlier)
}

rm.mcd_outlier <- function(dat, chisquare.crit.val = 0.975, plt = F, plot_title = ''){
  # multivariate outlier detection based on minimum covariance determinent (MCD),
  # i.e., robust Mahalanobis distance.
  # dat is a 2D matrix of size nxp, where n is the number of observations
  # and p is the number of variables
  # output is a boolean vector of length n, where T indicates outlier
  mcd <- covMcd(dat)
  cm <- mcd$center
  S <- mcd$cov
  # Take square root to get the distance
  d_rd <- sqrt(apply(dat, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))
  #d_rd <- sqrt(mcd$mah)
  # calculate critical value
  crit = sqrt(qchisq(chisquare.crit.val,df = ncol(dat)))
  if (plt) {
    plot(d_rd)
    abline(a=crit, b = 0)
    title(plot_title)
  }
  return(d_rd > crit)
}

rm.md_outlier <- function(dat, chisquare.crit.val = 0.975, plt = F, plot_title = ''){
  # multivariate outlier detection based on Mahalanobis distance
  # dat is a 2D matrix of size nxp, where n is the number of observations
  # and p is the number of variables
  # output is a boolean vector of length n, where T indicates outlier
  cm <- colMeans(dat)
  S <- cov(dat)
  # Take square root to get the distance
  d_md <- sqrt(apply(dat, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))
  # calculate critical value
  crit = sqrt(qchisq(chisquare.crit.val,df = ncol(dat)))
  if (plt) {
    plot(d_md)
    abline(a=crit, b = 0)
    title(plot_title)
  }
  return(d_md > crit)
}

#########################################################
# # use the following to install the libraries
# # devtools::install_github("derekbeaton/GSVD")
# # devtools::install_github("derekbeaton/OuRS", subdir = "/OuRS")
# library(tictoc)
# library(ours)
# library(GSVD)
# library(rrcov)
# library(robustbase)
# library(cellWise)
# #library(golubEsets)
# #library(corrplot)
# #library(ExPosition)
# #library(limma)
# 
# rm.ours_outlier <- function(dat, cutoff.thresh=0.9){
#   mcd.results <- cont.mcd(dat)
#   bootstrap.results <- cont.boot.sup.u(dat, center = mcd.results$cov$center, scale = mcd.results$cov$scale, loadings = mcd.results$cov$loadings, singular.values = mcd.results$cov$singular.values)
#   robust.outlier_cutoff <- sort(c(bootstrap.results))[length(sort(c(bootstrap.results)))*cutoff.thresh]
#   robust.outliers <- mcd.results$dists$rob.md > robust.outlier_cutoff
#   return(robust.outliers)
# }