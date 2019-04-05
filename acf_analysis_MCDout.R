library(ggplot2)
library(plyr)
library(ggcorrplot)
library(plotly)

# includes my functions for mulitvariate outlier detection
source('~/Dropbox/code/phantomQA/multivar_outlier.R')


remove_outlier_session <- function(data, sites, vars){
  # detect and remove outliers for each site
  set.seed(123) # not needed if rm.mcd_outlier.qq_chisq uses deterministic MCD
  #data_out <- matrix(,nrow = 0, ncol = length(vars))
  data_out <- NULL
  outliers <- list() # store the outliers for each site
  out_sessions <- list()
  for (s in sites){
    data_sub <- subset(data,data$site == s)
    dat <- data_sub[vars]
    dat <- data.matrix(dat)
    cat(s,'# sessions =',nrow(dat))
    out <- rm.mcd_outlier.qq_chisq(dat,adj.r.square.thresh = 0.5, plt=T, plot_title = s)
    #out <- rm.ours_outlier(dat)
    #out <- rm.mcd_outlier(dat,plt = F, plot_title = s)
    outliers[[s]] <- out
    out_sID <- subset(data,data$site==s)$sessionID[out]
    #print(out_sID)
    out_sessions[[s]] <- paste(out_sID)
    cat(', # outliers =',sum(out),'\n')
    data_sub_out <- data_sub[!out,]
    data_out <- rbind(data_out, data_sub_out)
  }
  
  # # detect and remove outliers for each manufacturer- not good
  # data_out <- matrix(,nrow = 0, ncol = length(vars))
  # for (s in c("GE MEDICAL SYSTEMS", "Philips Medical Systems", "SIEMENS")){
  #   dat <- subset(data,data$scannermanufacturer == s)
  #   dat <- dat[vars]
  #   dat <- data.matrix(dat)
  #   cat(s,nrow(dat),ncol(dat),'\n')
  #   out <- rm.mcd_outlier.qq_chisq(dat,adj.r.square.thresh = 0.9, plt=F, plot_title = s)
  #   #out <- rm.mcd_outlier(dat,plt = F, plot_title = s)
  #   print(sum(out))
  #   dat_out <- dat[!out,]
  #   data_out <- rbind(data_out, dat_out)
  # }
  
  data_out <- data.frame(data_out)
  
  return(list("data_out"=data_out, "out_sessions"=out_sessions))
}

acf_analysis_MCDout <- function(data,
                                plot_title='',
                                show.pca_all = T,
                                show.pca_acf = T,
                                show.pca_fbirn = T,
                                show.pca.varaxes = F,
                                show.pca.timecourse = T,
                                pca.var_axes_separate = F,
                                save_png=F){
  # output:
  # $data_out: outlier-removed data
  # $out_sessions: list of outlier sessions per site
  
  # includes my functions for plotting
  source('~/Dropbox/code/phantomQA/plotting.R')
  
  plotdata(data,
           plot_title = plot_title,
           show.pca_fbirn = show.pca_fbirn,
           show.pca_all = show.pca_all,
           show.pca_acf = show.pca_acf,
           show.pca.varaxes = show.pca.varaxes,
           show.pca.timecourse = show.pca.timecourse,
           pca.var_axes_separate = pca.var_axes_separate,
           save_png = save_png)

  sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")
  
  # vars_fbirn <- c("mean","SFNR","std","percentFluc","drift","driftfit","rdc",
  #                 "minFWHMX","minFWHMY","minFWHMZ","maxFWHMX",
  #                 "meanGhost","meanBrightGhost","SNR")
  # 
  # 
  # vars_acf <- c("minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
  #               "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy")
  
  res <- remove_outlier_session(data, sites = sites, vars = vars_acf)
  data_out <- res$data_out
  out_sessions <- res$out_sessions
  
  plotdata(data_out,
           plot_title = paste(plot_title,'out',sep='_'),
           show.pca_fbirn = show.pca_fbirn,
           show.pca_all = show.pca_all,
           show.pca_acf = show.pca_acf,
           show.pca.varaxes = show.pca.varaxes,
           show.pca.timecourse = show.pca.timecourse,
           pca.var_axes_separate = pca.var_axes_separate,
           save_png = save_png)

  return(list("data_out"=data_out, "out_sessions"=out_sessions))  
}

