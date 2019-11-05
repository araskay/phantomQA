library(ggplot2)
library(plyr)
library(ggcorrplot)
library(plotly)

acf_analysis <- function(data,
                         plot_title='',
                         show.pca_all = T,
                         show.pca_acf = T,
                         show.pca_fbirn = T,
                         show.pca.varaxes = F,
                         show.pca.timecourse = T,
                         pca.var_axes_separate = F,
                         pca.return_plotly = F,
                         save_png = F,
                         anom_coef = 10){
  # output:
  # $data_out: outlier-removed data
  # $out_sessions: list of outlier sessions per site
  
  script.dir <- dirname(sys.frame(1)$ofile)
  source(paste(script.dir,'plotting.R',sep='')) # includes functions for plotting
  source(paste(script.dir,'acf_analysis_physical_anomalies.R',sep=''))
  
  plots_all <- plotdata(data,
                        plot_title = plot_title,
                        show.pca_fbirn = show.pca_fbirn,
                        show.pca_all = show.pca_all,
                        show.pca_acf = show.pca_acf,
                        show.pca.varaxes = show.pca.varaxes,
                        show.pca.timecourse = show.pca.timecourse,
                        pca.var_axes_separate = pca.var_axes_separate,
                        pca.return_plotly = pca.return_plotly,
                        save_png = save_png)

  sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")
  
  # vars_fbirn <- c("mean","SFNR","std","percentFluc","drift","driftfit","rdc",
  #                 "minFWHMX","minFWHMY","minFWHMZ","maxFWHMX",
  #                 "meanGhost","meanBrightGhost","SNR")
  # 
  # 
  # vars_acf <- c("minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
  #               "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy")
  
  res <- physical_anomalies(data = data, sites = sites, c = anom_coef)
  print_physical_anomalies(data = data, sites = sites, c = anom_coef)
  data_out <- res$data_out
  out_sessions <- res$out_sessions
  
  plots_anom <- plotdata(data_out,
                         plot_title = paste(plot_title,'anom',sep='_'),
                         show.pca_fbirn = show.pca_fbirn,
                         show.pca_all = show.pca_all,
                         show.pca_acf = show.pca_acf,
                         show.pca.varaxes = show.pca.varaxes,
                         show.pca.timecourse = show.pca.timecourse,
                         pca.var_axes_separate = pca.var_axes_separate,
                         pca.return_plotly = pca.return_plotly,
                         save_png = save_png)

  plots <- list("all_data"=plots_all, "anomalies_excluded"=plots_anom)
  return(list("data_out"=data_out, "out_sessions"=out_sessions, "plots"=plots))  
}

