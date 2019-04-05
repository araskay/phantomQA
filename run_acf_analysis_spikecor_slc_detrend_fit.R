# to produce report:
# library(rmarkdown)
# render('run_acf_analysis_spikecor_slc_detrend_fit.R')

workingdir <- "~/Dropbox/analysis/phantomQA/acf/spikecor_slc_detrend_fit"

setwd(workingdir)

acf_file <- 'acfFWHM.csv'

spotfire_file <- '../../fBIRN_MR_QA_20180820.csv'
fbirnQA_file <- 'subjects_spikecor_slc_fbirnQA.csv'

acf_rem_suffix <- '_spikecor_3dDetrend'
fbirn_rem_suffix <- '_spikecor'

source("~/Dropbox/code/phantomQA/get_data.R")
data <- get_data(acfFWHM_file = acf_file, fbirnQA_file = fbirnQA_file, spotfire_file = spotfire_file,
                 acf_rem_suffix = acf_rem_suffix, fbirn_rem_suffix = fbirn_rem_suffix)

plot_title <- 'spikecor_slc_detrend_fit'

# detect anomalies based on physical quantities of fwhm
sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")
source('~/Dropbox/code/phantomQA/acf_analysis_physical_anomalies.R')

#res <- physical_anomalies(data = data,sites = sites)
print_physical_anomalies(data = data, sites = sites)



source('~/Dropbox/code/phantomQA/acf_analysis_MCDout.R')
res <- acf_analysis_MCDout(data = data,
                           plot_title = plot_title,
                           show.pca.varaxes = T,
                           show.pca.timecourse = T,
                           save_png = T,
                           pca.var_axes_separate = T)

nrow(res$data_out)
print(res$out_sessions)

res_spikecor_slc_detrend <- res

cat(paste(res_spikecor_slc_detrend$out_sessions$BYC, collapse = '\n'))
