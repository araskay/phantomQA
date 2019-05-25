# to produce report:
# library(rmarkdown)
# render('run_acf_analysis_smooth_detrend_fit.R')

source("~/Dropbox/code/phantomQA/get_data.R")
source('~/Dropbox/code/phantomQA/acf_analysis.R')

working_dir <- "~/Dropbox/analysis/phantomQA/acf/smooth_detrend_fit"

setwd(working_dir)

acf_file <- 'acfFWHM.csv'

spotfire_file <- '../../fBIRN_MR_QA_20180820.csv'
fbirnQA_file <- 'subjects_smooth_fbirnQA.csv'

acf_rem_suffix <- '_3dBlurToFWHM_3dDetrend'
fbirn_rem_suffix <- '_3dBlurToFWHM'

data <- get_data(acfFWHM_file = acf_file, fbirnQA_file = fbirnQA_file, spotfire_file = spotfire_file,
                 acf_rem_suffix = acf_rem_suffix, fbirn_rem_suffix = fbirn_rem_suffix)

write.csv(data, file = "data_clean.csv")

plot_title <- 'smooth_detrend_fit'
sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")

res_smooth <- acf_analysis(data = data,
                    plot_title = plot_title,
                    show.pca.varaxes = T,
                    show.pca.timecourse = T,
                    save_png = T,
                    pca.var_axes_separate = T,
                    anom_coef = 10)

nrow(res_smooth$data_out)
cat(paste(res_smooth$out_sessions$BYC, collapse = '\n'))

