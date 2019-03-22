# to produce report:
# library(rmarkdown)
# render('run_acf_analysis_smooth_detrend_fit.R')

acfdir <- "~/Dropbox/analysis/phantomQA/acf/smooth_detrend_fit"

setwd(acfdir)

spotfire_file <- '../../fBIRN_MR_QA_20180820.csv'
fbirnQA_file <- 'subjects_smooth_fbirnQA.csv'

acf_rem_suffix <- '_3dBlurToFWHM_3dDetrend.nii.gz'
fbirn_rem_suffix <- '_3dBlurToFWHM'

source("~/Dropbox/code/phantomQA/get_data.R")
data <- get_data(acfdir = acfdir, fbirnQA_file = fbirnQA_file, spotfire_file = spotfire_file,
                 acf_rem_suffix = acf_rem_suffix, fbirn_rem_suffix = fbirn_rem_suffix)

nrow(data)

plot_title <- 'smooth_detrend_fit'

source('~/Dropbox/code/phantomQA/acf_analysis_MCDout.R')
res_smooth <- acf_analysis_MCDout(data = data, output_dir = acfdir, plot_title = plot_title,
                                  show.pca.varaxes = T,
                                  show.pca.timecourse = F,
                                  save_png = T,
                                  pca.var_axes_separate = T)

nrow(res_smooth$data_out)
cat(paste(res_smooth$out_sessions$BYC, collapse = '\n'))

####################################################
# compare outlier detection based on acf and fbirn
####################################################
vars_fbirn <- c("mean","SFNR","std","percentFluc","drift","driftfit","rdc",
                "minFWHMX","minFWHMY","minFWHMZ","maxFWHMX",
                "meanGhost","meanBrightGhost","SNR")


vars_acf <- c("minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
              "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy")

sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")
#res_smooth_acf <- remove_outlier_session(data = data, sites = sites, vars = vars_acf)

# the following throws an error for SMH
#res_smooth_fbirn <- remove_outlier_session(data = data, sites = sites, vars = vars_fbirn)

#nrow(data)
#nrow(res_smooth_acf$data_out)

