# to produce report:
# library(rmarkdown)
# render('run_acf_analysis_detrend_fit.R')

source("~/Dropbox/code/phantomQA/get_data.R")
source('~/Dropbox/code/phantomQA/acf_analysis.R')

acf_file <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit/acfFWHM.csv"
working_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

setwd(working_dir)

spotfire_file <- '../../fBIRN_MR_QA_20180820.csv'
fbirnQA_file <- './fbirnQA/subjects_fbirnQA.csv'

acf_rem_suffix <- '_3dDetrend'
fbirn_rem_suffix <- ''

data <- get_data(acfFWHM_file = acf_file, fbirnQA_file = fbirnQA_file, spotfire_file = spotfire_file,
                 acf_rem_suffix = acf_rem_suffix, fbirn_rem_suffix = fbirn_rem_suffix)

write.csv(data, file = "data_clean.csv")

plot_title <- 'detrend_fit'

# detect anomalies based on physical quantities of fwhm
sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")

res <- acf_analysis(data = data,
                    plot_title = plot_title,
                    show.pca.varaxes = T,
                    show.pca.timecourse = T,
                    save_png = T,
                    pca.var_axes_separate = T, # if var_axes_separate is False, will NOT return a plotly PCA plot
                    anom_coef = 10)

api_create(res$plots$all_data$pca_fbirn[[1]], filename = "phantomQA-detrend_fit_all_data_pca_fbirn")
api_create(res$plots$anomalies_excluded$pca_fbirn[[1]], filename = "phantomQA-detrend_fit_anomalies_excluded_pca_fbirn")
api_create(ggplotly(res$plots$all_data$fwhm_barplot$p_FWHM), filename = "phantomQA-detrend_fit_all_data_acf_fwhm_barplot")
api_create(res$plots$all_data$fbirn_barplot, filename = "phantomQA-detrend_fit_all_data_fbirn_barplot")

nrow(res$data_out)
print(res$out_sessions)

res_detrend <- res
cat(paste(res_detrend$out_sessions$UCA, collapse = '\n'))

####################################################
# compare outlier detection based on acf and fbirn
####################################################
# vars_fbirn <- c("mean","SFNR","std","percentFluc","drift","driftfit","rdc",
#                 "minFWHMX","minFWHMY","minFWHMZ","maxFWHMX",
#                 "meanGhost","meanBrightGhost","SNR")
# 
# 
# vars_acf <- c("minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
#               "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy")
# 
# sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "TBR", "SMH", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")
# res_acf <- remove_outlier_session(data = data, sites = sites, vars = vars_acf)
# 
# res_fbirn <- remove_outlier_session(data = data, sites = sites, vars = vars_fbirn)
