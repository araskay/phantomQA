# This script creates additional plots, used in the resulting publications

output_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

# the directory containing unprocessed data
unprocessed_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

# the directory containing spikecor processed data
spikecor_dir <- "~/Dropbox/analysis/phantomQA/acf/spikecor_slc_detrend_fit"

setwd(output_dir)

data_unproc <- read.csv(paste(unprocessed_dir,'/data_clean.csv',sep = ''))
data_spikecor <- read.csv(paste(spikecor_dir,'/data_clean.csv',sep = ''))


## ACF FWHM bar plots
source('~/Dropbox/code/phantomQA/plotting.R')
p_unproc <- get_fwhm_sub_barplots(data_unproc)
#p_spikecor <- get_fwhm_sub_barplots(data_spikecor)

p_unproc$p_maxFWHMx <- p_unproc$p_maxFWHMx + theme(legend.title=element_blank())
mylegend <- g_legend(p_unproc$p_maxFWHMx)
png(filename='detrend_fit_acf_min_max_fwhm.png', width = 21, height = 10, units = "cm", res = 600)
grid.arrange(arrangeGrob(arrangeGrob(p_unproc$p_maxFWHMx + theme(legend.position="none"),
                                     p_unproc$p_maxFWHMy + theme(legend.position="none"),
                                     p_unproc$p_minFWHMx + theme(legend.position="none"),
                                     p_unproc$p_minFWHMy + theme(legend.position="none"),
                                     ncol = 2, nrow = 2, top = 'Min vs. Max FWHM'),
                         ncol = 1, nrow = 1),
             mylegend,
             ncol=2, widths=c(5,1))
dev.off()


## (# anomalies)/volume bar plots
source('~/Dropbox/code/phantomQA/plotting.R')
p_unproc <- get_fwhm_sub_barplots(data_unproc)
p_spikecor <- get_fwhm_sub_barplots(data_spikecor)

p_unproc$p_maxFWHMx <- p_unproc$p_maxFWHMx + theme(legend.title=element_blank())
mylegend <- g_legend(p_unproc$p_maxFWHMx)
png(filename='anomaly_per_vol.png', width = 21, height = 24, units = "cm", res = 600)
grid.arrange(arrangeGrob(arrangeGrob(p_unproc$p_medAnomalyPerVolx + theme(legend.position="none"),
                                     p_unproc$p_medAnomalyPerVoly + theme(legend.position="none"),
                                     p_unproc$p_q1AnomalyPerVolx + theme(legend.position="none"),
                                     p_unproc$p_q1AnomalyPerVoly + theme(legend.position="none"),
                                     p_unproc$p_q3AnomalyPerVolx + theme(legend.position="none"),
                                     p_unproc$p_q3AnomalyPerVoly + theme(legend.position="none"),
                                     ncol = 2, nrow = 3, top = '(a) Unprocessed'),
                         arrangeGrob(p_spikecor$p_medAnomalyPerVolx + theme(legend.position="none"),
                                     p_spikecor$p_medAnomalyPerVoly + theme(legend.position="none"),
                                     p_spikecor$p_q1AnomalyPerVolx + theme(legend.position="none"),
                                     p_spikecor$p_q1AnomalyPerVoly + theme(legend.position="none"),
                                     p_spikecor$p_q3AnomalyPerVolx + theme(legend.position="none"),
                                     p_spikecor$p_q3AnomalyPerVoly + theme(legend.position="none"),
                                     ncol = 2, nrow = 3, top = '(b) SpikeCor processed'),
                         ncol = 1, nrow = 2),
             mylegend,
             ncol=2, widths=c(5,1))
dev.off()


## total number of anomalies bar plots
source('~/Dropbox/code/phantomQA/plotting.R')
p_unproc <- get_fwhm_sub_barplots(data_unproc)
p_spikecor <- get_fwhm_sub_barplots(data_spikecor)

p_unproc$p_maxFWHMx <- p_unproc$p_maxFWHMx + theme(legend.title=element_blank())
mylegend <- g_legend(p_unproc$p_maxFWHMx)
png(filename='total_num_anomaly.png', width = 21, height = 8, units = "cm", res = 600)
grid.arrange(arrangeGrob(arrangeGrob(p_unproc$p_numAnomaly_x + theme(legend.position="none"),
                                     p_unproc$p_numAnomaly_y + theme(legend.position="none"),
                                     ncol = 2, nrow = 1, top = '(a) Unprocessed'),
                         arrangeGrob(p_spikecor$p_numAnomaly_x + theme(legend.position="none"),
                                     p_spikecor$p_numAnomaly_y + theme(legend.position="none"),
                                     ncol = 2, nrow = 1, top = '(b) SpikeCor processed'),
                         ncol = 1, nrow = 2),
             mylegend,
             ncol=2, widths=c(5,1))
dev.off()

