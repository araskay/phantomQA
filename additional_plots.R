# This script creates additional plots, used in the resulting publications

library(ggplot2)
library(reshape2)
library(plotly)

reorder_sites <- function(data){
  # reoder sites for plotting
  
  data$site <- factor(
    data$site,
    levels = c("CAM", "MCM", "SBH", "TWH", "UCA", "BYC", "WEU", "QNS", "TOH", "UTO", "SMH", "TBR", "UBC")
  )
  
  # sort by site
  data <- data[order(data$site),]
  
  # make sessionID an ordered factor so that ggplot does not sort it
  data$sessionID <- factor(data$sessionID, levels = data$sessionID)
  
  return(data)
}

output_dir <- "/Users/Aras/projects/phantomqa/phantomQA_data/detrend_fit"

# the directory containing unprocessed data
unprocessed_dir <- "/Users/Aras/projects/phantomqa/phantomQA_data/detrend_fit"

# the directory containing spikecor processed data
spikecor_dir <- "/Users/Aras/projects/phantomqa/phantomQA_data/spikecor_slc_detrend_fit"

setwd(output_dir)

data_unproc <- read.csv(paste(unprocessed_dir,'/data_clean.csv',sep = ''))
data_spikecor <- read.csv(paste(spikecor_dir,'/data_clean.csv',sep = ''))

data_unproc <- reorder_sites(data_unproc)
data_spikecor <- reorder_sites(data_spikecor)

## ACF FWHM bar plots
source('/Users/Aras/projects/phantomqa/phantomQA/plotting.R')
p_unproc <- get_fwhm_sub_barplots(data_unproc)
p_spikecor <- get_fwhm_sub_barplots(data_spikecor)

p_unproc$p_maxFWHMx <- p_unproc$p_maxFWHMx + theme(legend.title=element_blank())
mylegend <- g_legend(p_unproc$p_maxFWHMx)
png(filename='acf_min_max_fwhm.png', width = 21, height = 10, units = "cm", res = 600)
grid.arrange(arrangeGrob(arrangeGrob(p_unproc$p_maxFWHMx + theme(legend.position="none"),
                                     p_unproc$p_maxFWHMy + theme(legend.position="none"),
                                     p_unproc$p_minFWHMx + theme(legend.position="none"),
                                     p_unproc$p_minFWHMy + theme(legend.position="none"),
                                     ncol = 2, nrow = 2, top = '(a) Unprocessed'),
                         arrangeGrob(p_spikecor$p_maxFWHMx + theme(legend.position="none"),
                                     p_spikecor$p_maxFWHMy + theme(legend.position="none"),
                                     p_spikecor$p_minFWHMx + theme(legend.position="none"),
                                     p_spikecor$p_minFWHMy + theme(legend.position="none"),
                                     ncol = 2, nrow = 2, top = '(b) SpikeCor processed'),
                         ncol = 1, nrow = 2),
             mylegend,
             ncol=2, widths=c(5,1))
dev.off()


## (# anomalies)/volume bar plots
source('~/Dropbox/code/phantomQA/plotting.R')
p_unproc <- get_fwhm_sub_barplots(data_unproc)
p_spikecor <- get_fwhm_sub_barplots(data_spikecor)

p_unproc$p_maxFWHMx <- p_unproc$p_maxFWHMx + theme(legend.title=element_blank())
mylegend <- g_legend(p_unproc$p_maxFWHMx)
png(filename='anomaly_per_vol.png', width = 21, height = 8, units = "cm", res = 600)
grid.arrange(arrangeGrob(arrangeGrob(p_unproc$p_medAnomalyPerVolx + theme(legend.position="none"),
                                     p_unproc$p_medAnomalyPerVoly + theme(legend.position="none"),
                                     ncol = 2, nrow = 1, top = '(a) Unprocessed'),
                         arrangeGrob(p_spikecor$p_medAnomalyPerVolx + theme(legend.position="none"),
                                     p_spikecor$p_medAnomalyPerVoly + theme(legend.position="none"),
                                     ncol = 2, nrow = 1, top = '(b) SpikeCor processed'),
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

## total number of anomalies and (# anomalies)/vol in a single plot
png(filename='anomalies.png', width = 21, height = 15, units = "cm", res = 600)
grid.arrange(arrangeGrob(arrangeGrob(rbind(ggplotGrob(p_unproc$p_numAnomaly_x + theme(legend.position="none")),
                                           ggplotGrob(p_unproc$p_medAnomalyPerVolx + theme(legend.position="none")),
                                           size = "first"),
                                     rbind(ggplotGrob(p_unproc$p_numAnomaly_y + theme(legend.position="none")),
                                           ggplotGrob(p_unproc$p_medAnomalyPerVoly + theme(legend.position="none")),
                                           size = "first"),
                                     ncol=2, top = '(a) Unprocessed'),
                         arrangeGrob(rbind(ggplotGrob(p_spikecor$p_numAnomaly_x + theme(legend.position="none")),
                                           ggplotGrob(p_spikecor$p_medAnomalyPerVolx + theme(legend.position="none")),
                                           size = "first"),
                                     rbind(ggplotGrob(p_spikecor$p_numAnomaly_y + theme(legend.position="none")),
                                           ggplotGrob(p_spikecor$p_medAnomalyPerVoly + theme(legend.position="none")),
                                           size = "first"),
                                     ncol=2, top = '(b) SpikeCor processed'),
                         
                         nrow = 2),
             mylegend,
             ncol=2, widths=c(5,1))
dev.off()

## Eigen spectra before and after SpikeCor censoring
pca_unproc <- prcomp(~mean+SFNR+std+percentFluc+drift+driftfit+rdc+minFWHMX+minFWHMY+maxFWHMX+maxFWHMY+meanGhost+meanBrightGhost+SNR,
                     data = data_unproc,
                     center = T,
                     scale. = T)

pca_spikecor <- prcomp(~mean+SFNR+std+percentFluc+drift+driftfit+rdc+minFWHMX+minFWHMY+maxFWHMX+maxFWHMY+meanGhost+meanBrightGhost+SNR,
                       data = data_spikecor,
                       center = T,
                       scale. = T)

plot(pca_unproc$sdev^2 / sum(pca_unproc$sdev^2), type = "b")
plot(pca_unproc$sdev^2 / sum(pca_unproc$sdev^2), type = "b")

Unprocessed <- pca_unproc$sdev^2 / sum(pca_unproc$sdev^2)
SpikeCore <- pca_spikecor$sdev^2 / sum(pca_spikecor$sdev^2)
PC <- seq_along(pca_unproc$sdev)

eigenspectra <- data.frame(PC, Unprocessed, SpikeCore)
d <- melt(eigenspectra, id.vars="PC", variable.name = 'Data', value.name = 'Fractional_Variance')

p <- ggplot(data = d, aes(x = PC, y = Fractional_Variance, col = Data)) + geom_point() + geom_line()
print(p)

png(filename = "eigen_spectra.png", width = 16, height = 10, units = "cm", res = 600)
print(p)
dev.off()

# publish to plotly
p <- ggplotly(p)
api_create(p, filename = "phantomQA-eigenspectra")
