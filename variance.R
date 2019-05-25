# this scripts calculates and compares variance for different manufacturers

#output_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

# the directory containing unprocessed data
unprocessed_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

# the directory containing spikecor processed data
#spikecor_dir <- "~/Dropbox/analysis/phantomQA/acf/spikecor_slc_detrend_fit"

#setwd(output_dir)

data <- read.csv(paste(unprocessed_dir,'/data_clean.csv',sep = ''))

pca <- prcomp(~mean+SFNR+std+percentFluc+drift+driftfit+rdc+minFWHMX+minFWHMY+maxFWHMX+maxFWHMY+meanGhost+meanBrightGhost+SNR,
              data = data,
              center = T,
              scale. = T)

# calculate total variance for PC1
cat('Total var =', pca$sdev[1]^2) # equal to var(pca$x[,'PC1'])

# variance for each manufacturer for PC1
manufacturers <- levels(data$scannermanufacturer)
for (m in manufacturers){
  cat(m, 'var =', var(pca$x[data$scannermanufacturer==m,'PC1']),'\n')
}

sites <- levels(data$site)
for (s in sites){
  cat(s, 'var =', var(pca$x[data$site==s,'PC1']),'\n')
}

