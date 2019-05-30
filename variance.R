# this scripts calculates and compares variance for different manufacturers

source('~/Dropbox/code/phantomQA/acf_analysis_physical_anomalies.R')

# the directory containing unprocessed data
unprocessed_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

data <- read.csv(paste(unprocessed_dir,'/data_clean.csv',sep = ''))

print_variace <- function(data){
  pca <- prcomp(~mean+SFNR+std+percentFluc+drift+driftfit+rdc+minFWHMX+minFWHMY+maxFWHMX+maxFWHMY+meanGhost+meanBrightGhost+SNR,
                data = data,
                center = T,
                scale. = T)
  
  # calculate total variance for PC1
  cat('Total var =', pca$sdev[1]^2, '\n') # equal to var(pca$x[,'PC1'])
  
  # variance for each manufacturer for PC1
  manufacturers <- levels(data$scannermanufacturer)
  for (m in manufacturers){
    cat(m, 'var =', var(pca$x[data$scannermanufacturer==m,'PC1']),'\n')
  }
  
  sites <- levels(data$site)
  for (s in sites){
    cat(s, 'var =', var(pca$x[data$site==s,'PC1']),'\n')
  }
}

## (a) first calculate the variance for all the sessions, including the anomalous sessions
cat('Variance with anomalies included:\n')
print_variace(data = data)

## (b) now calculate the variance with anomalous sessions excluded
sites <- levels(data$site)
res <- physical_anomalies(data = data, sites = sites)
data_out <- res$data_out
cat('Variance with anomalies excluded:\n')
print_variace(data = data_out)
