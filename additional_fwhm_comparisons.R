# this scripts compares the detected FWHM between different manufacturers

print_variance <- function(data){
  sites <- levels(data$site)
  for (s in sites){
    cat(s, '95%-CI of minFWHMx = ')
    cat(format(mean(data[data$site==s,"minFWHMx"]), digits = 2), '+- ')
    cat(format(qnorm(0.975)*sd(data[data$site==s,"minFWHMx"])/sqrt(length(data[data$site==s,"minFWHMx"])),digits = 2))
    
    cat(' , minFWHMy = ')
    cat(format(mean(data[data$site==s,"minFWHMy"]), digits = 2), '+- ')
    cat(format(qnorm(0.975)*sd(data[data$site==s,"minFWHMy"])/sqrt(length(data[data$site==s,"minFWHMy"])),digits = 2))
    
    cat('\n')
  }
}

## (a) print variance for unprocessed data

# the directory containing unprocessed data
unprocessed_dir <- "~/Dropbox/analysis/phantomQA/acf/detrend_fit"

data <- read.csv(paste(unprocessed_dir,'/data_clean.csv',sep = ''))

print_variance(data)


## (b) print variance for spatially smoothed data

# the directory containing unprocessed data
unprocessed_dir <- "~/Dropbox/analysis/phantomQA/acf/smooth_detrend_fit"

data <- read.csv(paste(unprocessed_dir,'/data_clean.csv',sep = ''))

print_variance(data)
