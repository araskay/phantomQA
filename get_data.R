vars_fbirn <- c("mean","SFNR","std","percentFluc","drift","driftfit","rdc",
                "minFWHMX","minFWHMY","minFWHMZ","maxFWHMX",
                "meanGhost","meanBrightGhost","SNR")


vars_acf <- c("minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
              "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy")

organise_sessionID <- function (data, rem_suffix){
  # This function modifies session IDs by removing unwanted suffixes
  # Inputs:
  #       data: dataframe
  #       rem_suffix: string- suffix to be removed
  # Output:
  #       modified dataframe
  data$sessionID <- gsub(rem_suffix,'',data$sessionID)
  data <- data[order(data$sessionID),]
  return(data)
}

get_fbirnQA_data <- function(QAfile, rem_suffix='') {
  # this function reads the QA file and returns a dataframe of the QA data
  # inputs:
  #       QAfile: path/name of the QA csv file
  #       rem_suffix: suffix to be removed from sessionIDs
  # output:
  #       a dataframe including the QA data
  data <- read.csv(QAfile)
  data <- organise_sessionID(data, rem_suffix = rem_suffix)
  return(data)
}




get_acf_data <- function(acfdir, rem_suffix){
  
  makeSessionID <- function(l,rem_suffix) {
    # this function gets a list including file names and FWHM values
    # and extracts subjectIDs from filenames
    # get base file name from the path and file name
    l[,1] <- basename(as.character(l[,1]))
    # remove unwnated parts of the file name to get subject ID
    l[,1] <- gsub(rem_suffix,'',l[,1])
    # sort by SessionID- this is crucial since different summary stats (e.g., max, Q1, etc.) are written in different orders
    l <- l[order(l[,1]),]
    return(l)
  }
  
  # make sure ending / is consistent
  acfdir <- sub('/$','',acfdir)
  acfdir <- paste(acfdir,'/',sep = '')
  
  # read volume-wise FWHM
  maxFWHMx_v <- read.csv(paste(acfdir,'maxFWHMx.csv',sep = ''))
  maxFWHMx_v <- makeSessionID(maxFWHMx_v, rem_suffix)
  
  minFWHMx_v <- read.csv(paste(acfdir,'minFWHMx.csv',sep = ''))
  minFWHMx_v <- makeSessionID(minFWHMx_v, rem_suffix)
  
  meanFWHMx_v <- read.csv(paste(acfdir,'meanFWHMx.csv',sep = ''))
  meanFWHMx_v <- makeSessionID(meanFWHMx_v, rem_suffix)
  
  medFWHMx_v <- read.csv(paste(acfdir,'medFWHMx.csv',sep = ''))
  medFWHMx_v <- makeSessionID(medFWHMx_v, rem_suffix)
  
  q1FWHMx_v <- read.csv(paste(acfdir,'q1FWHMx.csv',sep = ''))
  q1FWHMx_v <- makeSessionID(q1FWHMx_v, rem_suffix)
  
  q3FWHMx_v <- read.csv(paste(acfdir,'q3FWHMx.csv',sep = ''))
  q3FWHMx_v <- makeSessionID(q3FWHMx_v, rem_suffix)
  
  stdFWHMx_v <- read.csv(paste(acfdir,'stdFWHMx.csv',sep = ''))
  stdFWHMx_v <- makeSessionID(stdFWHMx_v, rem_suffix)
  
  # read y FWHM
  maxFWHMy_v <- read.csv(paste(acfdir,'maxFWHMy.csv',sep = ''))
  maxFWHMy_v <- makeSessionID(maxFWHMy_v, rem_suffix)
  
  minFWHMy_v <- read.csv(paste(acfdir,'minFWHMy.csv',sep = ''))
  minFWHMy_v <- makeSessionID(minFWHMy_v, rem_suffix)
  
  meanFWHMy_v <- read.csv(paste(acfdir,'meanFWHMy.csv',sep = ''))
  meanFWHMy_v <- makeSessionID(meanFWHMy_v, rem_suffix)
  
  medFWHMy_v <- read.csv(paste(acfdir,'medFWHMy.csv',sep = ''))
  medFWHMy_v <- makeSessionID(medFWHMy_v, rem_suffix)
  
  q1FWHMy_v <- read.csv(paste(acfdir,'q1FWHMy.csv',sep = ''))
  q1FWHMy_v <- makeSessionID(q1FWHMy_v, rem_suffix)
  
  q3FWHMy_v <- read.csv(paste(acfdir,'q3FWHMy.csv',sep = ''))
  q3FWHMy_v <- makeSessionID(q3FWHMy_v, rem_suffix)
  
  stdFWHMy_v <- read.csv(paste(acfdir,'stdFWHMy.csv',sep = ''))
  stdFWHMy_v <- makeSessionID(stdFWHMy_v, rem_suffix)
  
  # transint time
  teq <- read.csv(paste(acfdir,'teq.csv',sep = ''))
  teq <- makeSessionID(teq, rem_suffix)
  
  # calculate session-wise parameters from slice-wise
  maxFWHMx <- apply(maxFWHMx_v[,2:length(maxFWHMx_v)],1,max)
  minFWHMx <- apply(minFWHMx_v[,2:length(minFWHMx_v)],1,min)
  meanFWHMx <- apply(meanFWHMx_v[,2:length(meanFWHMx_v)],1,mean)
  medFWHMx <- apply(medFWHMx_v[,2:length(medFWHMx_v)],1,median)
  q1FWHMx <- apply(q1FWHMx_v[,2:length(q1FWHMx_v)],1,quantile, probs=0.25, na.rm=T)
  q3FWHMx <- apply(q3FWHMx_v[,2:length(q3FWHMx_v)],1,quantile, probs=0.75, na.rm=T)
  stdFWHMx <- apply(stdFWHMx_v[,2:length(stdFWHMx_v)],1,mean)
  
  maxFWHMy <- apply(maxFWHMy_v[,2:length(maxFWHMy_v)],1,max)
  minFWHMy <- apply(minFWHMy_v[,2:length(minFWHMy_v)],1,min)
  meanFWHMy <- apply(meanFWHMy_v[,2:length(meanFWHMy_v)],1,mean)
  medFWHMy <- apply(medFWHMy_v[,2:length(medFWHMy_v)],1,median)
  q1FWHMy <- apply(q1FWHMy_v[,2:length(q1FWHMy_v)],1,quantile, probs=0.25, na.rm=T)
  q3FWHMy <- apply(q3FWHMy_v[,2:length(q3FWHMy_v)],1,quantile, probs=0.75, na.rm=T)
  
  stdFWHMy <- apply(stdFWHMy_v[,2:length(stdFWHMy_v)],1,mean)
  
  sessionID <- maxFWHMx_v[,1]
  
  teq <- teq[,2]
  
  data <- data.frame(sessionID,maxFWHMx,minFWHMx,meanFWHMx,medFWHMx,q1FWHMx,q3FWHMx,stdFWHMx,maxFWHMy,minFWHMy,meanFWHMy,medFWHMy,q1FWHMy,q3FWHMy,stdFWHMy, teq)
  
  # get 
  
  return(data)
}


get_data <- function(acfdir, fbirnQA_file, spotfire_file, acf_rem_suffix='', fbirn_rem_suffix){
# this function reads the acf and fbirn data, merges them,
# adds site and scanner manufacturer data from spotfire,
# and removes unwanted sessions

  data_acf <- get_acf_data(acfdir = acfdir, rem_suffix = acf_rem_suffix)
  cat('\nTotal of ', toString(nrow(data_acf)), ' ACF sessions read.\n')
  
  data_fbirn <- get_fbirnQA_data(QAfile = fbirnQA_file, rem_suffix = fbirn_rem_suffix)
  cat('Total of ', toString(nrow(data_fbirn)), ' fBIRN QA sessions read.\n')
  
  data_spotfire <- read.csv(spotfire_file)
  cat('Total of ', toString(nrow(data_spotfire)), ' Spotfire sessions read.\n')
  
  # merge ACF and fBIRN data based on sessionID
  data <- merge(data_fbirn, data_acf, by.x = "sessionID")
  
  # data that are in ACF file but not in fBIRN file
  if (length(which(! data_acf$sessionID %in% data_fbirn$sessionID)) > 0){
    cat('\nThe following sessions have ACF data but no fBIRN QA data:\n')
    print(data_acf[which(! data_acf$sessionID %in% data_fbirn$sessionID),]$sessionID)
  }
  
  # data that are in fBIRN file but not in ACF file
  if (length(which(! data_fbirn$sessionID %in% data_acf$sessionID)) > 0){
    cat('\nThe following session have fBIRN QA data but no ACF data:\n')
    print(data_fbirn[which(! data_fbirn$sessionID %in% data_acf$sessionID),]$sessionID)
  }
  
  # add site and scannermanufacturer from Spotfire data
  data <- merge(data, data_spotfire[,c("sessionID","site","scannermanufacturer")], by = "sessionID")
  # print data that are not in the spotfire file
  if (length(which(! data$sessionID %in% data_spotfire$sessionID)) > 0){
    cat('\nThe following sessions have no Spotfire data:\n')
    print(data[which(! data$sessionID %in% data_spotfire$sessionID),]$sessionID)
  }
  
  # exclude sites with limited data points
  # for (s in levels(data$site)){
  #   subdat <- subset(data,data$site==s)
  #   cat(s,nrow(subdat),'\n')
  # }
  sites <- c("BYC", "CAM", "MCM", "QNS", "SBH", "SMH", "TBR", "TOH", "TWH", "UBC", "UCA", "UTO", "WEU")
  cat('\nThe following sessions/sites excludes due to limited data points:\n')
  print(data[which(!data$site %in% sites),]$sessionID)
  cat('Total of ', sum(!data$site %in% sites), ' sessions exluded.\n')
  data <- subset(data,data$site %in% sites)
  data$site <- factor(data$site) # remove dangling levels
  levels(data$site)
  
  # there are a few siemens scanners in SBH. Exclude them.
  cat('\nThe following SBH sessions were excluded (scannermanufacturer=SIEMENS):\n')
  print(data[which(data$site=="SBH" & data$scannermanufacturer=="SIEMENS"),]$sessionID)
  cat('Total of', sum(data$site=="SBH" & data$scannermanufacturer=="SIEMENS"), 'sessions excluded.\n')
  data <- subset(data,!(data$site=="SBH" & data$scannermanufacturer=="SIEMENS"))
  
  # remove rows with NAs and/or INFs
  # only care about quantitative values
  dataq <- data[,c("mean","SFNR","std","percentFluc","drift","driftfit","rdc",
                   "minFWHMX","minFWHMY","minFWHMZ","maxFWHMX","maxFWHMY","maxFWHMZ",
                   "meanGhost","meanBrightGhost","SNR",
                   "minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
                   "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy",
                   "teq")]
  
  nas <- which(is.na(as.matrix(dataq)), arr.ind = T) # => None
  infs <- which(is.infinite(as.matrix(dataq)), arr.ind = T) # => 154
  rem_ind <- c(nas[,'row'], infs[,'row'])
  
  cat('\nThe following sessions were excluded due to NA and/or INF values:\n')
  print(data[rem_ind,]$sessionID)
  cat('Total of', length(rem_ind), 'sessions excluded.\n')
  data <- data[-rem_ind,]
  
  return(data)
}

