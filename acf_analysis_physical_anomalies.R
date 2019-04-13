physical_anomalies <- function(data,sites,c=5){
  out_sessions <- list()
  data_out <- NULL
  for (s in sites){
    data_site = subset(data, data$site==s)
    anomaly = subset(data_site, data_site$maxFWHMx > c*data_site$minFWHMx |
                       + data_site$maxFWHMy > c*data_site$minFWHMy)
    out_sessions[[s]] <- paste(anomaly$sessionID)
    data_site_out <- subset(data_site, !(data_site$maxFWHMx > c*data_site$minFWHMx | data_site$maxFWHMy > c*data_site$minFWHMy))
    data_out <- rbind(data_out,data_site_out)
  }
  return(list("data_out"=data_out, "out_sessions"=out_sessions))
}

print_physical_anomalies <- function(data,sites,c=5){
  anom <- physical_anomalies(data = data, sites = sites, c = c)
  for (s in sites){
    data_site = subset(data, data$site==s)
    #anomaly = subset(data_site, data_site$maxFWHMx > c*data_site$minFWHMx & data_site$maxFWHMy > c*data_site$minFWHMy)
    print(s)
    cat(length(anom$out_sessions[[s]]), ' anomalies out of ', length(data_site$sessionID),'\n')
    print(anom$out_sessions[[s]])
  }
}