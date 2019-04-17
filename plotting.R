library(ggplot2)
library(ggrepel)
library(plotly)
library(ggcorrplot)
library(gridExtra)

########################################
## This function creates PCA biplots
########################################
pca_biplot <- function(data,pca,time_course=F, var_axes=T, plot_title='', save_png=F, var_axes_separate=F){
  # inputs:
  #       data: dataframe containing data
  #       pca: pca object- output of prcomp
  #       time_course: whether or not connect sessions in temporal order
  #       var_axes: whether or not plot variable axes
  #       plot_title: title of the plot
  #       save_png: whether or not to save the plot in a png file ('<plot_title>_pca.png')
  #       var_axes_separate: whether to plot variable axes on top of the pca plot or in a separate plot
  # output:
  #       if var_axes=T a ggplot object is returned
  #       if var_axes=F a plotly object is returned  
  
  # p2 is only used if separate var_axes plot requested
  p2 <- NULL
  
  p <- ggplot(data= data.frame(pca$x), mapping = aes(x=PC1,y=PC2))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_vline(xintercept = 0)
  p <- p + geom_point(aes(colour=data$site,shape=data$scannermanufacturer,label=data$sessionID), alpha=0.7) # label just added for plotly  if
  p <- p + stat_ellipse(mapping = aes(colour=data$site),level = 0.95, alpha=0.7)
  p <- p + theme(legend.title=element_blank(), legend.position = 'right')
  var_pc1=pca$sdev[1]^2/sum(pca$sdev^2)
  var_pc2=pca$sdev[2]^2/sum(pca$sdev^2)
  p <- p + xlab(paste('PC1 (',round(var_pc1*100),'% var. explained)',sep=""))
  p <- p + ylab(paste('PC2 (',round(var_pc2*100),'% var. explained)',sep=""))
  p <- p + ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (time_course){
    p <- p + geom_path(data= data.frame(pca$x), mapping = aes(colour=data$site,shape=data$scannermanufacturer,label=data$sessionID), # shape and label just added for plotly
                       arrow = arrow(type = 'closed',length = unit(x = 0.3,units = 'cm')), alpha=0.7)    
  }
  
  if (var_axes){
    if (!var_axes_separate){
      eigvecs = pca$rotation * 20
      p <- p + geom_segment(data= data.frame(eigvecs), mapping = aes(x=0,y=0,xend=PC1,yend=PC2), colour = 'red',arrow = arrow(type = 'closed',length = unit(x = 0.2,units = 'cm')), alpha=1)+
        #geom_label(data= data.frame(eigvecs), mapping = aes(x=PC1,y=PC2),label=rownames(pca$rotation))
        geom_label_repel(data= data.frame(eigvecs), mapping = aes(x=PC1,y=PC2),label=rownames(pca$rotation), alpha = 0.7)
    } else{
      eigvecs = pca$rotation * 8
      p2 <- ggplot(data= data.frame(pca$x), mapping = aes(x=PC1,y=PC2))
      p2 <- p2 + geom_hline(yintercept = 0)
      p2 <- p2 + geom_vline(xintercept = 0)
      var_pc1=pca$sdev[1]^2/sum(pca$sdev^2)
      var_pc2=pca$sdev[2]^2/sum(pca$sdev^2)
      p2 <- p2 + xlab(paste('PC1 (',round(var_pc1*100),'% var. explained)',sep=""))
      p2 <- p2 + ylab(paste('PC2 (',round(var_pc2*100),'% var. explained)',sep=""))
      p2 <- p2 + ggtitle(plot_title) +
        theme(plot.title = element_text(hjust = 0.5))
      p2 <- p2 + geom_segment(data= data.frame(eigvecs), mapping = aes(x=0,y=0,xend=PC1,yend=PC2), colour = 'red',arrow = arrow(type = 'closed',length = unit(x = 0.2,units = 'cm')))+
        #geom_label(data= data.frame(eigvecs), mapping = aes(x=PC1,y=PC2),label=rownames(pca$rotation))
        geom_label_repel(data= data.frame(eigvecs), mapping = aes(x=PC1,y=PC2),label=rownames(pca$rotation))
    }
  }
  
  if (save_png){
    filename <- paste(plot_title,'_pca',sep = '')
    if (var_axes & !var_axes_separate){
      filename <- paste(filename,'_varaxes',sep = '')
    }
    if (time_course){
      filename <- paste(filename,'_timecourse',sep = '')
    }
    png(filename=paste(filename,'.png',sep = ''), width = 24, height = 16, units = "cm", res = 600)
    print(p)
    dev.off()
    if (var_axes & var_axes_separate){
      png(filename=paste(filename,'_variables.png',sep = ''), width = 10, height = 10, units = "cm", res = 600)
      print(p2)
      dev.off()
    }
  }
  
  if (!var_axes | var_axes_separate){ # var_axes does not work with ggplotly
    p <- ggplotly(p)
  } 
  return(list("biplot"=p,"var_axis_plot"=p2))
}

##################################
# extract legend
# from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
##################################
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

########################################
# this function creates ggplots objects of barplots of the FWHM values.
# the barplots are not plotted, but returned for other functions to plot them.
########################################
get_fwhm_sub_barplots <- function(data){
  
  # set the following to change the title/text size for the y axis
  y_title_size = 10
  y_text_size = 10
  
  # FWHM
  max_fwhm <- max(data$maxFWHMx, data$maxFWHMy)
  
  p_maxFWHMx <- ggplot(data = data, mapping = aes(x = sessionID, y = maxFWHMx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))

  p_minFWHMx <- ggplot(data = data, mapping = aes(x = sessionID, y = minFWHMx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_medFWHMx <- ggplot(data = data, mapping = aes(x = sessionID, y = medFWHMx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_q1FWHMx <- ggplot(data = data, mapping = aes(x = sessionID, y = q1FWHMx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_q3FWHMx <- ggplot(data = data, mapping = aes(x = sessionID, y = q3FWHMx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_maxFWHMy <- ggplot(data = data, mapping = aes(x = sessionID, y = maxFWHMy)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_minFWHMy <- ggplot(data = data, mapping = aes(x = sessionID, y = minFWHMy)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_medFWHMy <- ggplot(data = data, mapping = aes(x = sessionID, y = medFWHMy)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_q1FWHMy <- ggplot(data = data, mapping = aes(x = sessionID, y = q1FWHMy)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_q3FWHMy <- ggplot(data = data, mapping = aes(x = sessionID, y = q3FWHMy)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  # fracAnomalies
  p_fracAnomaliesx <- ggplot(data = data, mapping = aes(x = sessionID, y = fracAnomaliesx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    ylim(1,max(data$fracAnomaliesx,data$fracAnomaliesy))
  
  p_fracAnomaliesy <- ggplot(data = data, mapping = aes(x = sessionID, y = fracAnomaliesy)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    ylim(1,max(data$fracAnomaliesx,data$fracAnomaliesy))
  
  # AnomalyPerVol
  maxAnomalyPerVol <- max(data$q3AnomalyPerVolx,data$q3AnomalyPerVoly)
                          
  p_medAnomalyPerVolx <- ggplot(data = data, mapping = aes(x = sessionID, y = medAnomalyPerVolx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    ylim(0,max(data$q3AnomalyPerVolx))+
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,maxAnomalyPerVol))
  
  p_q1AnomalyPerVolx <- ggplot(data = data, mapping = aes(x = sessionID, y = q1AnomalyPerVolx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    ylim(0,max(data$q3AnomalyPerVolx))+
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,maxAnomalyPerVol))
  
  p_q3AnomalyPerVolx <- ggplot(data = data, mapping = aes(x = sessionID, y = q3AnomalyPerVolx)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    ylim(0,max(data$q3AnomalyPerVolx))+
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,maxAnomalyPerVol))

  p_medAnomalyPerVoly <- ggplot(data = data, mapping = aes(x = sessionID, y = medAnomalyPerVoly)) +
    ylim(0,max(data$q3AnomalyPerVoly))+
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,maxAnomalyPerVol))
  
  p_q1AnomalyPerVoly <- ggplot(data = data, mapping = aes(x = sessionID, y = q1AnomalyPerVoly)) +
    ylim(0,max(data$q3AnomalyPerVoly))+
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,maxAnomalyPerVol))
  
  p_q3AnomalyPerVoly <- ggplot(data = data, mapping = aes(x = sessionID, y = q3AnomalyPerVoly)) +
    ylim(0,max(data$q3AnomalyPerVoly))+
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,maxAnomalyPerVol))
  
  # numAnomaly
  p_numAnomaly_x <- ggplot(data = data, mapping = aes(x = sessionID, y = numAnomaly_x)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max(data$numAnomaly_x, data$numAnomaly_y)))
  
  p_numAnomaly_y <- ggplot(data = data, mapping = aes(x = sessionID, y = numAnomaly_y)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(),
                            axis.title.y = element_text(size = y_title_size),
                            axis.text.y = element_text(size = y_text_size),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max(data$numAnomaly_x, data$numAnomaly_y)))
  
  return(list("p_maxFWHMx"=p_maxFWHMx,
              "p_minFWHMx"=p_minFWHMx,
              "p_medFWHMx"=p_medFWHMx,
              "p_q1FWHMx"=p_q1FWHMx,
              "p_q3FWHMx"=p_q3FWHMx,
              "p_q3FWHMx"=p_q3FWHMx,
              "p_fracAnomaliesx"=p_fracAnomaliesx,
              "p_medAnomalyPerVolx"=p_medAnomalyPerVolx,
              "p_q1AnomalyPerVolx"=p_q1AnomalyPerVolx,
              "p_q3AnomalyPerVolx"=p_q3AnomalyPerVolx,
              "p_maxFWHMy"=p_maxFWHMy,
              "p_minFWHMy"=p_minFWHMy,
              "p_medFWHMy"=p_medFWHMy,
              "p_q1FWHMy"=p_q1FWHMy,
              "p_q3FWHMy"=p_q3FWHMy,
              "p_q3FWHMy"=p_q3FWHMy,
              "p_fracAnomaliesy"=p_fracAnomaliesy,
              "p_medAnomalyPerVoly"=p_medAnomalyPerVoly,
              "p_q1AnomalyPerVoly"=p_q1AnomalyPerVoly,
              "p_q3AnomalyPerVoly"=p_q3AnomalyPerVoly,
              "p_numAnomaly_x"=p_numAnomaly_x,
              "p_numAnomaly_y"=p_numAnomaly_y))
}

#############################################
# This function creates barplots of the FWHM values.
# Two barplots are created, one in the x direction, and one in the y direction.
#############################################
fwhm_barplot <- function(data, plot_title='', save_png=F){
  # inputs:
  #       data: dataframe containing data
  #       plot_title: title of the plot
  #       save_png: whether or not to save the plot in a png file ('<plot_title>_pca.png')
  # output:
  #       ggplotly objects
  
  p <- get_fwhm_sub_barplots(data)
  
  p$p_maxFWHMx <- p$p_maxFWHMx + theme(legend.title=element_blank())
  mylegend <- g_legend(p$p_maxFWHMx)

  if (save_png){
    png(filename=paste(plot_title,'_barplots_FWHMx.png',sep = ''), width = 24, height = 18, units = "cm", res = 600)
    grid.arrange(arrangeGrob(p$p_maxFWHMx + theme(legend.position="none"),
                             p$p_minFWHMx + theme(legend.position="none"),
                             p$p_medAnomalyPerVolx + theme(legend.position="none"),
                             p$p_q1AnomalyPerVolx + theme(legend.position="none"),
                             p$p_q3AnomalyPerVolx + theme(legend.position="none"),
                             nrow=5),
                 mylegend,
                 ncol=2, widths=c(5,1))
    dev.off()
  }

  p_FWHMx <- subplot(p$p_maxFWHMx + theme(legend.position="none"),
                     p$p_minFWHMx + theme(legend.position="none"),
                     p$p_medAnomalyPerVolx + theme(legend.position="none"),
                     p$p_q1AnomalyPerVolx + theme(legend.position="none"),
                     p$p_q3AnomalyPerVolx + theme(legend.position="none"),
                     nrows = 5, titleY = T, shareX = T) %>% layout(title = paste(plot_title, 'FWHMx', sep = " "))
  

  
  p$p_maxFWHMy <- p$p_maxFWHMy + theme(legend.title=element_blank())
  mylegend <- g_legend(p$p_maxFWHMy)
  
  if (save_png){
    png(filename=paste(plot_title,'_barplots_FWHMy.png',sep = ''), width = 24, height = 18, units = "cm", res = 600)
    grid.arrange(arrangeGrob(p$p_maxFWHMy + theme(legend.position="none"),
                             p$p_minFWHMy + theme(legend.position="none"),
                             p$p_medAnomalyPerVoly + theme(legend.position="none"),
                             p$p_q1AnomalyPerVoly + theme(legend.position="none"),
                             p$p_q3AnomalyPerVolx + theme(legend.position="none"),
                             nrow=5),
                 mylegend,
                 ncol=2, widths=c(5,1))
    dev.off()
  }
  
  p_FWHMy <- subplot(p$p_maxFWHMy + theme(legend.position="none"),
                     p$p_minFWHMy + theme(legend.position="none"),
                     p$p_medAnomalyPerVoly + theme(legend.position="none"),
                     p$p_q1AnomalyPerVoly + theme(legend.position="none"),
                     p$p_q3AnomalyPerVoly + theme(legend.position="none"),
                     nrows = 5, titleY = T, shareX = T) %>% layout(title = paste(plot_title, 'FWHMy', sep = " "))

  if (save_png){
    png(filename=paste(plot_title,'_barplots_acf.png',sep = ''), width = 24, height = 18, units = "cm", res = 600)
    grid.arrange(arrangeGrob(p$p_maxFWHMx + theme(legend.position="none"),
                             p$p_maxFWHMy + theme(legend.position="none"),
                             p$p_minFWHMx + theme(legend.position="none"),
                             p$p_minFWHMy + theme(legend.position="none"),
                             p$p_medFWHMx + theme(legend.position="none"),
                             p$p_medFWHMy + theme(legend.position="none"),
                             p$p_q1FWHMx + theme(legend.position="none"),
                             p$p_q1FWHMy + theme(legend.position="none"),
                             p$p_q3FWHMx + theme(legend.position="none"),
                             p$p_q3FWHMy + theme(legend.position="none"),
                             ncol = 2, nrow = 5),
                 mylegend,
                 ncol=2, widths=c(5,1))
    dev.off()
  }
  
  return(list("p_FWHMx"=p_FWHMx, "p_FWHMy"=p_FWHMy))
}

#############################################
# This function creates barplots of the fBIRN QA values.
#############################################
fbirn_barplot <- function(data, plot_title='', save_png=F){
  # inputs:
  #       data: dataframe containing data
  #       plot_title: title of the plot
  #       save_png: whether or not to save the plot in a png file ('<plot_title>_pca.png')
  # output:
  #       ggplotly object
  
  # bar plot of FWHM
  max_fwhm <- max(data$maxFWHMX, data$maxFWHMY) # used for y-axis range
  
  p_maxFWHMX <- ggplot(data = data, mapping = aes(x = sessionID, y = maxFWHMX)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_minFWHMX <- ggplot(data = data, mapping = aes(x = sessionID, y = minFWHMX)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
                            axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_maxFWHMY <- ggplot(data = data, mapping = aes(x = sessionID, y = maxFWHMY)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))
  
  p_minFWHMY <- ggplot(data = data, mapping = aes(x = sessionID, y = minFWHMY)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) +
    scale_y_log10(limits = c(1,max_fwhm))

  p_maxFWHMZ <- ggplot(data = data, mapping = aes(x = sessionID, y = maxFWHMZ)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  
  p_minFWHMZ <- ggplot(data = data, mapping = aes(x = sessionID, y = minFWHMZ)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  
  p_mean <- ggplot(data = data, mapping = aes(x = sessionID, y = mean)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

  p_snr <- ggplot(data = data, mapping = aes(x = sessionID, y = SNR)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  
  p_sfnr <- ggplot(data = data, mapping = aes(x = sessionID, y = SFNR)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  
  p_std <- ggplot(data = data, mapping = aes(x = sessionID, y = std)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  
  p_percentfluc <- ggplot(data = data, mapping = aes(x = sessionID, y = percentFluc)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  
  p_drift <- ggplot(data = data, mapping = aes(x = sessionID, y = drift)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

  p_driftfit <- ggplot(data = data, mapping = aes(x = sessionID, y = driftfit)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

  p_rdc <- ggplot(data = data, mapping = aes(x = sessionID, y = rdc)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

  p_meanGhost <- ggplot(data = data, mapping = aes(x = sessionID, y = meanGhost)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

  p_meanBrightGhost <- ggplot(data = data, mapping = aes(x = sessionID, y = meanBrightGhost)) +
    geom_bar(aes(fill = data$site, colour = data$site),stat = "identity") +
    theme_classic() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

  p_maxFWHMX <- p_maxFWHMX + theme(legend.title=element_blank())
  mylegend <- g_legend(p_maxFWHMX)
  
  # do not plot FWHMZ (since it is irrelevant to 2D acquisitions)
  if (save_png){
    png(filename=paste(plot_title,'_barplots_fbirn.png',sep = ''), width = 24, height = 26, units = "cm", res = 600)
    grid.arrange(arrangeGrob(p_mean + theme(legend.position="none"),
                             p_std + theme(legend.position="none"),
                             p_snr + theme(legend.position="none"),
                             p_sfnr + theme(legend.position="none"),
                             p_percentfluc + theme(legend.position="none"),
                             p_rdc + theme(legend.position="none"),
                             p_drift + theme(legend.position="none"),
                             p_driftfit + theme(legend.position="none"),
                             p_meanGhost + theme(legend.position="none"),
                             p_meanBrightGhost + theme(legend.position="none"),
                             p_maxFWHMX + theme(legend.position="none"),
                             p_minFWHMX + theme(legend.position="none"),
                             p_maxFWHMY + theme(legend.position="none"),
                             p_minFWHMY + theme(legend.position="none"),
                             nrow = 7, ncol = 2),
                 mylegend,
                 ncol=2, widths=c(9,1))
    dev.off()
  }
  
  # plotting FWHMZ here just so that I can see them, though not showing in the paper
  p <- subplot(p_mean + theme(legend.position="none"),
               p_std + theme(legend.position="none"),
               p_snr + theme(legend.position="none"),
               p_sfnr + theme(legend.position="none"),
               p_percentfluc + theme(legend.position="none"),
               p_drift + theme(legend.position="none"),
               p_driftfit + theme(legend.position="none"),
               p_rdc + theme(legend.position="none"),
               p_meanGhost + theme(legend.position="none"),
               p_meanBrightGhost + theme(legend.position="none"),
               p_maxFWHMX + theme(legend.position="none"),
               p_minFWHMX + theme(legend.position="none"),
               p_maxFWHMY + theme(legend.position="none"),
               p_minFWHMY + theme(legend.position="none"),
               p_maxFWHMZ + theme(legend.position="none"),
               p_minFWHMZ + theme(legend.position="none"),
               nrows = 4, titleY = T, shareX = T) %>% layout(title = paste(plot_title, 'fBIRN QA', sep = " "))
  
  return(p)
}

##########################################
## This function creates correlation plots
##########################################
correlation_plot <- function(data, plot_title='', save_png=F){
  # inputs:
  #       data: dataframe containing data
  #       plot_title: title of the plot
  #       save_png: whether or not to save the plot in a png file ('<plot_title>_pca.png')
  # output:
  #       ggplotly object
  
  # compute correlation matrix
  cormat <- cor(data)
  # plot correlation matrix
  p <- ggcorrplot(cormat) +
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0.5))
  if (save_png){
    png(filename=paste(plot_title,'_corr_plot.png',sep = ''), width = 24, height = 24, units = "cm", res = 600)
    print(p)
    dev.off()
  }
  p <- ggplotly(p)
  return(p)
}

plotdata <- function(data,
                     show.fwhm_barplot = T,
                     show.fbirn_barplot = T,
                     show.corr_plot = T,
                     show.pca_all = T,
                     show.pca_fbirn = T,
                     show.pca_acf = T,
                     show.pca.varaxes = F,
                     show.pca.timecourse = T,
                     pca.var_axes_separate=F,
                     plot_title='',
                     save_png=F){
  if (show.fwhm_barplot){
    p <- fwhm_barplot(data,plot_title = plot_title, save_png = save_png)
    print(p)
  }
  if (show.fbirn_barplot){
    p <- fbirn_barplot(data,plot_title = plot_title, save_png = save_png)
    print(p)
  }  
  if (show.corr_plot){
    ### Correlation Analysis- including 16 fBIRN parameters
    # get QA params.
    qaparams <- data[c("mean","std","SNR","SFNR","percentFluc","drift","driftfit","rdc",
                       "minFWHMX","maxFWHMX","minFWHMY","maxFWHMY",
                       "meanGhost","meanBrightGhost",
                       "minFWHMx","maxFWHMx","medFWHMx","q1FWHMx","q3FWHMx","meanFWHMx","stdFWHMx",
                       "minFWHMy","maxFWHMy","medFWHMy","q1FWHMy","q3FWHMy","meanFWHMy","stdFWHMy")]
    p <- correlation_plot(qaparams,plot_title = plot_title, save_png = save_png)
    print(p)
  }
  if (show.pca_all){
    pca <- prcomp(~minFWHMx+maxFWHMx+medFWHMx+q1FWHMx+q3FWHMx+
                    minFWHMy+maxFWHMy+medFWHMy+q1FWHMy+q3FWHMy+
                    mean+SFNR+std+percentFluc+drift+driftfit+rdc+minFWHMX+minFWHMY+maxFWHMX+maxFWHMY+meanGhost+meanBrightGhost+SNR,
                  data = data,
                  center = T,
                  scale. = T)
    p <- pca_biplot(data,pca,var_axes = show.pca.varaxes,
                    time_course = show.pca.timecourse,
                    plot_title = paste(plot_title,'fbirn+acf',sep='_'),
                    save_png = save_png,
                    var_axes_separate = pca.var_axes_separate)
    print(p)
  }
  if (show.pca_fbirn){
    pca <- prcomp(~mean+SFNR+std+percentFluc+drift+driftfit+rdc+minFWHMX+minFWHMY+maxFWHMX+maxFWHMY+meanGhost+meanBrightGhost+SNR,
                  data = data,
                  center = T,
                  scale. = T)
    p <- pca_biplot(data,pca,var_axes = show.pca.varaxes,
                    time_course = show.pca.timecourse,
                    plot_title = paste(plot_title,'fbirn',sep='_'),
                    save_png = save_png,
                    var_axes_separate = pca.var_axes_separate)
    print(p)
  }
  if (show.pca_acf){
    pca <- prcomp(~minFWHMx+maxFWHMx+medFWHMx+q1FWHMx+q3FWHMx+
                    minFWHMy+maxFWHMy+medFWHMy+q1FWHMy+q3FWHMy,
                  data = data,
                  center = T,
                  scale. = T)
    p <- pca_biplot(data,pca,var_axes = show.pca.varaxes,
                    time_course = show.pca.timecourse,
                    plot_title = paste(plot_title,'acf',sep='_'),
                    save_png = save_png,
                    var_axes_separate = pca.var_axes_separate)
    print(p)
  }
  
}
  