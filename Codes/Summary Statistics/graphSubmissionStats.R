function(plotdata,
         midquote,
         name,
         legend.labels,
         tick.breaks,
         tick.labels,
         type) {
  #' Graph submissions data.
  #'
  #' @description This function graphs a barplot of submissions data (e.g., intradaily or time series),
  #' split into n categories (e.g., anonymous vs. attributed; buy vs. sell), overlayed with a line graph of the 
  #' evolution of the midquote.
  #' 
  #' Define by n the number of categories and t the number of timepoints.
  #'
  #' @param plotdata Dataframe with n*t rows and 3 columns: (1) plotdata$t contains the timepoints; 
  #' (2) plotdata$variable contains the category; and (3) plotdata$value contains the data value (i.e., the number
  #' of submissions belonging to category n at time t)
  #' @param midquote Vector of length t containg the midquotes at each timepoint. 
  #' @param name String containing the title of the plot.
  #' @param legend.labels String vector of length n with category names to be included in the plot legend.
  #' @param tick.breaks Vector of length m*t, 0<m<=1, containg positions of the x-axis ticks.
  #' @param tick.labels String vector of the same length as tick.breaks containg the corresponding x-axis tick labels.
  #' @param type String, either "fill", which produces a filled bar plot (i.e., relative values such that summing
  #' across categories at each time point is equal to one); or "stack", which produces a stacked bar plot.
  #'
  #' @usage graphSubmissionStats(plotdata,  midquote,  name,  legend.labels,  tick.breaks,  tick.labels,  type)
  #' 
  #' @return p, a ggplot2 object.
  
  if (type == "fill") {
    #scaling factor for second y-axis
    scalefact1 <- min(midquote, na.rm = T)
    scalefact2 <- max(midquote - scalefact1, na.rm = T)
    ylabel <- "Submission Ratio"
  }
  
  if (type == "stack") {
    #scaling factor for second y-axis
    plotdata$value <- plotdata$value / 1000000
    ylabel <- "Submission ($mil)"
    scalefact1 <- min(midquote, na.rm = T)
    scalefact2 <-
      max(midquote - scalefact1, na.rm = T) / max(plotdata$value, na.rm = T)
  }
  
  #plot
  p <- ggplot(plotdata, aes(x = factor(t), y = value, fill = variable)) +
    xlab("") +
    ylab("") +
    ggtitle(name) +
    geom_bar(position = type,
             stat = "identity",
             width = 1) +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name = "Dose", labels = legend.labels) +
    scale_x_discrete(breaks = tick.breaks,
                     labels = tick.labels) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.title = element_blank()) +
    geom_line(aes(
      y = rep((midquote - scalefact1) / scalefact2, length(legend.labels)),
      group = 1,
      color = "blackline"
    )) +
    scale_y_continuous(sec.axis = sec_axis(
      trans = ~ (. * scalefact2) + scalefact1 ,
      name = "Midquote"
    )) +
    scale_color_manual('', labels = 'Midquote', values = 'black')
  
  return(p)
  
  
}