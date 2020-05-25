function(plotdata,
         midquote,
         name,
         legend.labels,
         tick.breaks,
         tick.labels,
         type) {
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