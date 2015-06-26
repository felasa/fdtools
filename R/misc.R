require(RColorBrewer)
require(ggplot2)
RColorBrewer::brewer.pal(9, "Greys") -> grey_pal
theme(rect = element_rect(fill=grey_pal[2]), panel.grid.minor = element_blank(), 
      panel.background=element_rect(fill = grey_pal[2]), 
      panel.grid.major = element_line(color = grey_pal[3]), text = element_text(family = "mono"),
      axis.ticks = element_blank(),
      axis.text = element_text(size=14, color=grey_pal[8]),
      axis.title = element_text(size=16, face="bold")) -> thm