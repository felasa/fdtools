thm_fd <- function(...) {
  require(ggplot2)
  c("#FFFFFF" ,"#F0F0F0" ,"#D9D9D9" ,"#BDBDBD" ,"#969696" ,"#737373" ,"#525252" ,"#252525", "#000000") -> grey_pal
  base_theme <- 
    theme(rect = element_rect(fill=grey_pal[2]), 
        panel.grid.minor = element_blank(), 
        panel.background=element_rect(fill = grey_pal[2]), 
        panel.grid.major = element_line(color = grey_pal[3]), 
        axis.ticks = element_blank(),
        axis.text = element_text(color=grey_pal[8]),
        axis.title.x = element_text(vjust=0),
        axis.title.y = element_text(vjust=1))
  base_theme + theme(...)
}

thm <- thm_fd()
