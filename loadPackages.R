usePackage <- function(pk){
  for (p in pk) {
    if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
    library(p, character.only = TRUE)
  }
}
usePackage(c('knitr','R.matlab','data.table','xtable','dplyr','tidyr',
             'broom','ggplot2','ggvis','nlme', 'cowplot','ez'))

theme_set(theme_bw(base_size = 12))

# control legends etc. theme
legend_pos <-function(l) {
  theme( plot.title = element_text(face="bold",size = 12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = l,
         legend.title = element_blank(),
         #         legend.text = element_text(size=12),
         legend.key = element_blank() # switch off the rectangle around symbols
  )
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

options(digits = 3, scipen = 3)
