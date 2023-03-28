#-----------------------------------------------------------------------------#
# Title  : Figure themes
# Purpose:
# Author :
# Date   : March 28, 2023
#-----------------------------------------------------------------------------#

require(ggplot2)

#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
theme_coef <- function(no_legend = FALSE){
  out <- theme(
    panel.background   = element_rect(fill = NA, color = "black"),
    panel.grid.major.x = element_line(color     = "gray80",
                                      linetype  = "dotted",
                                      linewidth = 0.25),
    axis.title.x       = element_blank(),
    legend.position    = "bottom"
  )

  if(no_legend){
    out <- out + theme(legend.position = "none")
  }
  return(out)
}
#-----------------------------------------------------------------------------#
