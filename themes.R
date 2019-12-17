# page margins
page_width = 210
page_height = 297
indents = 0.5*c((page_height-243),
                (page_width-170),
                (page_height-243),
                (page_width-170))

# define global plot elements (e.g. horizontal gridlines)
theme_all = theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey55",
                                                    size = 0.15,
                                                    linetype = "dashed"),
                  panel.background = element_rect(fill = "grey92",
                                                  colour = NA),
                  panel.border = element_blank(),
                  # plot.margin = unit(c(2,2,2,2), 'mm'),
                  axis.title.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(size=0.1),
                  legend.key = element_blank(),
                  legend.background = element_rect(colour=NA,
                                                   fill=alpha("grey92", 0.7)))

theme_all = theme_all + theme(
  plot.background = element_blank(),
  # panel.background = element_blank(),
  strip.background = element_blank(),
  legend.justification = c(0.5, 0.5),
  legend.position = c(3/4, 1/6)
)

theme_fig_HIV = theme_all +
  theme(legend.box.margin = margin(),
        legend.box.spacing = margin(),
        legend.key.width = unit(4, "mm"),
        legend.key.height = unit(6.3, "mm"),
        legend.justification = c(1, 0),
        legend.position = c(1, 0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=11, face="bold", margin=margin(r=3)),
        axis.ticks.length = unit(0, "mm"),
        # axis.ticks.x = element_line(size=2),
        plot.title = element_text(size=11.5, margin=margin(b=1)),
        # plot.margin = unit(c(0,0,0,0), "mm"),
        text = element_text(size = 10.5))
