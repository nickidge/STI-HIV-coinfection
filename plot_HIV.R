### All plots ###

plot_all_HIV <- function(SID_list){
  

  y_HIV_df = rbind.fill(y_HIV_df, HIV_main_data)
  HIV_df = rbind.fill(HIV_df, HIV_main_data)
  
  y_p = ggplot(data=y_HIV_df, aes(x=year, y=value, group=HIV_group, colour=HIV_group)) +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5))
  HIV_p = ggplot(data=HIV_df, aes(x=year, colour=N, group=N)) +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5))
  
  theme_plot_HIV = theme_all
  
  # plot 1
  all_HIV_1 =
    HIV_p +
    theme_plot_HIV +
    coord_cartesian(ylim=c(0,250)) +
    geom_line(data = subset(HIV_df, N=="model"), aes(y=diagnoses_HIV, group=1)) +
    geom_point(data = subset(HIV_df, N=="data"), aes(y=diagnoses_HIV, group=1), na.rm=TRUE) +
    scale_y_continuous(expand=c(0,0)) +
    labs(y="annual HIV diagnoses", title="diagnoses")
  
  
  
  # plot 2
  all_HIV_2 =
    y_p +
    theme_plot_HIV +
    geom_line(data = subset(y_HIV_df, N=="model" & HIV_group=="HIV_plus"), aes(y=value, group=1)) +
    geom_point(data = subset(y_HIV_df, N=="data"), aes(y=value, group=1), na.rm=TRUE) +
    labs(y="number of people with HIV", title="PL HIV")
  
  
  # cascade
  HIV_cascade = array(0, dim=c(dim(y)[1], 5))
  
  HIV_cascade[,1] = seq(first_year, last_year, by=dt)
  HIV_cascade[,2] = y[,9,"pop_sti"] / y[,13,"pop_sti"]
  HIV_cascade[,3] = y[,4,"pop_sti"] / rowSums(y[,3:4,"pop_sti"])
  HIV_cascade[,4] = rowSums(y[,6:7,"pop_sti"]) / y[,4,"pop_sti"]
  HIV_cascade[,5] = y[,7,"pop_sti"] / rowSums(y[,6:7,"pop_sti"])
  
  HIV_cascade = as.data.frame(HIV_cascade)
  colnames(HIV_cascade) = c("year", "prop_HIV", "prop_D1", "prop_D2", "prop_D3")
  # HIV_cascade$N = "model"
  
  # plot 3
  all_HIV_3 =
    ggplot(data = HIV_cascade, aes(x=year, y=prop_HIV, group=1)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
    theme_plot_HIV +
    labs(y="proportion of MSM who have HIV", title="HIV as proportion of MSM") +
    coord_cartesian(ylim=c(0,0.3))

  
  #plot 4
  all_HIV_4 =
    HIV_p +
    geom_line(data = subset(HIV_df, N=="model"), aes(y=incidence_HIV/HIV_df$incidence_HIV[1], group=N)) +
    geom_point(data = subset(HIV_df, N=="data"), aes(y=diagnoses_HIV/HIV_df$incidence_HIV[1], group=N), na.rm=TRUE) +
    theme_plot_HIV +
    coord_cartesian(ylim=c(0,250)) +
    labs(y="relative incidence compared to 2007", title="relative annual incidence")
    
  
  
  #plot 5
  all_HIV_5 =
    HIV_p +
    geom_line(data = subset(HIV_df, N=="model"), aes(y=incidence_HIV, group=1)) +
    geom_point(data = subset(HIV_df, N=="data"), aes(y=diagnoses_HIV, group=1), na.rm=TRUE) +
    theme_plot_HIV +
    coord_cartesian(ylim=c(0, 270)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(y="annual incidence of HIV", title="annual incidence")
  
  
  HIV_cascade_melt = melt(HIV_cascade, id.vars = 'year', variable.name = 'value')
  colnames(HIV_cascade_melt)[2] = 'prop'
  
  #plot 6
  all_HIV_6 =
    ggplot(data=subset(HIV_cascade_melt, prop=="prop_D1" | prop=="prop_D2" | prop=="prop_D3"), aes(x=year, y=value, group=prop, colour=prop)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(labels = scales::percent, expand=c(0,0), breaks= c(0.2*0:5)) +
    scale_colour_discrete(labels=c("Prop. diagnosed", "Prop. diagnosed on treatment", "Prop. on treatment virally\nsuppressed")) +
    coord_cartesian(ylim=c(0,1)) +
    theme_plot_HIV +
    theme(legend.title=element_blank(),
          legend.background = element_rect(colour=NA,
                                           fill=alpha("grey92", 0.7)),
          legend.position=c(1,0.5),
          legend.justification=c(1,0.5)) +
    labs(y="Care cascade", title="Diagnosis proportions")
  
  
  #plot 7
  all_HIV_7 =
    y_p +
    theme_plot_HIV +
    geom_line(data = subset(y_HIV_df, N=="model" & (HIV_group=="HIV_minus" | HIV_group=="I" | HIV_group=="D")), aes(y=value, group=HIV_group)) +
    coord_cartesian(ylim=c(0,max(y_HIV_df$value, na.rm=T))) +
    scale_y_continuous(expand=c(0,0))
    
  
  
  #plot 8
  prep_df = subset(y_HIV_df, HIV_group=="S2")
  prep_df$value = prep_df$value / subset(y_HIV_df, HIV_group=="HIV_minus")$value
  
  all_HIV_8 =
    y_p +
    geom_line(data = prep_df, aes(y=value, group=1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       labels = scales::percent) +
    theme_plot_HIV +
    labs(y="proportion of S on PrEP", title="PrEP proportion") +
    coord_cartesian(ylim=c(0,1))
    
  
  #plot 9
  all_HIV_9 =
    y_p +
    theme_plot_HIV +
    scale_y_continuous(expand=c(0,0)) +
    geom_line(data=subset(y_HIV_df, (HIV_group=="pop_HIV" | HIV_group=="I" | HIV_group=="D" | HIV_group=="D3") & sti_group=="pop_sti"))
  
  
  
  # all_HIV_grobs = list(all_HIV_1, all_HIV_2, all_HIV_3, all_HIV_5, all_HIV_6, all_HIV_7, all_HIV_8, all_HIV_9)
  # 
  # all_HIV = grid.arrange(grobs=all_HIV_grobs)
  
  all_HIV = invisible(ggarrange(all_HIV_1, all_HIV_2, all_HIV_3, all_HIV_5, all_HIV_6, all_HIV_7, all_HIV_8, all_HIV_9, nrow=3, ncol=3))
  
  all_HIV = annotate_figure(all_HIV, top = text_grob("HIV plots", face = "bold", size = 24))
  ggsave("all_HIV_plots.pdf", plot=all_HIV, width = 14, height=8)
  # ggsave("all_HIV_plots.png", plot=all_HIV, width = 14, height=8)
  browseURL("all_HIV_plots.pdf")
  
}

plot_all_HIV(SID_list)
