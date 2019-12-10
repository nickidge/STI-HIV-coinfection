### All plots ###

plot_all_sti <- function(SID_list){


  y_sti_df = rbind.fill(y_sti_df, sti_main_data)
  sti_df = rbind.fill(sti_df, sti_main_data)
  
  
  y_p = ggplot(data=y_sti_df, aes(x=year, y=value, group=HIV_group, colour=HIV_group))
  sti_p = ggplot(data=sti_df, aes(x=year, colour=N, group=N))
  

  sti_by_testing = SID_list[[5]]
  all_df = SID_list[[6]]
  
  theme_plot_sti = theme_all
  
  maxval_sti_1 = max(subset(sti_df, sti_group=="diagnoses_sti")$value, na.rm=T)
  maxval_sti_2 = max(subset(y_df, sti_group=="sti_plus")$value)
  
  # plot 1
  all_sti_1 =
    sti_p +
    geom_line(data = subset(sti_df, N=="model" & sti_group=="diagnoses_sti" & (HIV_group=="pop_HIV" | HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(y=value, group=HIV_group, colour=HIV_group)) +
    geom_point(data = subset(sti_df, N=="data" & sti_group=="diagnoses_sti"), aes(y=value, group=HIV_group, colour=HIV_group), na.rm=TRUE) +
    theme_plot_sti +
    labs(y="annual sti diagnoses", title="diagnoses") +
    coord_cartesian(ylim=c(0,maxval_sti_1)) +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(expand=c(0,0))
  
  
  # plot 2
  all_sti_2 =
    y_p +
    geom_line(data = subset(y_df, sti_group=="sti_plus" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value, group=HIV_group, colour=HIV_group)) +
    geom_point(data = subset(y_sti_df, N=="data" & sti_group=="sti_plus"), aes(y=value, group=HIV_group, colour=HIV_group), na.rm=TRUE) +
    theme_plot_sti +
    labs(y="number of people with sti", title="PL sti") +
    coord_cartesian(ylim=c(0,maxval_sti_2)) +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(expand=c(0,0))
  
  
  
  
  # cascade
  sti_cascade = array(0, dim=c(dim(y)[1], 5, 5))
  
  # sti_cascade[,1,1] = seq(first_year, last_year, by=dt)
  
  dimnames(sti_cascade)[[1]] = seq(first_year, last_year, by=dt)
  
  
  
  sti_cascade[,,1] = y[,c(1,2,8,9,13),7] / y[,c(1,2,8,9,13),"pop_sti"]
  sti_cascade[,,2] = y[,c(1,2,8,9,13),2] / y[,c(1,2,8,9,13),7]
  sti_cascade[,,3] = y[,c(1,2,8,9,13),3] / y[,c(1,2,8,9,13),7]
  sti_cascade[,,4] = y[,c(1,2,8,9,13),4] / y[,c(1,2,8,9,13),7]
  sti_cascade[,,5] = y[,c(1,2,8,9,13),5] / y[,c(1,2,8,9,13),7]
  
  dimnames(sti_cascade) = list(seq(first_year, last_year, by=dt), c("S1", "S2", "HIV_minus", "HIV_plus", "pop_HIV"), c("prop_sti", "prop_E", "prop_Sy", "prop_ASy", "prop_T"))

  
  sti_cascade = as.data.frame.table(sti_cascade)
  colnames(sti_cascade) = c("year", "HIV_group", "sti_group", "value")
  
  
  
  
  sti_cascade_all = array(0, dim=c(dim(y)[1], 5, 5))
  
  # sti_cascade[,1,1] = seq(first_year, last_year, by=dt)
  
  dimnames(sti_cascade_all)[[1]] = seq(first_year, last_year, by=dt)
  
  sti_cascade_all[,,1] = y[,c(1,2,8,9,13),1] / y[,c(1,2,8,9,13),"pop_sti"]
  sti_cascade_all[,,2] = y[,c(1,2,8,9,13),2] / y[,c(1,2,8,9,13),"pop_sti"]
  sti_cascade_all[,,3] = y[,c(1,2,8,9,13),3] / y[,c(1,2,8,9,13),"pop_sti"]
  sti_cascade_all[,,4] = y[,c(1,2,8,9,13),4] / y[,c(1,2,8,9,13),"pop_sti"]
  sti_cascade_all[,,5] = y[,c(1,2,8,9,13),5] / y[,c(1,2,8,9,13),"pop_sti"]
  
  dimnames(sti_cascade_all) = list(seq(first_year, last_year, by=dt),
                                    c("S1", "S2", "HIV_minus", "HIV_plus", "pop_HIV"),
                                    c("prop_S", "prop_E", "prop_Sy", "prop_ASy", "prop_T"))
  
  
  sti_cascade_all = as.data.frame.table(sti_cascade_all)
  colnames(sti_cascade_all) = c("year", "HIV_group", "sti_group", "value")
  
  
  
  
  

  # plot 3
  all_sti_3 =
    ggplot(data = subset(sti_cascade, sti_group=="prop_sti" & (HIV_group=="pop_HIV" | HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value, group=HIV_group, colour=HIV_group)) +
    geom_line() +
    scale_x_discrete(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(labels = scales::percent,
                       limits=c(0,1),
                       expand=c(0,0)) +
    theme_plot_sti +
    labs(y="proportion of MSM who have sti", title="sti as proportion of MSM")
  

  
  # #plot 4
  # all_sti_4 =
  #   sti_p +
  #   geom_line(data = subset(sti_df, N=="model" & sti_group=="incidence_sti" & HIV_status=="total"), aes(y=value/subset(sti_df, sti_group=="incidence_sti" & HIV_status=="total")$value[1], group=HIV_status, colour=HIV_status)) +
  #   geom_point(data = subset(sti_df, N=="data" & sti_group=="diagnoses_sti" & HIV_status=="total"), aes(y=value/subset(sti_df, sti_group=="diagnoses_sti" & HIV_status=="total")$value[1], group=HIV_status, colour=HIV_status), na.rm=TRUE) +
  #   theme_plot_sti +
  #   labs(y="relative incidence compared to 2007", title="relative annual incidence")
  
  
  #plot 5
  all_sti_5 =
    sti_p +
    geom_line(data = subset(sti_df, N=="model" & (sti_group=="incidence_sti" | sti_group=="recovered_sti") & HIV_group=="pop_HIV"), aes(y=value, group=sti_group, colour=sti_group)) +
    geom_point(data = subset(sti_df, N=="data" & sti_group=="diagnoses_sti" & HIV_group=="pop_HIV"), aes(y=value, group=HIV_group, colour=sti_group), na.rm=TRUE) +
    theme_plot_sti +
    labs(y="annual incidence of sti", title="annual incidence") +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(expand=c(0,0),
                       limits = c(0, 1.05*max(sti_df$value)))
  
  
  sti_cascade_melt = melt(subset(sti_cascade, HIV_group=="pop_HIV" & sti_group!="prop_sti"), id.vars = c('year', 'HIV_group', 'sti_group'), value.name = 'value')
  colnames(sti_cascade_melt)[3] = 'prop'
  
  #plot 6
  all_sti_6 =
    ggplot(data=subset(sti_cascade_melt, prop!="#prop_T"), aes(x=year, y=value, group=prop, fill=prop)) +
    geom_area() +
    geom_hline(yintercept=0.25*(1:4), size=0.2, linetype="dashed", colour="grey35") +
    scale_x_discrete(breaks = seq(2000, 2040, by = 5), expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent) +
    theme_plot_sti +
    labs(y="SEILT", title="proportion of sti_plus")
  
  
  
  #plot 7
  all_sti_7 =
    y_p +
    geom_area(data = subset(y_sti_df, N=="model" & HIV_group=="pop_HIV" & (sti_group=="sti_minus" | sti_group=="sti_plus")), aes(y=value, group=sti_group, fill=sti_group, colour=NULL)) +
    geom_hline(yintercept=10000*(1:4), size=0.2, linetype="dashed", colour="grey35") +
    theme_plot_sti +
    labs(y="total sti numbers", title="sti totals") +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5), expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
  
  
  
  sti_cascade_all_melt = melt(subset(sti_cascade_all, HIV_group=="pop_HIV"), id.vars = c('year', 'HIV_group', 'sti_group'), value.name = 'value')
  colnames(sti_cascade_all_melt)[3] = 'prop'
  
  
  #plot 8
  all_sti_8 =
    ggplot(data=sti_cascade_all_melt, aes(x=year, y=value, group=prop, fill=prop)) +
    geom_area() +
    scale_x_discrete(breaks = seq(2000, 2040, by = 5), expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0),
                       labels = scales::percent) +
    theme_plot_sti +
    labs(y="SEILT", title="proportion of population by sti group")
  
  #plot 9
  all_sti_9 =
    ggplot(data=sti_by_testing, aes(x=year, y=prop, colour=testing_status)) +
    geom_line() +
    theme_plot_sti +
    labs(y="prevalence of sti", title="sti proportion by testing status") +
    scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
    scale_y_continuous(expand=c(0,0),
                       # limits = c(0, 1.05*max(sti_by_testing$prop)),
                       limits = c(0, 1),
                       labels = scales::percent)
  
  
  # all_sti_grobs = list(all_sti_1, all_sti_2, all_sti_3, all_sti_5, all_sti_6, all_sti_7)
  # 
  # all_sti = grid.arrange(grobs=all_sti_grobs)
  
  all_sti = ggarrange(all_sti_1, all_sti_2, all_sti_3, all_sti_5, all_sti_6, all_sti_7, all_sti_8, all_sti_9, nrow=3, ncol=3)
  all_sti = annotate_figure(all_sti, top = text_grob("sti plots", face = "bold", size = 24))
  ggsave("all_sti_plots.pdf", plot=all_sti, width = 14, height=8)
  browseURL("all_sti_plots.pdf")
  # 
  # 
  # paper_plot_1 =
  #   ggplot(data = subset(sti_df, sti_group=="diagnoses_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, group=HIV_group, colour=HIV_group)) +
  #   geom_line(data = subset(sti_df, N=="model" & sti_group=="diagnoses_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(y=value, group=HIV_group, colour=HIV_group)) +
  #   geom_point(data = subset(sti_df, N=="data" & sti_group=="diagnoses_sti"), aes(y=value, group=HIV_group, colour=HIV_group, shape=HIV_group), na.rm=TRUE) +
  #   labs(x=NULL, y="Notifications among GBM",
  #        title="sti notifications among Victorian GBM by HIV status", subtitle="Data versus model") +
  #   scale_x_continuous(breaks = seq(2007, 2015, by = 1)) +
  #   coord_cartesian(xlim=c(2007,2015))
  #   # facet_wrap(~HIV_group, nrow=1, scales="free_x")
  # 
  # paper_plot_2 =
  #   ggplot(data = subset(y_sti_df, HIV_group=="HIV_minus" | HIV_group=="HIV_plus"), aes(x=year, y=value, group=HIV_group, colour=HIV_group)) +
  #   geom_line(data = subset(y_sti_df, N=="model" & sti_group=="sti_plus" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value /
  #                                                                                                                                      subset(y_sti_df, N=="model" & sti_group=="pop_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus"))$value,
  #                                                                                                                                    group=HIV_group, colour=HIV_group)) +
  #   geom_point(data = subset(y_sti_df, N=="data" & sti_group=="prev_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value, group=HIV_group, colour=HIV_group, shape=HIV_group), na.rm=TRUE) +
  #   labs(x=NULL, y="Prevalence",
  #        title="sti prevalence among Victorian GBM by HIV status", subtitle="Data versus model") +
  #   scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
  #   scale_y_continuous(labels = scales::percent) +
  #   coord_cartesian(xlim=c(2007,2015))
  #   # facet_wrap(~HIV_group, nrow=1)
  # 
  # 
  # all_paper_grobs = list(paper_plot_1, paper_plot_2)
  # all_paper = grid.arrange(grobs=all_paper_grobs)
  # all_paper = annotate_figure(all_paper, top = text_grob("paper plots", face = "bold", size = 24))
  # ggsave("all_paper_plots.pdf", plot=all_paper, width = 14, height=8)
  # browseURL("all_paper_plots.pdf")
  # 
  # # paper_plot_1 =
  # #   ggplot(data = subset(sti_df, sti_group=="diagnoses_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, group=HIV_group, colour=N)) +
  # #   geom_line(data = subset(sti_df, N=="model" & sti_group=="diagnoses_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(y=value, group=HIV_group, colour=N)) +
  # #   geom_point(data = subset(sti_df, N=="data" & sti_group=="diagnoses_sti"), aes(y=value, group=HIV_group, colour=N), na.rm=TRUE) +
  # #   labs(x=NULL, y="Notifications among GBM",
  # #        title="sti notifications among Victorian GBM by HIV status", subtitle="Data versus model") +
  # #   scale_x_continuous(breaks = seq(2007, 2015, by = 1)) +
  # #   coord_cartesian(xlim=c(2007,2015)) +
  # #   facet_wrap(~HIV_group, nrow=1, scales="free_x")
  # # 
  # # paper_plot_2 =
  # #   ggplot(data = subset(y_sti_df, HIV_group=="HIV_minus" | HIV_group=="HIV_plus"), aes(x=year, y=value, group=HIV_group, colour=N)) +
  # #   geom_line(data = subset(y_sti_df, N=="model" & sti_group=="sti_plus" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value /
  # #                                                                                                                                      subset(y_sti_df, N=="model" & sti_group=="pop_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus"))$value,
  # #                                                                                                                                    group=HIV_group, colour=N)) +
  # #   geom_point(data = subset(y_sti_df, N=="data" & sti_group=="prev_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value, group=HIV_group, colour=N), na.rm=TRUE) +
  # #   labs(x=NULL, y="Prevalence",
  # #        title="sti prevalence among Victorian GBM by HIV status", subtitle="Data versus model") +
  # #   scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
  # #   scale_y_continuous(labels = scales::percent) +
  # #   coord_cartesian(xlim=c(2007,2015)) +
  # #   facet_wrap(~HIV_group, nrow=1)
  
  
    
  
}

plot_all_sti(SID_list)
