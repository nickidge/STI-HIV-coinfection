source("reset_pars.R", echo = F)
is_gel=T
prev_heat = function(SID_in, prev_year){
  sti_prev_val =
    SID_in[[1]][as.character(prev_year), "pop_HIV", "sti_plus"] /
    SID_in[[1]][as.character(prev_year), "pop_HIV", "pop_sti"] -
    prev_base_heat
  return(sti_prev_val)
}
inc_heat = function(SID_in, inc_year){
  sti_inc_val =
    sum(SID_in[[1]][which(floor(as.numeric(dimnames(SID_in[[1]])[[1]]))<=inc_year & floor(as.numeric(dimnames(SID_in[[1]])[[1]]))>=split_year),"pop_HIV", "incidence_sti"])/inc_base_heat - 1
  return(sti_inc_val)
}

gel_up_res = 5
gel_down_res = gel_up_res
gel_up_res = gel_up_res + 1
gel_down_res = gel_down_res + 1
eff_gel_opts = seq(0.2, 0.5, by=0.1)
# eff_gel_opts = 0.294
eff_gel_l = length(eff_gel_opts)
heat_year = end_year
tvec_heat = seq(split_year, heat_year+1+dt, by=dt)
tvec_heat_de = tvec_heat[which(floor(tvec_heat)<=heat_year)]

gel_up=0
gel_down=0
SID_heat_base = run_model(y0_split, tvec_heat)
prev_base_heat=SID_heat_base[[1]][as.character(heat_year), "pop_HIV", "sti_plus"] / SID_heat_base[[1]][as.character(heat_year), "pop_HIV", "pop_sti"]
inc_base_heat=sum(SID_heat_base[[1]][which(floor(as.numeric(dimnames(SID_heat_base[[1]])[[1]]))<=heat_year & floor(as.numeric(dimnames(SID_heat_base[[1]])[[1]]))>=split_year),"pop_HIV", "incidence_sti"])
py_base_heat=sum(SID_heat_base[[1]][as.character(tvec_heat_de),"pop_HIV", "pop_sti"]*dt)

gel_up_seq = rep(seq(0, 1, length=gel_up_res), each=gel_down_res, times=eff_gel_l)
gel_down_seq = rep(seq(0, 1, length=gel_down_res), times=gel_up_res*eff_gel_l)
eff_gel_seq = rep(eff_gel_opts, each=gel_up_res*gel_down_res)

heat_mat = matrix(0, nrow=gel_up_res*gel_down_res*eff_gel_l, ncol=4, dimnames=list(NULL, c("gel_up", "gel_down", "eff_gel_sti", "value")))
heat_mat[,1] = gel_up_seq
heat_mat[,2] = gel_down_seq
heat_mat[,3] = eff_gel_seq


for(i_heat in 1:(gel_up_res*gel_down_res*eff_gel_l)){
  gel_up = gel_up_seq[i_heat]
  gel_down = gel_down_seq[i_heat]
  gel_mat = rbind(rep(gel_up, 3),
                  rep(gel_down, 3))
  eff_gel[2] = eff_gel_seq[i_heat]
  SID_heat = run_model(y0_split, tvec_heat)
  heat_mat[i_heat,4] = inc_heat(SID_heat, heat_year)
}
gel_up = 0
gel_down = 0
gel_mat = matrix(0, nrow=2, ncol=3)
eff_gel = eff_gel_base

maxval_heat = max(abs(heat_mat[,4]))
heat_df = data.frame(heat_mat)

heat_df$eff_gel_label<-paste("Gel-PSI eff. for NG =", percent(heat_df$eff_gel))


heat_p = ggplot(heat_df, aes(y=gel_up, x=gel_down, fill=value, z=value))
heat_p = heat_p +
  geom_raster(interpolate=T) +
  # geom_raster() +
  # geom_contour(colour="white", bins=8) +
  # geom_contour(colour="grey20", breaks=0, linetype="dotted") +
  stat_contour(colour="grey20", breaks=c(0), linetype="dotted") +
  scale_x_continuous(expand=c(0,0), labels = scales::percent) +
  scale_y_continuous(expand=c(0,0), labels = scales::percent) +
  # coord_cartesian(xlim=c(0,1),
  #                 ylim=c(0,1)) +
  coord_fixed(xlim=c(0,1),
              ylim=c(0,1)) +
  facet_wrap(~eff_gel_label, ncol=2) +
  labs(fill="% change from base\ncumulative incidence",
    # fill=paste0("Incidence compared to\nbase incidence of ~", 100*round(inc_base_heat/100)),
    title=paste0("Change in cumulative gonorrhoea incidence between\n", split_year, " and ", heat_year, " due to introduction of gel-based\npoint-of-sex intervention"),
    # subtitle = paste0(" Compared to base cumulative incidence of ", 1*round(inc_base_heat/1), " cases\n",
    #                   "  (= ", 0.1*round(10*py_base_heat/inc_base_heat), " person-years per notification)"),
    subtitle = paste0(" Compared to base cumulative incidence of ", 1*round(inc_base_heat/1), " cases\n",
                      "  (= ", 0.1*round(1000*inc_base_heat/py_base_heat), " notifications per 100 person-years)"),
    # subtitle=paste0("Gel effectiveness: ", 0.01*round(100*eff_gel)),
    y="Proportion of non users changing to gel condoms",
    x="Proportion of condom users changing to gel condoms") +
  guides(fill = guide_colourbar(ticks=F))


heat_p = heat_p +
  scale_fill_gradient2(low="green", high="red", mid="white",
                       labels = function(x) ifelse(x>0, paste0("+", percent(x)), percent(x)),
                       limits=c(-maxval_heat, maxval_heat)) +
  theme(axis.ticks=element_blank(),
        axis.ticks.length=unit(0.5, "mm"),
        strip.background=element_rect(colour="grey20", fill=NA),
        strip.text = element_blank(),
        panel.spacing = unit(0, "mm"),
        panel.border=element_rect(colour="grey20", fill=NA),
        text=element_text(size=10))

heat_p = heat_p +
  geom_text(x=0.02, y=0.98, aes(label=eff_gel_label),
             check_overlap=T,
             size=2.8,
             fontface="plain",
             colour="grey20",
             hjust=0, vjust=1)

# heat_p = heat_p +
#   geom_label(x=0.00, y=1, aes(label=eff_gel_label),
#              size=2.8,
#              fontface="plain",
#              colour="grey20",
#              fill="grey95",
#              label.r=unit(0, "mm"),
#              label.size=0.1,
#              alpha=0.3,
#              hjust=0, vjust=1)

  

heat_p = heat_p +
  geom_hline(yintercept=c(0.25, 0.50, 0.75), colour="black", linetype="solid", alpha=0.1, size=0.2) +
  geom_vline(xintercept=c(0.25, 0.50, 0.75), colour="black", linetype="solid", alpha=0.1, size=0.2)

# heat_p = direct.label(heat_p)
    
    
ggsave("heat_plot.pdf", heat_p, width=150, height=120, units="mm")
ggsave("heat_plot.png", heat_p, width=150, height=120, units="mm")
browseURL("heat_plot.pdf")

is_gel=F