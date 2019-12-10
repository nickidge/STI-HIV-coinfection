heat_scen_q=1

secant <- function(fun, x0, x1, tol=1e-04, niter=80){
  for ( i in 1:niter ) {
    x2 <- x1-fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
    if (abs(fun(x2)) < tol)
      return(x2)
    x0 <- x1
    x1 <- x2
  }
  return(x2)
}

inc_heat_gel_up = function(gu){
  gel_up_heat <<- gu
  SID_heat = run_model(y0_split, tvec_heat)
  return(inc_heat(SID_heat, heat_year))
}

get_gel_up = function(geleffic){
  eff_gel_heat <<- geleffic
  gel_up_opt = secant(inc_heat_gel_up, 0, 1)
  return(gel_up_opt)
}

f_eff = function(eff){
  gel_down_heat <<- 1
  gel_up_heat <<- 1
  eff_gel_heat <<- eff
  SID_heat = run_model(y0_split, tvec_heat)
  return(inc_heat(SID_heat, heat_year))
}

heat_year = 2030
tvec_heat = seq(split_year, heat_year+1+dt, by=dt)
tvec_heat_de = tvec_heat[which(floor(tvec_heat)<=heat_year)]
gel_up_heat=0
gel_down_heat=0
SID_heat_base = run_model(y0_split, tvec_heat)
prev_base_heat=SID_heat_base[[1]][as.character(heat_year), "pop_HIV", "sti_plus"] / SID_heat_base[[1]][as.character(heat_year), "pop_HIV", "pop_sti"]
inc_base_heat=sum(SID_heat_base[[1]][which(floor(as.numeric(dimnames(SID_heat_base[[1]])[[1]]))<=heat_year & floor(as.numeric(dimnames(SID_heat_base[[1]])[[1]]))>=split_year),"pop_HIV", "incidence_sti"])
py_base_heat=sum(SID_heat_base[[1]][as.character(tvec_heat_de),"pop_HIV", "pop_sti"]*dt)

efficacy_df = data.frame(efficacy=c(seq(0.0001, 0.27, length.out=20),
                                    seq(0.27, eff_condom - 0.0001, length.out=10)), gradient=NA)
for(i in 1:nrow(efficacy_df)){
  efficacy = efficacy_df$efficacy[i]
  gel_down_heat <<- min(0.01/efficacy, 0.5, na.rm=T)
  efficacy_df$gradient[i] = get_gel_up(efficacy)/gel_down_heat
}

perfect_diag = secant(f_eff, 0, 1)
print(paste0('Gradient is exactly 1 when gel efficacy = ', round(100*perfect_diag, 3), '%'))

p = ggplot(data=efficacy_df, aes(x=efficacy, y=gradient)) +
  geom_line() +
  geom_point(x=perfect_diag, y=1) +
  scale_x_continuous(limits = c(0, eff_condom*1.05), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 20), expand=c(0,0)) +
  geom_vline(xintercept=eff_condom, linetype='dashed') +
  geom_segment(x=0, xend=perfect_diag, y=1, yend=1, linetype='dashed') +
  geom_segment(x=perfect_diag, xend=perfect_diag, y=0, yend=1, linetype='dashed') +
  labs(x='Efficacy of gel', y="Gradient of zero-line on heatmap", title="Heatmap equal tradeoff as a function of gel efficacy") +
  theme_fig_HIV

ggsave("heatmap_gradient.pdf", plot=p, width = 7, height=5)
browseURL("heatmap_gradient.pdf")

heat_scen_q=0