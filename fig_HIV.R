## FIGURE S1 ##
# set plot size
indents[3] = 110

# prepare data
y_HIV_df = rbind.fill(y_HIV_df, HIV_main_data)
HIV_df = rbind.fill(HIV_df, HIV_main_data)

# prepare plots
y_p = ggplot(data=y_HIV_df, aes(x=year, y=value, group=HIV_group, colour=HIV_group)) +
  scale_x_continuous(breaks = seq(2000, 2040, by = 5),
                     limits = c(2007, end_year))
HIV_p = ggplot(data=HIV_df, aes(x=year, colour=N, group=N)) +
  scale_x_continuous(breaks = seq(2000, 2040, by = 5),
                     limits = c(2007, end_year))

# create common theme
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


# create HIV cascade
HIV_cascade = array(0, dim=c(dim(y)[1], 5))
HIV_cascade[,1] = seq(first_year, last_year, by=dt)
HIV_cascade[,2] = y[,9,"pop_sti"] / y[,13,"pop_sti"]
HIV_cascade[,3] = y[,4,"pop_sti"] / rowSums(y[,3:4,"pop_sti"])
HIV_cascade[,4] = rowSums(y[,6:7,"pop_sti"]) / y[,4,"pop_sti"]
HIV_cascade[,5] = y[,7,"pop_sti"] / rowSums(y[,6:7,"pop_sti"])

# make into a neat data frame
HIV_cascade = as.data.frame(HIV_cascade)
colnames(HIV_cascade) = c("year",
                          "prop_HIV",
                          "People with HIV who\nare diagnosed",
                          "People diagnosed with HIV\nwho are on treatment",
                          "People with HIV on treatment\nwho are virally suppressed")
# HIV_cascade$N = "model"

# make into a long data frame
HIV_cascade_melt = melt(HIV_cascade, id.vars = 'year', variable.name = 'value')
colnames(HIV_cascade_melt)[2] = 'prop'


# create plots
# top left
fig_HIV_topleft =
  HIV_p +
  geom_line(data = subset(HIV_df, N=="model"), aes(y=diagnoses_HIV, group=1), na.rm=TRUE,
            colour="grey40", lwd=1.6) +
  geom_point(data = subset(HIV_df, N=="data"), aes(y=diagnoses_HIV, group=1), na.rm=TRUE,
             colour="grey20", size=1.3) +
  scale_y_continuous(limits=c(0,1.1*max(HIV_df$diagnoses_HIV, na.rm=T)),
                     expand=c(0,0)) +
  guides(colour=FALSE) +
  theme_fig_HIV +
  labs(x="Year", y="Diagnoses", title="Diagnoses of HIV in year")



# top right
fig_HIV_topright =
  y_p +
  geom_line(data = subset(y_HIV_df, N=="model" & HIV_group=="HIV_plus"), aes(y=value, group=1), na.rm=TRUE,
            colour="grey40", lwd=1.6) +
  geom_point(data = subset(y_HIV_df, N=="data"), aes(y=value, group=1), na.rm=TRUE,
             colour="grey20", size=1.3) +
  scale_y_continuous(limits=c(0,1.1*max(subset(y_HIV_df, N=="model" & HIV_group=="HIV_plus")$value, na.rm=T)),
                     expand=c(0,0)) +
  theme_fig_HIV +
  guides(colour=FALSE) +
  labs(x="Year", y="People living with HIV", title="People living with HIV")



# bottom left
fig_HIV_bottomleft =
  ggplot(data = HIV_cascade, aes(x=year, y=prop_HIV, group=1)) +
  geom_line(lwd=1.6,
            colour="grey40") +
  scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
  scale_y_continuous(labels = scales::percent,
                     expand=c(0,0)) +
  theme_fig_HIV +
  labs(x="Year", y="Proportion of MSM who have HIV", title="Prevalence of HIV") +
  coord_cartesian(ylim=c(0,0.25),
                  xlim=c(start_year, last_year))


# bottom right
fig_HIV_bottomright =
  ggplot(data=subset(HIV_cascade_melt, prop != "prop_HIV"), aes(x=year, y=value, group=prop, colour=prop)) +
  geom_line(size=1.6) +
  scale_x_continuous(breaks = seq(2000, 2040, by = 5)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1),
                     expand = c(0,0)) +
  theme_fig_HIV +
  geom_hline(yintercept=1, colour="grey55", size=0.15) +
  scale_colour_manual(values=c(muted("orange", l=70), muted("gold", l=80, c=90), muted("green", l=70))) +
  labs(x="Year", y="Levels of treatment", title="Care cascade",
       colour = NULL) +
  coord_cartesian(xlim=c(start_year, last_year))

# combine plots and output pdf and png
fig_HIV = ggarrange(fig_HIV_topleft, fig_HIV_topright, fig_HIV_bottomleft, fig_HIV_bottomright, nrow=2, ncol=2, common.legend=FALSE)
fig_HIV = annotate_figure(fig_HIV, top = text_grob("Figure S1", face = "bold", size = 15, vjust=0.8))
ggsave("fig_HIV.pdf", plot=fig_HIV, width = page_width - sum(indents[c(2,4)]), height=page_height - sum(indents[c(1,3)]), units="mm")
ggsave("fig_HIV.png", plot=fig_HIV, dpi = 500, width = page_width - sum(indents[c(2,4)]), height=page_height - sum(indents[c(1,3)]), units="mm")
browseURL("fig_HIV.pdf")
# browseURL("fig_HIV.png")
