
######### Genearating figures for publication ##########

library(ggplot2); theme_set(theme_minimal(base_size=8))
library(ggpubr)

##### Figure 2 - possum detections per 100 camera trap nights and per 100 camera trap hours


# per 100 camera trap night

p1 <- ggplot(dailyDetections, aes(x=date, y=detectionRate))+
  geom_line(alpha = 0.3)+
  # geom_smooth(method="lm", aes(ymin=0 ),colour="black", linetype="dashed",show.legend = F)+
  geom_smooth(method="loess", aes(ymin=0),colour="black", span=0.3, se=F)+
  facet_wrap(vars(SiteName), scales="free_y")+
  ylab("Detections / 100 camera trap nights")+
  xlab("Date")+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10))+
  theme_minimal(base_size=12)

p1


# per 100 camera trap hours (night only) to test if higher detection rates in winter are not due to longer nights (longer window of detection)

p2 <- ggplot(dailyDetections, aes(x=date, y=hourlyDetectionRate))+
  geom_line(alpha = 0.3)+
  #geom_smooth(method="lm", aes(ymin=0 ),colour="black", linetype="dashed",show.legend = F)+
  geom_smooth(method="loess", aes(ymin=0),colour="black", span=0.3, se=F)+
  facet_wrap(vars(SiteName), scales="free_y")+
  ylab("Detections / 100 camera trap hours (night only)")+
  xlab("Date")+
  theme_minimal(base_size=12)

p2


# arranging the two plots side by side

ggarrange(p1, p2)

ggsave(
  filename = "figure2.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 174,
  height = 100,
  units = "mm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white"
)





library(effects)


# individual plots

p1 <- plot(effect(mod=ddModelFinal1, xlevels=list(Rainfall=c(0, 20, 40, 60)), term="meanTempAtNight:Rainfall"),
           x.var=1,
           # main="Mean temperature at night * Rainfall",
           main="(a)", 
           xlab="Mean temperature at night (\u00B0C)",
           ylab="Number of daily detections",
           key.args = list(x=0.95, y=0.05, corner=c(1,0), columns=1, border=F, title="Rainfall (mm)",cex.title=1, cex=1),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)



p2 <- plot(effect(mod=ddModelFinal1, xlevels=4, term="meanTempAtNight:nightLength"),
           x.var=1,
           # main="Mean temperature at night * night length",
           main="(b)", 
           xlab="Mean temperature at night (\u00B0C)",
           ylab=NULL,
           key.args = list(x=0.95, y=0.95, corner=c(1,1), columns=1, border=F, title="Night length (h)",cex.title=1, cex=1 ),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)


p3 <- plot(effect(mod=ddModelFinal1, xlevels=4, term="meanTempAtNight:meanCloudCover"),
           x.var=1,
           #   main="Mean temperature at night * mean cloud cover",
           main="(c)", 
           xlab="Mean temperature at night (\u00B0C)",
           ylab=NULL,
           key.args = list(x=0.90, y=0.95, corner=c(1,1), columns=1, border=F, title="Mean cloud cover (%)",cex.title=1 ),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)



ggarrange(p1, p2, p3, nrow=1)

ggsave(
  filename = "figure3.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 174,
  height = 100,
  units = "mm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white"
)




# Figure 4



ggplot(BTP, aes(x=date, y=nightFraction))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.16)+
  scale_fill_distiller(palette="Greys", direction=1) +
  geom_point(alpha=0.2, size = 0.6)+
  geom_smooth(colour="black")+
  geom_line(aes(y=0), colour = "grey")+
  geom_line(aes(y=1), colour = "grey")+
  facet_wrap(vars(SiteName))+
  theme_minimal(base_size=14)+
  ylim(0,1)+
  theme(
    legend.position='none'
  )+
  labs(
    # title = "Temporal distribution of activity",
    x = "Date",
    y = "Standardised time of nocturnal activity"
  )




# Figure 5





p1 <- plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:nightLength"),
           x.var=1,
           #    main="Mean temperature at night * Night length",
           main="(a)",
           xlab="Mean temperature at night (\u00B0C)" ,
           ylab="Mean time of activity",
           key.args = list(x=0.95, y=0.05, corner=c(1,0), columns=1, border=F, title="Night length",cex.title=1, cex=1 ),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)

p2 <- plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:meanCloudCover"),
           x.var=2,
           #main="Mean temperature at night * Mean cloud cover",
           main="(b)",
           xlab="Mean cloud cover (%)",
           ylab=NULL,
           key.args = list(x=0.15, y=0.05, corner=c(0,0), columns=1, border=F, title="Mean temperature at night (\u00B0C)",cex.title=1 ),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)

p3 <- plot(effect(mod=taModel_plot, xlevels=4, term="lunarNoonNightFraction:meanMoonIllumination"),
           x.var=1,
           #main="Time of lunar noon * Mean moonlight intensity",
           main="(c)",
           xlab="Time of lunar noon",
           ylab=NULL,
           key.args = list(x=0.9, y=0.05, corner=c(1,0), columns=1, border=F, title="Mean moonlight intensity",cex.title=1, cex=1),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)

ggarrange(p1, p2, p3, nrow=1)
