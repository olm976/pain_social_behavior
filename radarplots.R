library(tidyverse)
library(scales)

#obtain straight polygon lines
coord_radar <- function (theta = "x", start = - pi / 2, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto(
    "CordRadar", CoordPolar, theta = theta, r = r, start = start,
    direction = sign(direction),
    is_linear = function(coord) TRUE)
}

#plot the radar FN05
radarFN05 <- ggplot(fn05, aes(x=parameter, y=FN05, group=1))+
  geom_polygon(aes(y=upper), fill="lightblue", alpha=0.5)+
  geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
  geom_polygon(fill=NA,colour="blue", linewidth=1)+
  geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
  theme_light()+
  theme(panel.grid.minor=element_blank())+
  coord_radar()+
  ylim(-0.25,0.20)+
  xlab("")+
  ylab("Proportional change from baseline")+
  theme(axis.title.y = element_text(size = 15))

#plot the radar FN2030
radarFN2030 <- ggplot(fn2030, aes(x=parameter, y=FN2030, group=1))+
  geom_polygon(aes(y=upper), fill="lightblue", alpha=0.5)+
  geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
  geom_polygon(fill=NA,colour="blue", linewidth=1)+
  geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
  theme_light()+
  theme(panel.grid.minor=element_blank())+
  coord_radar()+
  ylim(-0.25,0.20)+
  xlab("")+
  ylab("Proportional change from baseline")+
  theme(axis.title.y = element_text(size = 15))

#plot the radar SN030
radarSN030 <- ggplot(sn030, aes(x=parameter, y=SN030, group=1))+
  geom_polygon(aes(y=upper), fill="lightblue", alpha=0.5)+
  geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
  geom_polygon(fill=NA,colour="blue", linewidth=1)+
  geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
  theme_light()+
  theme(panel.grid.minor=element_blank())+
  coord_radar()+
  ylim(-0.25,0.20)+
  xlab("")+
  ylab("Proportional change from baseline")+
  theme(axis.title.y = element_text(size = 15))

#plot the radar FH05
radarFH05 <- ggplot(fh05, aes(x=parameter, y=FH05, group=1))+
  geom_polygon(aes(y=upper), fill="darkolivegreen1", alpha=0.3)+
  geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
  geom_polygon(fill=NA,colour="darkgreen", linewidth=1)+
  geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
  theme_light()+
  theme(panel.grid.minor=element_blank())+
  coord_radar()+
  ylim(-0.25,0.20)+
  xlab("")+
  ylab("Proportional change from baseline")+
  theme(axis.title.y = element_text(size = 15))

#plot the radar FH2030
radarFH2030 <- ggplot(fh2030, aes(x=parameter, y=FH2030, group=1))+
  geom_polygon(aes(y=upper), fill="darkolivegreen1", alpha=0.3)+
  geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
  geom_polygon(fill=NA,colour="darkgreen", linewidth=1)+
  geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
  theme_light()+
  theme(panel.grid.minor=element_blank())+
  coord_radar()+
  ylim(-0.25,0.20)+
  xlab("")+
  ylab("Proportional change from baseline")+
  theme(axis.title.y = element_text(size = 15))

#plot the radar SH030
radarSH030 <- ggplot(sh030, aes(x=parameter, y=SH030, group=1))+
  geom_polygon(aes(y=upper), fill="darkolivegreen1", alpha=0.3)+
  geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
  geom_polygon(fill=NA,colour="darkgreen", linewidth=1)+
  geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
  theme_light()+
  theme(panel.grid.minor=element_blank())+
  coord_radar()+
  ylim(-0.25,0.20)+
  xlab("")+
  ylab("Proportional change from baseline")+
  theme(axis.title.y = element_text(size = 15))

ggsave(radarFN05, filename=file.path("figures","radarFN05.png"))
ggsave(radarFN2030, filename=file.path("figures","radarFN2030.png"))
ggsave(radarSN030, filename=file.path("figures","radarSN030.png"))
ggsave(radarFH05, filename=file.path("figures","radarFH05.png"))
ggsave(radarFH2030, filename=file.path("figures","radarFH2030.png"))
ggsave(radarSH030, filename=file.path("figures","radarSH030.png"))