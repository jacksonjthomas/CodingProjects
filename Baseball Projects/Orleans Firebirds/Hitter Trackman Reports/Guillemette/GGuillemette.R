Guillemette <- read.csv(file.choose())
head(Guillemette)

library(ggplot2)

#modifying DF
Guillemette$HardHit <- Guillemette$EV >= 95
Guillemette$HardHit <- replace(Guillemette$HardHit, Guillemette$HardHit == "TRUE", "Hard Hit")
Guillemette$HardHit <- replace(Guillemette$HardHit, Guillemette$HardHit == "FALSE", "Not Hard Hit")

#seeing frequencies
table(Guillemette$HardHit)

#Hard Hit Graphs
ggplot(data = Guillemette, aes(x = HardHit)) + 
  geom_bar(aes(fill = HardHit), color = "black") + 
  ggtitle("Guillemette BIP Splits") + 
  theme_minimal() + 
  labs(x = "Balls in Play", y = "Count") + 
  geom_text(label = "7", y = 3.5, x = 1, size = 10) + 
  geom_text(label = "27", y = 13.5, x = 2,  size = 10) + 
  ylim(0, 30) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        legend.position = "none")

#Strikezone and Home Plate
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z) 
plate_dimensons <- data.frame(x1 = 0, x2 = -0.95, x3 = 0.95, y1 = 1, y2 = 0.5, y3 = 0)

#dataframe
GuillemetteHH <- subset(Guillemette, EV >= 95)

#HH HeatMap
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = GuillemetteHH, 
                        aes(x = PlateLocSide, y = PlateLocHeight, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE, 
                        alpha = 0.85) +
  scale_fill_viridis_d(option = "C") + 
  geom_path(data = sz, aes(x=x, y=z), size = 2) +
  geom_point(data = GuillemetteHH, aes(x = PlateLocSide, y = PlateLocHeight, size = 2.5, alpha = .6))+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons, size = 2) + 
  xlim(-2.25, 2.25) + 
  ylim(-0.25, 4.25) +
  theme_minimal() + 
  ggtitle("Guillemette Hard Hit Zone", subtitle = "Pitcher POV") +
  xlab("Feet from Home Plate") +
  ylab("Feet Above Ground") + 
  scale_size(range = c(0.01,3)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        plot.subtitle = element_text(hjust = 0.5, size = 16), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), 
        legend.position = "none") 
GuillemetteAVG = mean(Guillemette$EV)

#test distrubution
ggplot(data = Guillemette, aes(x = EV)) +
  geom_histogram(color = "black", fill = "cornflowerblue", bins = 14) +
  theme_minimal() + 
  geom_vline(xintercept = GuillemetteAVG, 
             linetype = "dashed", 
             size = 2,
             color = "red") + 
  geom_text(aes(x = GuillemetteAVG - 5, label = "Guillemette AVG", y = 5),
            color = "red", 
            angle = 90, 
            size = 5) + 
  ggtitle("Guillemette EV Spread") + 
  ylab("Count") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        plot.subtitle = element_text(hjust = 0.5, size = 16), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        strip.text = element_text(size = 16), 
        legend.position = "none") 
