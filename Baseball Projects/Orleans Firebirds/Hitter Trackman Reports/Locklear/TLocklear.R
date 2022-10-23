Locklear <- read.csv(file.choose())
head(Locklear)

library(ggplot2)

#modifying DF
Locklear$HardHit <- Locklear$EV >= 95
Locklear$HardHit <- replace(Locklear$HardHit, Locklear$HardHit == "TRUE", "Hard Hit")
Locklear$HardHit <- replace(Locklear$HardHit, Locklear$HardHit == "FALSE", "Not Hard Hit")

#seeing frequencies
table(Locklear$HardHit)

#Hard Hit Graphs
ggplot(data = Locklear, aes(x = HardHit)) + 
  geom_bar(aes(fill = HardHit), color = "black") + 
  ggtitle("Locklear BIP Splits") + 
  theme_minimal() + 
  labs(x = "Balls in Play", y = "Count") + 
  geom_text(label = "25", y = 12.5, x = 1, size = 10) + 
  geom_text(label = "56", y = 28, x = 2,  size = 10) + 
  ylim(0, 60) + 
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
LocklearHH <- subset(Locklear, EV >= 95)

#HH HeatMap
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = LocklearHH, 
                        aes(x = PlateLocSide, y = PlateLocHeight, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE, 
                        alpha = 0.85) +
  scale_fill_viridis_d(option = "C") + 
  geom_path(data = sz, aes(x=x, y=z), size = 2) +
  geom_point(data = LocklearHH, aes(x = PlateLocSide, y = PlateLocHeight, size = 2.5, alpha = .6))+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons, size = 2) + 
  xlim(-2.25, 2.25) + 
  ylim(-0.25, 4.25) +
  theme_minimal() + 
  ggtitle("Locklear Hard Hit Zone", subtitle = "Pitcher POV") +
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


LocklearAVG = mean(Locklear$EV)

#test distrubution
ggplot(data = Locklear, aes(x = EV)) +
  geom_histogram(color = "black", fill = "cornflowerblue", bins = 14) +
  theme_minimal() + 
  geom_vline(xintercept = LocklearAVG, 
             linetype = "dashed", 
             size = 2,
             color = "red") + 
  geom_text(aes(x = LocklearAVG - 5, label = "Locklear AVG", y = 12),
                color = "red", 
                angle = 90, 
                size = 6) + 
  ylab("Count") + 
  ggtitle("Locklear EV Spread") + 
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