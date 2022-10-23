Keaschall <- read.csv(file.choose())
head(Keaschall)

library(ggplot2)

#modifying DF
Keaschall$HardHit <- Keaschall$EV >= 95
Keaschall$HardHit <- replace(Keaschall$HardHit, Keaschall$HardHit == "TRUE", "Hard Hit")
Keaschall$HardHit <- replace(Keaschall$HardHit, Keaschall$HardHit == "FALSE", "Not Hard Hit")

#seeing frequencies
table(Keaschall$HardHit)

#Hard Hit Graphs
ggplot(data = Keaschall, aes(x = HardHit)) + 
  geom_bar(aes(fill = HardHit), color = "black") + 
  ggtitle("Keaschall BIP Splits") + 
  theme_minimal() + 
  labs(x = "Balls in Play", y = "Count") + 
  geom_text(label = "10", y = 5, x = 1, size = 14) + 
  geom_text(label = "52", y = 26, x = 2,  size = 14) + 
  ylim(0, 55) + 
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
KeaschallHH <- subset(Keaschall, EV >= 95)

#HH HeatMap
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = KeaschallHH, 
                        aes(x = PlateLocSide, y = PlateLocHeight, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE, 
                        alpha = 0.85) +
  scale_fill_viridis_d(option = "C") + 
  geom_path(data = sz, aes(x=x, y=z), size = 2) +
  geom_point(data = KeaschallHH, aes(x = PlateLocSide, y = PlateLocHeight, size = 2.5, alpha = .6))+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons, size = 2) + 
  xlim(-2.25, 2.25) + 
  ylim(-0.25, 4.25) +
  theme_minimal() + 
  ggtitle("Keaschall Hard Hit Zone", subtitle = "Pitcher POV") +
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

KeaschallAVG = mean(Keaschall$EV)

KeaschallAVG    
#test distrubution
ggplot(data = Keaschall, aes(x = EV)) +
  geom_histogram(color = "black", fill = "cornflowerblue", bins = 14) +
  theme_minimal() + 
  geom_vline(xintercept = KeaschallAVG, 
             linetype = "dashed", 
             size = 2,
             color = "red") + 
  geom_text(aes(x = KeaschallAVG - 5, label = "Keaschall AVG", y = 8),
            color = "red", 
            angle = 90, 
            size = 6) + 
  ylab("Count") + 
  ggtitle("Keaschall EV Spread") + 
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