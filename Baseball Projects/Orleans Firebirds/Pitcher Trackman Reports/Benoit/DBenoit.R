Benoit <- read.csv(file.choose())
head(Benoit)

colnames(Benoit)
library(ggplot2)

colnames(Benoit)


#Strikezone and Home Plate
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z) 
plate_dimensons <- data.frame(x1 = 0, x2 = -.95, x3 = .95, y1 = 1, y2 = 0.5, y3 = 0)


#Pitch Movement
ggplot(data = Benoit, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.80) + 
  xlim(-25, 25) +
  ylim(-25, 25) + 
  labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_minimal() + 
  ggtitle("Benoit Pitch Movement") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        strip.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))

#KZone Heat Map
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = Benoit, 
                        aes(x = PlateLocSide, y = PlateLocHeight, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE, 
                        alpha = 0.85) +
  scale_fill_viridis_d(option = "C") + 
  geom_path(data = sz, aes(x=x, y=z), size = 2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons, size = 2) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons, size = 2) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons, size = 2) + 
  theme_minimal() +
  ggtitle("Benoit Pitch Location", subtitle = "Pitcher POV") +
  xlab("Feet from Home Plate") +
  ylab("Feet Above Ground") + 
  xlim(-2.5, 2.5) + 
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
        strip.text = element_text(size = 16)) + 
  facet_wrap(~ PitchType) 

#subsetting pitch types
BenoitFB <-subset(Benoit, PitchType == "Four-Seam")
BenoitCH <-subset(Benoit, PitchType == "Changeup")
BenoitSL <-subset(Benoit, PitchType == "Slider")

#tunneling graph
ggplot(data = Benoit, aes(x = RelSide, y = RelHeight, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.60) + 
  xlim(2, 6) + 
  ylim(3,7) + 
  theme_minimal() + 
  labs(x = "Release Side (ft)", y = "Release Height (ft)") + 
  ggtitle("Benoit Pitch Tunneling") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))

#spin rate histograms

#FB Spin Rate
MLBfb <- 2257
BenoitAVGfb <- mean(BenoitFB$SpinRate)

ggplot(data = BenoitFB, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 12, fill = "cornflowerblue") + 
  ggtitle("Benoit Fastball Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBfb, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = BenoitAVGfb, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBfb - 20, label = "MLB AVG", y = 9), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = BenoitAVGfb - 20, label = "Benoit AVG", y = 9), 
            color = "black",
            angle = 90, 
            size = 6) + 
  theme_minimal() + 
  #xlim(1750, 2300) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

#Changeup Spin Rate
MLBchange <- 1750
BenoitAVGchange <- mean(BenoitCH$SpinRate)

ggplot(data = BenoitCH, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 5, fill = "cornflowerblue") + 
  ggtitle("Benoit Changeup Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBchange, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = BenoitAVGchange, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBchange - 20, label = "MLB AVG", y = 1.5), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = BenoitAVGchange - 20, label = "Benoit AVG", y = 1.5), 
            color = "black", 
            angle = 90, 
            size = 6) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

#Slider Spin Rate
MLBslider <- 2450
BenoitAVGslider <- mean(BenoitSL$SpinRate)

ggplot(data = BenoitSL, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 14, fill = "cornflowerblue") + 
  ggtitle("Benoit Slider Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBslider, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = BenoitAVGslider, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBslider - 25, label = "MLB AVG", y = 7), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = BenoitAVGslider - 25, label = "Benoit AVG", y = 7), 
            color = "black", 
            angle = 90, 
            size = 6) + 
  ylim(0,15) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))
