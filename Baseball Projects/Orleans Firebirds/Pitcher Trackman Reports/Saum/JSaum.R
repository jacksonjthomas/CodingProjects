saum <- read.csv(file.choose())
head(saum)

colnames(saum)
library(ggplot2)

colnames(saum)

#Strikezone and Home Plate
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z) 
plate_dimensons <- data.frame(x1 = 0, x2 = -.95, x3 = .95, y1 = 1, y2 = 0.5, y3 = 0)

#Pitch Movement
ggplot(data = saum, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.80) + 
  xlim(-25, 25) +
  ylim(-25, 25) + 
  labs(x = "Horizontal Break", y = "Induced Vertical Break") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_minimal() + 
  ggtitle("Saum Pitch Movement") + 
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
  geom_density2d_filled(data = saum, 
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
  ggtitle("Saum Pitch Location", subtitle = "Pitcher POV") +
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
saumFB <-subset(saum, PitchType == "Four-Seam")
saumSL <-subset(saum, PitchType == "Slider")
saumCH <-subset(saum, PitchType == "Changeup")

#Sinker Spin Rate
MLBsink <- 2257
saumAVGfb <- mean(saumFB$SpinRate)

ggplot(data = saumFB, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 14, fill = "cornflowerblue") + 
  ggtitle("Saum Fastball Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBsink, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = saumAVGfb, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBsink - 25, label = "MLB AVG", y = 16), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = saumAVGfb - 25, label = "Saum AVG", y = 16), 
            color = "black", 
            angle = 90, 
            size = 6) + 
  theme_minimal() + 
  xlim(1750, 2300) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

#Slider Spin Rate
MLBslider <- 2450
saumAVGslider <- mean(saumSL$SpinRate)

ggplot(data = saumSL, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 14, fill = "cornflowerblue") + 
  ggtitle("Saum Slider Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBslider, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = saumAVGslider, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBslider - 25, label = "MLB AVG", y = 8), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = saumAVGslider - 25, label = "Saum AVG", y = 8), 
            color = "black", 
            angle = 90, 
            size = 6) + 
  theme_minimal() + 
  xlim(1800, 2600) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

#Changeup Spin Rate
MLBchange <- 1750
saumAVGchange <- mean(saumCH$SpinRate)

ggplot(data = saumCH, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 12, fill = "cornflowerblue") + 
  ggtitle("Saum Changeup Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBchange, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = saumAVGchange, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBchange - 25, label = "MLB AVG", y = 10), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = saumAVGchange - 25, label = "Saum AVG", y = 10), 
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

colnames(saum)

#tunneling graph
ggplot(data = saum, aes(x = RelSide, y = RelHeight, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  xlim(0, -4) + 
  ylim(2.5, 6.5) + 
  theme_minimal() + 
  labs(x = "Release Side", y = "Release Height") + 
  ggtitle("Saum Pitch Tunneling") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))
