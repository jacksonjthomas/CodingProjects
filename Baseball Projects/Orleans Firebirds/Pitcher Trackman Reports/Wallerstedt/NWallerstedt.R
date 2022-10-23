Wallerstedt <- read.csv(file.choose())

head(Wallerstedt)

colnames(Wallerstedt)
library(ggplot2)

#Strikezone and Home Plate
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z) 
plate_dimensons <- data.frame(x1 = 0, x2 = -.95, x3 = .95, y1 = 1, y2 = 0.5, y3 = 0)

Wallerstedt$PitchType <- replace(Wallerstedt$PitchType, Wallerstedt$PitchType == "Curveball", "Slider")

#Pitch Movement
ggplot(data = Wallerstedt, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType, size = RelSpeed)) + 
  geom_point(size = 2.5, alpha = 0.80) + 
  xlim(-25, 25) +
  ylim(-25, 25) + 
  labs(x = "Horizontal Break", y = "Induced Vertical Break") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_minimal() + 
  ggtitle("Wallerstedt Pitch Movement") + 
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
  geom_density2d_filled(data = Wallerstedt, 
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
  ggtitle("Wallerstedt Pitch Location", subtitle = "Pitcher POV") +
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
WallerstedtFB <-subset(Wallerstedt, PitchType == "Four-Seam")
WallerstedtSL <-subset(Wallerstedt, PitchType == "Slider")
WallerstedtCH <-subset(Wallerstedt, PitchType == "Changeup")


#FB Spin Rate
MLBfb <- 2257
WallerstedtAVGfb <- mean(WallerstedtFB$SpinRate)

ggplot(data = WallerstedtFB, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 16, fill = "cornflowerblue") + 
  ggtitle("Wallerstedt Fastball Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBfb, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = WallerstedtAVGfb, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBfb - 25, label = "MLB AVG", y = 16), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = WallerstedtAVGfb - 25, label = "Wallerstedt AVG", y = 16), 
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
WallerstedtAVGslider <- mean(WallerstedtSL$SpinRate)

ggplot(data = WallerstedtSL, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 14, fill = "cornflowerblue") + 
  ggtitle("Wallerstedt Slider Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBslider, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = WallerstedtAVGslider, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBslider - 30, label = "MLB AVG", y = 8), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = WallerstedtAVGslider - 30, label = "Wallerstedt AVG", y = 8), 
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

#Changeup Spin Rate
MLBchange <- 1750
WallerstedtAVGchange <- mean(WallerstedtCH$SpinRate)

ggplot(data = WallerstedtCH, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 8, fill = "cornflowerblue") + 
  ggtitle("Wallerstedt Changeup Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBchange, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = WallerstedtAVGchange, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBchange - 25, label = "MLB AVG", y = 2), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = WallerstedtAVGchange - 25, label = "Wallerstedt AVG", y = 2), 
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

#tunneling graph
ggplot(data = Wallerstedt, aes(x = RelSide, y = RelHeight, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.60) + 
  theme_minimal() + 
  xlim(0,4) +
  ylim(3,7) + 
  labs(x = "Release Side (ft)", y = "Release Height (ft)") + 
  ggtitle("Wallerstedt Pitch Tunneling") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))