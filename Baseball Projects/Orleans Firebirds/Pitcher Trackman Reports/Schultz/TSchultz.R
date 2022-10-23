Schultz <- read.csv(file.choose())
head(Schultz)

colnames(Schultz)
library(ggplot2)

colnames(Schultz)

#Strikezone and Home Plate
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z) 
plate_dimensons <- data.frame(x1 = 0, x2 = -.95, x3 = .95, y1 = 1, y2 = 0.5, y3 = 0)

#Pitch Movement
ggplot(data = Schultz, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.80) + 
  xlim(-22, 22) +
  ylim(-22, 22) + 
  labs(x = "Horizontal Break", y = "Induced Vertical Break") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_minimal() + 
  ggtitle("Schultz Pitch Movement") + 
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
  geom_density2d_filled(data = Schultz, 
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
  ggtitle("Schultz Pitch Location", subtitle = "Pitcher POV") +
  xlab("Feet from Home Plate") +
  ylab("Feet Above Ground") + 
  xlim(-2.5, 2.5) + 
  ylim(-0.25, 5) + 
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
SchultzFB <-subset(Schultz, PitchType == "Four-Seam")
SchultzCH <-subset(Schultz, PitchType == "Changeup")
SchultzSL <-subset(Schultz, PitchType == "Slider")
SchultzCB <-subset(Schultz, PitchType == "Curveball")

#tunneling graph
ggplot(data = Schultz, aes(x = RelSide, y = RelHeight, color = PitchType)) + 
  geom_point(size = 2.5, alpha = 0.60) + 
  xlim(0, 4) + 
  ylim(4,8) + 
  theme_minimal() + 
  labs(x = "Release Side (ft)", y = "Release Height (ft)") + 
  ggtitle("Schultz Pitch Tunneling") + 
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
SchultzAVGfb <- mean(SchultzFB$SpinRate)

ggplot(data = SchultzFB, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 14, fill = "cornflowerblue") + 
  ggtitle("Schultz Fastball Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBfb, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = SchultzAVGfb, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBfb - 25, label = "MLB AVG", y = 3), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = SchultzAVGfb - 25, label = "Schultz AVG", y = 3), 
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
SchultzAVGchange <- mean(SchultzCH$SpinRate)

ggplot(data = SchultzCH, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 8, fill = "cornflowerblue") + 
  ggtitle("Schultz Changeup Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBchange, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = SchultzAVGchange, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBchange - 25, label = "MLB AVG", y = 2), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = SchultzAVGchange - 25, label = "Schultz AVG", y = 2), 
            color = "black", 
            angle = 90, 
            size = 6) + 
  theme_minimal() + 
  #xlim(1800, 2600) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

#Slider Spin Rate
MLBslider <- 2450
SchultzAVGslider <- mean(SchultzSL$SpinRate)

ggplot(data = SchultzSL, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 12, fill = "cornflowerblue") + 
  ggtitle("Schultz Slider Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBslider, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = SchultzAVGslider, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBslider - 25, label = "MLB AVG", y = 4), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = SchultzAVGslider - 25, label = "Schultz AVG", y = 4), 
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

#Slider Spin Rate
MLBcb <- 2550
SchultzAVGcb <- mean(SchultzCB$SpinRate)

ggplot(data = SchultzCB, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 12, fill = "cornflowerblue") + 
  ggtitle("Schultz Curveball Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBcurveball, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = SchultzAVGcb, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBcurveball - 25, label = "MLB AVG", y = 2), 
            color = "red", 
            angle = 90, 
            size = 6) + 
  geom_text(aes(x = SchultzAVGcb - 25, label = "Schultz AVG", y = 2), 
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
