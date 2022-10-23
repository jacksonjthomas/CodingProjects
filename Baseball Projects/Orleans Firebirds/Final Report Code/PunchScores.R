#spider packages
library(fmsb)

#imoprt csv
PUNCHdf <- read.csv(file.choose())

head(PUNCHdf)

#get zscores transformed
PUNCHdf$PUNCHZ <- round(pnorm(PUNCHdf$PUNCHZ)*100,0)
PUNCHdf$OPP.SLG. <- round(pnorm(PUNCHdf$OPP.SLG.)*100,0)
PUNCHdf$BB.9 <- round(pnorm(PUNCHdf$BB.9)*100,0)
PUNCHdf$Whiff. <- round(pnorm(PUNCHdf$Whiff.)*100,0)

#ORLDF Only
ORL <- PUNCHdf[PUNCHdf$TEAM == "ORL", ]
ORL
ORL$TEAM <- NULL
colnames(ORL) <- c("Player", "WhiffAbility", "WalkPreventionAbility", 
                   "DamagePreventionAbility", "PunchScore")

#ranges
max_min <- data.frame(
  WhiffAbility = c(100,0), WalkPreventionAbility = c(100,0), 
  DamagePreventionAbility = c(100,0), PunchScore = c(100,0))

#modifications for spider web
benoitDF <- ORL[ORL$Player == "Benoit", ]
benoitDF$Player <- NULL
benoitDF <- rbind(max_min, benoitDF)
rownames(benoitDF) <- c("Min", "Max", "Benoit")

cannonDF <- ORL[ORL$Player == "Cannon", ]
cannonDF$Player <- NULL
cannonDF <- rbind(max_min, cannonDF)
rownames(cannonDF) <- c("Min", "Max", "Cannon")

filbyDF <- ORL[ORL$Player == "Filby", ]
filbyDF$Player <- NULL
filbyDF <- rbind(max_min, filbyDF)
rownames(filbyDF) <- c("Min", "Max", "Filby")

kerkDF <- ORL[ORL$Player == "Kerkering", ]
kerkDF$Player <- NULL
kerkDF <- rbind(max_min, kerkDF)
rownames(kerkDF) <- c("Min", "Max", "Kerkering")

morrisDF <- ORL[ORL$Player == "Morris", ]
morrisDF$Player <- NULL
morrisDF <- rbind(max_min, morrisDF)
rownames(morrisDF) <- c("Min", "Max", "Morris")

netzDF <- ORL[ORL$Player == "Netz", ]
netzDF$Player <- NULL
netzDF <- rbind(max_min, netzDF)
rownames(netzDF) <- c("Min", "Max", "Netz")

rajcicDF <- ORL[ORL$Player == "Rajcic", ]
rajcicDF$Player <- NULL
rajcicDF <- rbind(max_min, rajcicDF)
rownames(rajcicDF) <- c("Min", "Max", "Rajcic")

saumDF <- ORL[ORL$Player == "Saum", ]
saumDF $Player <- NULL
saumDF <- rbind(max_min, saumDF)
rownames(benoitDF) <- c("Min", "Max", "Saum")

reillyDF <- ORL[ORL$Player == "Reilly", ]
reillyDF $Player <- NULL
reillyDF <- rbind(max_min, reillyDF)
rownames(reillyDF) <- c("Min", "Max", "Reilly")

schultzDF <- ORL[ORL$Player == "Schultz", ]
schultzDF$Player <- NULL
schultzDF <- rbind(max_min, schultzDF)
rownames(schultzDF) <- c("Min", "Max", "Schultz")

thomasDF <- ORL[ORL$Player == "Thomas", ]
thomasDF$Player <- NULL
thomasDF <- rbind(max_min, thomasDF)
rownames(thomasDF) <- c("Min", "Max", "Thomas")

thurmanDF <- ORL[ORL$Player == "Thurman", ]
thurmanDF$Player <- NULL
thurmanDF <- rbind(max_min, thurmanDF)
rownames(thurmanDF) <- c("Min", "Max", "Thurman")

wallyDF <- ORL[ORL$Player == "Wallerstedt", ]
wallyDF$Player <- NULL
wallyDF <- rbind(max_min, wallyDF)
rownames(wallyDF) <- c("Min", "Max", "Wallerstedt")

southardDF <- ORL[ORL$Player == "Southard", ]
southardDF$Player <- NULL
southardDF <- rbind(max_min, southardDF)
rownames(southardDF) <- c("Min", "Max", "Southard")

#radarchartTest
liteBlue <- "#39C3DB"
caxislabs <- c(" ", " ", " ", " ", " ")
vlabs <- c("Whiff Score - 39", "BB Score - 48", "Slugging Score - 22", "Punch - 13")

op <- par(mar = c(1, 2, 2, 1), family = "Helvetica")
radarchart(
  netzDF, axistype = 1, pcol = liteBlue, 
  pfcol = scales::alpha(liteBlue, 0.5), plwd = 3,
  plty = 1, cglcol = "#B3AC96", cglty = 1, cglwd = 2, 
  axislabcol = "#66604B", caxislabels = caxislabs, calcex = 1.3,
  vlabels = vlabs, title = "Punch Score", palcex = 0.3, vlcex = 1.2,
  cex.main = 1.7)
par(op)

