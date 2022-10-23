playerP = read.csv(file.choose())

#w - 848 h - 700 (for exporting)

library(ggplot2)
head(playerP)

jaceDF <- playerP[playerP$Player == "J. Jung", ]
jace <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
         geom_hline(yintercept = 0, linetype = "dashed") + 
         geom_vline(xintercept = 0, linetype = "dashed") +
         geom_point(size = 2.5) +
         theme_minimal() + 
         xlim(-1.4, 4.5) + 
         ylim(-2, 2.7) + 
         labs(x = "BB/K", y = "Total Bases/At-Bat") + 
         ggtitle("Plate Appearance Efficiency Profile") + 
         theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18), 
            strip.text = element_text(size = 16), 
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14)))

jordanDF <- playerP[playerP$Player == "J Sprinkle", ]
jordan <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
           geom_hline(yintercept = 0, linetype = "dashed") + 
           geom_vline(xintercept = 0, linetype = "dashed") +
           geom_point(size = 2.5) + 
           geom_point(data = jordanDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) +   
           theme_minimal() + 
           labs(x = "BB/K", y = "Total Bases/At-Bat") + 
           geom_text(aes(label=ifelse(Player== "J Sprinkle",as.character(Player), " ")), 
                     color = "black", size = 5, nudge_x = 0.5, nudge_y = 0.01) +
           ggtitle("Plate Appearance Efficiency Profile") + 
           theme(plot.title = element_text(hjust = 0.5, size = 20),
                 axis.text.x = element_text(size = 16),
                 axis.text.y = element_text(size = 16),
                 axis.title.x = element_text(size = 18),
                 axis.title.y = element_text(size = 18), 
                 strip.text = element_text(size = 16), 
                 legend.title = element_text(size = 16), 
                 legend.text = element_text(size = 14)))

jaredDF <- playerP[playerP$Player == "J. McKenzie", ]
jared <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
             geom_hline(yintercept = 0, linetype = "dashed") + 
             geom_vline(xintercept = 0, linetype = "dashed") +
             geom_point(size = 2.5) + 
             geom_point(data = jaredDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) +   
             theme_minimal() + 
             labs(x = "BB/K", y = "Total Bases/At-Bat") + 
             geom_text(aes(label=ifelse(Player== "J. McKenzie",as.character(Player), " ")), 
                       color = "black", size = 5, nudge_x = 0.6, nudge_y = 0.01) +
             ggtitle("Plate Appearance Efficiency Profile") + 
             theme(plot.title = element_text(hjust = 0.5, size = 20),
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   axis.title.x = element_text(size = 18),
                   axis.title.y = element_text(size = 18), 
                   strip.text = element_text(size = 16), 
                   legend.title = element_text(size = 16), 
                   legend.text = element_text(size = 14)))

rhylanDF <- playerP[playerP$Player == "R. Thomas", ]
rhylan <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
             geom_hline(yintercept = 0, linetype = "dashed") + 
             geom_vline(xintercept = 0, linetype = "dashed") +
             geom_point(size = 2.5) + 
             geom_point(data = rhylanDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) +   
             theme_minimal() + 
             labs(x = "BB/K", y = "Total Bases/At-Bat") + 
             geom_text(aes(label=ifelse(Player== "R. Thomas",as.character(Player), " ")), 
                       color = "black", size = 5, nudge_x = 0, nudge_y = -0.1) +
             ggtitle("Plate Appearance Efficiency Profile") + 
             theme(plot.title = element_text(hjust = 0.5, size = 20),
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   axis.title.x = element_text(size = 18),
                   axis.title.y = element_text(size = 18), 
                   strip.text = element_text(size = 16), 
                   legend.title = element_text(size = 16), 
                   legend.text = element_text(size = 14)))

chaseDF <- playerP[playerP$Player == "C. DeLauter", ]
chase <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
             geom_hline(yintercept = 0, linetype = "dashed") + 
             geom_vline(xintercept = 0, linetype = "dashed") +
            geom_point(size = 2.5) + 
            geom_point(data = chaseDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) +  
             theme_minimal() + 
             labs(x = "BB/K", y = "Total Bases/At-Bat") + 
             geom_text(aes(label=ifelse(Player== "C. DeLauter",as.character(Player), " ")), 
                       color = "black", size = 5, nudge_x = 0.6, nudge_y = 0.01) +
             ggtitle("Plate Appearance Efficiency Profile") + 
             theme(plot.title = element_text(hjust = 0.5, size = 20),
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   axis.title.x = element_text(size = 18),
                   axis.title.y = element_text(size = 18), 
                   strip.text = element_text(size = 16), 
                   legend.title = element_text(size = 16), 
                   legend.text = element_text(size = 14)))

tylerDF <- playerP[playerP$Player == "T. Locklear", ]
tyler <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
             geom_hline(yintercept = 0, linetype = "dashed") + 
             geom_vline(xintercept = 0, linetype = "dashed") +
            geom_point(size = 2.5) + 
            geom_point(data = tylerDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) +  
             theme_minimal() + 
             labs(x = "BB/K", y = "Total Bases/At-Bat") + 
             geom_text(aes(label=ifelse(Player== "T. Locklear",as.character(Player), " ")), 
                       color = "black", size = 5, nudge_x = 0.55, nudge_y = 0.01) +
             ggtitle("Plate Appearance Efficiency Profile") + 
             theme(plot.title = element_text(hjust = 0.5, size = 20),
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   axis.title.x = element_text(size = 18),
                   axis.title.y = element_text(size = 18), 
                   strip.text = element_text(size = 16), 
                   legend.title = element_text(size = 16), 
                   legend.text = element_text(size = 14)))

chatDF <- playerP[playerP$Player == "P. Chatagnier", ]
chat <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
           geom_hline(yintercept = 0, linetype = "dashed") + 
           geom_vline(xintercept = 0, linetype = "dashed") +
           geom_point(size = 2.5) + 
           geom_point(data = chatDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) + 
           theme_minimal() + 
           labs(x = "BB/K", y = "Total Bases/At-Bat") + 
           geom_text(aes(label=ifelse(Player== "P. Chatagnier",as.character(Player), " ")), 
                     color = "black", size = 5, nudge_x = -0.75, nudge_y =0) +
           ggtitle("Plate Appearance Efficiency Profile") + 
           theme(plot.title = element_text(hjust = 0.5, size = 20),                   
                 axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   axis.title.x = element_text(size = 18),
                   axis.title.y = element_text(size = 18), 
                   strip.text = element_text(size = 16), 
                   legend.title = element_text(size = 16), 
                   legend.text = element_text(size = 14)))

guillyDF <- playerP[playerP$Player == "G. Guillemette", ]
guilly <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
             geom_hline(yintercept = 0, linetype = "dashed") + 
             geom_vline(xintercept = 0, linetype = "dashed") +
             geom_point(size = 2.5) + 
             geom_point(data = guillyDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) + 
             theme_minimal() + 
             labs(x = "BB/K", y = "Total Bases/At-Bat") + 
             geom_text(aes(label=ifelse(Player== "G. Guillemette",as.character(Player), " ")), 
                       color = "black", size = 5, nudge_x = 0.5, nudge_y = -0.1) +
             ggtitle("Plate Appearance Efficiency Profile") + 
             theme(plot.title = element_text(hjust = 0.5, size = 20),
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16),
                   axis.title.x = element_text(size = 18),
                   axis.title.y = element_text(size = 18), 
                   strip.text = element_text(size = 16), 
                   legend.title = element_text(size = 16), 
                   legend.text = element_text(size = 14)))

lukeDF <- playerP[playerP$Player == "L. Keaschall", ]
luke <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
           geom_hline(yintercept = 0, linetype = "dashed") + 
           geom_vline(xintercept = 0, linetype = "dashed") +
           geom_point(size = 2.5) + 
           geom_point(data = lukeDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) + 
           theme_minimal() + 
           labs(x = "BB/K", y = "Total Bases/At-Bat") + 
           geom_text(aes(label=ifelse(Player== "L. Keaschall",as.character(Player), " ")), 
                     color = "black", size = 5, nudge_x = -0.7, nudge_y = 0.01) +
           ggtitle("Plate Appearance Efficiency Profile") + 
           theme(plot.title = element_text(hjust = 0.5, size = 20),
                 axis.text.x = element_text(size = 16),
                 axis.text.y = element_text(size = 16),
                 axis.title.x = element_text(size = 18),
                 axis.title.y = element_text(size = 18), 
                 strip.text = element_text(size = 16), 
                 legend.title = element_text(size = 16), 
                 legend.text = element_text(size = 14)))

kokxDF <- playerP[playerP$Player == "C. Kokx", ]
kokx <- (ggplot(data = playerP, aes(x = BB.K, y = TB.AB, color = Profile)) + 
           geom_hline(yintercept = 0, linetype = "dashed") + 
           geom_vline(xintercept = 0, linetype = "dashed") +
           geom_point(size = 2.5) + 
           geom_point(data = kokxDF, aes(x = BB.K, y = TB.AB), color = "black", size = 1.5) + 
           theme_minimal() + 
           labs(x = "BB/K", y = "Total Bases/At-Bat") + 
           geom_text(aes(label=ifelse(Player== "C. Kokx",as.character(Player), " ")), 
                     color = "black", size = 5, nudge_x = 0.4, nudge_y = -0.07) +
           ggtitle("Plate Appearance Efficiency Profile") + 
           theme(plot.title = element_text(hjust = 0.5, size = 20),
                  axis.text.x = element_text(size = 16),
                  axis.text.y = element_text(size = 16),
                  axis.title.x = element_text(size = 18),
                  axis.title.y = element_text(size = 18), 
                  strip.text = element_text(size = 16), 
                  legend.title = element_text(size = 16), 
                  legend.text = element_text(size = 14)))


kokx
jordan
jace
rhylan
chat 
chase
tyler 
guilly 
luke
jared #redo adjust


round(pnorm(1.96)*100,0)




