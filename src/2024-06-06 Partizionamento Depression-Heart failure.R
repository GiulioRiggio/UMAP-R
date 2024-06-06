# Partizionamento di depression_heart_failure
# Se lascio tutte e 3 le condizioni non seleziono righe.
# Allora seleziono solo con PLTC

library(pacman)
p_load(readr)
library(ggplot2)
p_load(dplyr)
p_load(gridExtra)
p_load(ggpubr)

# NON uso facet_wrap() perchè avrei un unica proiezione che poi suddivido, 
# dividendo i punti in 4 grafici che non hanno molto senso per come funziona UMAP 

# a- una caratteristica numerica importante va usata per dividere in quadranti
#### Scelgo Time from HF to hospitalization (days) l'intervallo di valori maggiore [1,730]. 

# b- una caratteristica categorica o binaria importante può essere usata per le forme dei punti
#### sesso

# c- una caratteristica categorica, binaria, o numerica importante può essere usata per il colore dei punti
#### mortalità

# d- una caratteristica numerica ordinale o numerica importante può essere usata per le dimensioni dei punti
#### Scelgo `Time from HF to Death (days)` [5,730]
# Voglio mappare un numero dall’intervallo [5,730] nell’intervallo [2,5] 
# per ridurre l'intervallo della dimensione dei punti 
input_start <- 5
input_end <- 730
output_start <- 2   # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
output_end <- 5    # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
# Calcola la pendenza (slope)
slope <- (output_end - output_start) / (input_end - input_start)
# Mappa il valore di input specifico
#input_value <- 205
#output_value <- output_start + slope * (input_value - input_start)


# Desisdero creare 3 partizioni, come 3 sono i cluster individuati 
# Utilizzo i valori della tabella massimi  
part_depression_heart <- load_depression_heart_failure

# Determino i clusters, inizializzo a 0
# Time from HF to hospitalization (days) in [11,152]
part_depression_heart$partizioni <- 0

# Time from HF to hospitalization (days) in (342,854]  
part_depression_heart$partizioni[part_depression_heart$"Time from HF to hospitalization (days)" > 571 #& 
] <- 1

# Time from HF to hospitalization (days) in (289,342]
part_depression_heart <- part_depression_heart %>%
  mutate(partizioni = ifelse(`Time from HF to hospitalization (days)` > 467 & `Time from HF to hospitalization (days)` <= 571 #&
                             , 2, partizioni))

# Time from HF to hospitalization (days) in (152,289]
#part_depression_heart <- part_depression_heart %>%
#  mutate(partizioni = ifelse("Time from HF to hospitalization (days)" > 152 & "Time from HF to hospitalization (days)" <= 289 #&
#                             , 3, partizioni))

# Modifico nome colonna
names(part_depression_heart)[names(part_depression_heart) == "Male (1=Yes, 0=No)"] <- "Sex"
names(part_depression_heart)[names(part_depression_heart) == "Death (1=Yes, 0=No)"] <- "Death"
names(part_depression_heart)[names(part_depression_heart) == "Hospitalized (1=Yes, 0=No)"] <- "Hospitalized"
names(part_depression_heart)[names(part_depression_heart) == "Time from HF to Death (days)"] <- "TimeHFtoDeath"

mortality_colors <- c("0" = "green", "1" = "red")

# Iperparametri
num_vicini = 25L
distanza_min = 0.09
r_s = 14L

# P 0
part_depression_heart_0 <- densvis::umap(part_depression_heart[part_depression_heart$partizioni==0,-ncol(part_depression_heart)], 
                                    densmap = FALSE, 
                                    n_neighbors = num_vicini, 
                                    min_dist = distanza_min,
                                    random_state = r_s)  

# Rinomina le colonne
colnames(part_depression_heart_0) <- c("UMAP1", "UMAP2")


# P 1
part_depression_heart_1 <- densvis::umap(part_depression_heart[part_depression_heart$partizioni==1,-ncol(part_depression_heart)], 
                                    densmap = FALSE, 
                                    n_neighbors = num_vicini, 
                                    min_dist = distanza_min,
                                    random_state = r_s)  

# Rinomina le colonne
colnames(part_depression_heart_1) <- c("UMAP1", "UMAP2")

# P 2
part_depression_heart_2 <- densvis::umap(part_depression_heart[part_depression_heart$partizioni==2,-ncol(part_depression_heart)], 
                                    densmap = FALSE, 
                                    n_neighbors = num_vicini, 
                                    min_dist = distanza_min,
                                    random_state = r_s)  

# Rinomina le colonne
colnames(part_depression_heart_2) <- c("UMAP1", "UMAP2")

# P 3
#part_depression_heart_3 <- densvis::umap(part_depression_heart[part_depression_heart$partizioni==3,-ncol(part_depression_heart)], 
#                                    densmap = FALSE, 
#                                    n_neighbors = num_vicini, 
#                                    min_dist = distanza_min,
#                                    random_state = 15L)  

# Rinomina le colonne
#colnames(part_depression_heart_3) <- c("UMAP1", "UMAP2")


# Devo farlo dopo aver proiettato UMAP
part_depression_heart$Sex[part_depression_heart$Sex == 1] <- "male"
part_depression_heart$Sex[part_depression_heart$Sex == 0] <- "female"


# Convertiamo la matrice in un data frame
part_depression_heart_0 <- as.data.frame(cbind(part_depression_heart_0, 
                                          part_depression_heart[part_depression_heart$partizioni==0,-ncol(part_depression_heart)]))
part_depression_heart_1 <- as.data.frame(cbind(part_depression_heart_1, 
                                          part_depression_heart[part_depression_heart$partizioni==1,-ncol(part_depression_heart)]))
part_depression_heart_2 <- as.data.frame(cbind(part_depression_heart_2, 
                                          part_depression_heart[part_depression_heart$partizioni==2,-ncol(part_depression_heart)]))
#part_depression_heart_3 <- as.data.frame(cbind(part_depression_heart_3, 
#                                          part_depression_heart[part_depression_heart$partizioni==3,-ncol(part_depression_heart)]))



# Convertiamo la colonna mortalità in un fattore 
part_depression_heart_0$'Death' <- as.factor(part_depression_heart_0$'Death')
part_depression_heart_1$'Death' <- as.factor(part_depression_heart_1$'Death')
part_depression_heart_2$'Death' <- as.factor(part_depression_heart_2$'Death')
#part_depression_heart_3$'Death' <- as.factor(part_depression_heart_3$'Death')

# Esempio: Converti la colonna 'Hospitalized' da numerica a carattere
part_depression_heart_0$Hospitalized <- as.character(part_depression_heart_0$Hospitalized)
part_depression_heart_1$Hospitalized <- as.character(part_depression_heart_1$Hospitalized)
part_depression_heart_2$Hospitalized <- as.character(part_depression_heart_2$Hospitalized)

# Creo gli oggetti grafici 
p0_grafico <- ggplot(part_depression_heart_0, 
                     aes(part_depression_heart_0[, 1], 
                         part_depression_heart_0[, 2], 
                         color = Death,
                         shape = Hospitalized)) +
  #geom_point(size=part_depression_heart_0$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_depression_heart_0$TimeHFtoDeath - input_start) , alpha=0.5) +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto ")) +
  ggtitle(paste("Time from HF to hospitalization (days) [1,467]")) +
  theme(plot.title = element_text(color="blue", size=10, face="italic", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF"), 
        panel.grid.major = element_line(color = "grey"),
        legend.title=element_text(color="blue", size=10, face="italic"),
        legend.text=element_text(color="lightblue", size=9, face="bold.italic"),
        plot.background = element_rect(colour = "white", linewidth = 1),
        legend.background = element_rect(
          #fill = "lemonchiffon", 
          colour = "grey", 
          linewidth = 0.5
        ),
        # Aumenta la distanza tra legenda e grafico
        legend.box.margin = unit(4, "pt"),   
        # Aumenta la distanza tra le legende
        legend.spacing.y = unit(2, "cm")
  ) +
  xlab(NULL) + ylab(NULL)


p1_grafico <- ggplot(part_depression_heart_1, 
                     aes(part_depression_heart_1[, 1], 
                         part_depression_heart_1[, 2], 
                         color = Death,
                         shape = Hospitalized)) +
  #geom_point(size=part_depression_heart_1$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_depression_heart_1$TimeHFtoDeath - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("Time from HF to hospitalization (days) (571,730]")) +
  theme(plot.title = element_text(color="blue", size=10, face="italic", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF"), 
        panel.grid.major = element_line(color = "grey"),
        legend.title=element_text(color="blue", size=10, face="italic"),
        legend.text=element_text(color="lightblue", size=9, face="bold.italic"),
        plot.background = element_rect(colour = "white", linewidth = 1),
        legend.background = element_rect(
          #fill = "lemonchiffon", 
          colour = "grey", 
          linewidth = 0.5
        ),
        # Aumenta la distanza tra legenda e grafico
        legend.box.margin = unit(4, "pt"),   
        # Aumenta la distanza tra le legende
        legend.spacing.y = unit(2, "cm")
  ) +
  xlab(NULL) + ylab(NULL)


p2_grafico <- ggplot(part_depression_heart_2, 
                     aes(part_depression_heart_2[, 1], 
                         part_depression_heart_2[, 2], 
                         color = Death,
                         shape = Hospitalized)) +
  #geom_point(size=part_depression_heart_2$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_depression_heart_2$TimeHFtoDeath - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("Time from HF to hospitalization (days) (467,571]")) +
  theme(plot.title = element_text(color="blue", size=10, face="italic", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF"), 
        panel.grid.major = element_line(color = "grey"),
        legend.title=element_text(color="blue", size=10, face="italic"),
        legend.text=element_text(color="lightblue", size=9, face="bold.italic"),
        plot.background = element_rect(colour = "white", linewidth = 1),
        legend.background = element_rect(
          #fill = "lemonchiffon", 
          colour = "grey", 
          linewidth = 0.5
        ),
        # Aumenta la distanza tra legenda e grafico
        legend.box.margin = unit(4, "pt"),   
        # Aumenta la distanza tra le legende
        legend.spacing.y = unit(2, "cm")
  ) +
  xlab(NULL) + ylab(NULL)



#p3_grafico <- ggplot(part_depression_heart_3, 
#                     aes(part_depression_heart_3[, 1], 
#                         part_depression_heart_3[, 2], 
#                         color = Mortality,
#                         shape = Sex)) +
#  #geom_point(size=part_depression_heart_3$`Time from HF to Death (days)`/15 , alpha=0.5) +
#  geom_point(size=output_start + slope * (part_depression_heart_3$`Time from HF to Death (days)` - input_start) , alpha=0.5) +
#  #scale_colour_discrete(name = "Gruppi") +
#  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
#                      labels = c("vivo", "morto")) +
#  ggtitle(paste("PLTC (152,289]")) +
#  theme(plot.title = element_text(color="blue", size=10, face="italic", hjust = 0.5),
#        panel.background = element_rect(fill = "#FFFFFF"), 
#        panel.grid.major = element_line(color = "grey"),
#        legend.title=element_text(color="blue", size=10, face="italic"),
#        legend.text=element_text(color="lightblue", size=9, face="bold.italic"),
#        plot.background = element_rect(colour = "white", linewidth = 1),
#        legend.background = element_rect(
#          #fill = "lemonchiffon", 
#          colour = "grey", 
#          linewidth = 0.5
#        ),
#        # Aumenta la distanza tra legenda e grafico
#        legend.box.margin = unit(4, "pt"),   
#        # Aumenta la distanza tra le legende
#        legend.spacing.y = unit(2, "cm")
#  ) +
#  xlab(NULL) + ylab(NULL)


final_display <- ggarrange(p0_grafico, #p3_grafico, 
                           p2_grafico, p1_grafico, 
                           ncol = 2, nrow = 2, 
                           common.legend = TRUE, legend = "left")

annotate_figure(final_display, top = text_grob("UMAP plot for depression heart failure dataset", 
                                               color = "blue", face = "bold.italic", size = 15))




