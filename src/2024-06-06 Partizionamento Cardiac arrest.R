# Partizionamento di cardiac arrest
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
#### Scelgo Age_years l'intervallo di valori maggiore [0.01,97]. 

# b- una caratteristica categorica o binaria importante può essere usata per le forme dei punti
#### sesso

# c- una caratteristica categorica, binaria, o numerica importante può essere usata per il colore dei punti
#### Asystole

# d- una caratteristica numerica ordinale o numerica importante può essere usata per le dimensioni dei punti
#### Scelgo `Time_min` [1,50]
# Voglio mappare un numero dall’intervallo [1,50] nell’intervallo [2,5] 
# per ridurre l'intervallo della dimensione dei punti 
input_start <- 1
input_end <- 50
output_start <- 2   # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
output_end <- 5    # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
# Calcola la pendenza (slope)
slope <- (output_end - output_start) / (input_end - input_start)
# Mappa il valore di input specifico
#input_value <- 205
#output_value <- output_start + slope * (input_value - input_start)


# Desisdero creare 3 partizioni, come 3 sono i cluster individuati 
# Utilizzo i valori della tabella massimi  
part_cardiac_arrest <- load_cardiac_arrest

# Determino i clusters, inizializzo a 0
# Time from HF to hospitalization (days) in [11,152]
part_cardiac_arrest$partizioni <- 0

# Time from HF to hospitalization (days) in (342,854]  
part_cardiac_arrest$partizioni[part_cardiac_arrest$"Age_years" > 62 #& 
] <- 1

# Time from HF to hospitalization (days) in (289,342]
part_cardiac_arrest <- part_cardiac_arrest %>%
  mutate(partizioni = ifelse(`Age_years` > 56 & `Age_years` <= 62 #&
                             , 2, partizioni))

# Time from HF to hospitalization (days) in (152,289]
#part_cardiac_arrest <- part_cardiac_arrest %>%
#  mutate(partizioni = ifelse("Time from HF to hospitalization (days)" > 152 & "Time from HF to hospitalization (days)" <= 289 #&
#                             , 3, partizioni))

# Modifico nome colonna
names(part_cardiac_arrest)[names(part_cardiac_arrest) == "sex_woman"] <- "Sex"
#names(part_cardiac_arrest)[names(part_cardiac_arrest) == "Death (1=Yes, 0=No)"] <- "Death"
#names(part_cardiac_arrest)[names(part_cardiac_arrest) == "Sex (1=Yes, 0=No)"] <- "Sex"
#names(part_cardiac_arrest)[names(part_cardiac_arrest) == "Time from HF to Death (days)"] <- "Time_min"

asystole_colors <- c("0" = "green", "1" = "red")

# Iperparametri
num_vicini = 9L
distanza_min = 0.01
r_s = 20L

# P 0
part_cardiac_arrest_0 <- densvis::umap(part_cardiac_arrest[part_cardiac_arrest$partizioni==0,-ncol(part_cardiac_arrest)], 
                                         densmap = FALSE, 
                                         n_neighbors = num_vicini, 
                                         min_dist = distanza_min,
                                         random_state = r_s)  

# Rinomina le colonne
colnames(part_cardiac_arrest_0) <- c("UMAP1", "UMAP2")


# P 1
part_cardiac_arrest_1 <- densvis::umap(part_cardiac_arrest[part_cardiac_arrest$partizioni==1,-ncol(part_cardiac_arrest)], 
                                         densmap = FALSE, 
                                         n_neighbors = num_vicini, 
                                         min_dist = distanza_min,
                                         random_state = r_s)  

# Rinomina le colonne
colnames(part_cardiac_arrest_1) <- c("UMAP1", "UMAP2")

# P 2
part_cardiac_arrest_2 <- densvis::umap(part_cardiac_arrest[part_cardiac_arrest$partizioni==2,-ncol(part_cardiac_arrest)], 
                                         densmap = FALSE, 
                                         n_neighbors = num_vicini, 
                                         min_dist = distanza_min,
                                         random_state = r_s)  

# Rinomina le colonne
colnames(part_cardiac_arrest_2) <- c("UMAP1", "UMAP2")

# P 3
#part_cardiac_arrest_3 <- densvis::umap(part_cardiac_arrest[part_cardiac_arrest$partizioni==3,-ncol(part_cardiac_arrest)], 
#                                    densmap = FALSE, 
#                                    n_neighbors = num_vicini, 
#                                    min_dist = distanza_min,
#                                    random_state = 15L)  

# Rinomina le colonne
#colnames(part_cardiac_arrest_3) <- c("UMAP1", "UMAP2")


# Devo farlo dopo aver proiettato UMAP
part_cardiac_arrest$Sex[part_cardiac_arrest$Sex == 1] <- "female"
part_cardiac_arrest$Sex[part_cardiac_arrest$Sex == 0] <- "male"


# Convertiamo la matrice in un data frame
part_cardiac_arrest_0 <- as.data.frame(cbind(part_cardiac_arrest_0, 
                                               part_cardiac_arrest[part_cardiac_arrest$partizioni==0,-ncol(part_cardiac_arrest)]))
part_cardiac_arrest_1 <- as.data.frame(cbind(part_cardiac_arrest_1, 
                                               part_cardiac_arrest[part_cardiac_arrest$partizioni==1,-ncol(part_cardiac_arrest)]))
part_cardiac_arrest_2 <- as.data.frame(cbind(part_cardiac_arrest_2, 
                                               part_cardiac_arrest[part_cardiac_arrest$partizioni==2,-ncol(part_cardiac_arrest)]))
#part_cardiac_arrest_3 <- as.data.frame(cbind(part_cardiac_arrest_3, 
#                                          part_cardiac_arrest[part_cardiac_arrest$partizioni==3,-ncol(part_cardiac_arrest)]))



# Convertiamo la colonna Asystole in un fattore 
part_cardiac_arrest_0$'Asystole' <- as.factor(part_cardiac_arrest_0$'Asystole')
part_cardiac_arrest_1$'Asystole' <- as.factor(part_cardiac_arrest_1$'Asystole')
part_cardiac_arrest_2$'Asystole' <- as.factor(part_cardiac_arrest_2$'Asystole')
#part_cardiac_arrest_3$'Asystole' <- as.factor(part_cardiac_arrest_3$'Asystole')

# Esempio: Converti la colonna 'Sex' da numerica a carattere
#part_cardiac_arrest_0$Sex <- as.character(part_cardiac_arrest_0$Sex)
#part_cardiac_arrest_1$Sex <- as.character(part_cardiac_arrest_1$Sex)
#part_cardiac_arrest_2$Sex <- as.character(part_cardiac_arrest_2$Sex)

# Creo gli oggetti grafici 
p0_grafico <- ggplot(part_cardiac_arrest_0, 
                     aes(part_cardiac_arrest_0[, 1], 
                         part_cardiac_arrest_0[, 2], 
                         color = Asystole,
                         shape = Sex)) +
  #geom_point(size=part_cardiac_arrest_0$`Time from HF to Asystole (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_cardiac_arrest_0$Time_min - input_start) , alpha=0.5) +
  scale_colour_manual(values = asystole_colors, name = "Asystole", 
                      labels = c("vivo", "morto ")) +
  ggtitle(paste("Age years [0.01,56]")) +
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


p1_grafico <- ggplot(part_cardiac_arrest_1, 
                     aes(part_cardiac_arrest_1[, 1], 
                         part_cardiac_arrest_1[, 2], 
                         color = Asystole,
                         shape = Sex)) +
  #geom_point(size=part_cardiac_arrest_1$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_cardiac_arrest_1$Time_min - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = asystole_colors, name = "Asystole", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("Age years (62,97]")) +
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


p2_grafico <- ggplot(part_cardiac_arrest_2, 
                     aes(part_cardiac_arrest_2[, 1], 
                         part_cardiac_arrest_2[, 2], 
                         color = Asystole,
                         shape = Sex)) +
  #geom_point(size=part_cardiac_arrest_2$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_cardiac_arrest_2$Time_min - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = asystole_colors, name = "Asystole", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("Age years (56,62]")) +
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



#p3_grafico <- ggplot(part_cardiac_arrest_3, 
#                     aes(part_cardiac_arrest_3[, 1], 
#                         part_cardiac_arrest_3[, 2], 
#                         color = Mortality,
#                         shape = Sex)) +
#  #geom_point(size=part_cardiac_arrest_3$`Time from HF to Death (days)`/15 , alpha=0.5) +
#  geom_point(size=output_start + slope * (part_cardiac_arrest_3$`Time from HF to Death (days)` - input_start) , alpha=0.5) +
#  #scale_colour_discrete(name = "Gruppi") +
#  scale_colour_manual(values = mortality_colors, name = "Asystole", 
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

annotate_figure(final_display, top = text_grob("UMAP plot for cardiac arrest dataset", 
                                               color = "blue", face = "bold.italic", size = 15))




