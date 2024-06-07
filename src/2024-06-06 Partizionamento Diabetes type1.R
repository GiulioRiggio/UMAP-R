# Partizionamento di diabetes type1
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
#### Scelgo free.testosterone l'intervallo di valori maggiore [0.4,18.1]. 

# b- una caratteristica categorica o binaria importante può essere usata per le forme dei punti
#### sesso

# c- una caratteristica categorica, binaria, o numerica importante può essere usata per il colore dei punti
#### insulin

# d- una caratteristica numerica ordinale o numerica importante può essere usata per le dimensioni dei punti
#### Scelgo `bolus` [7.4,64.09]
# Voglio mappare un numero dall’intervallo [1,50] nell’intervallo [2,5] 
# per ridurre l'intervallo della dimensione dei punti 
input_start <- 7.4
input_end <- 64.09
output_start <- 2   # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
output_end <- 5    # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
# Calcola la pendenza (slope)
slope <- (output_end - output_start) / (input_end - input_start)
# Mappa il valore di input specifico
#input_value <- 205
#output_value <- output_start + slope * (input_value - input_start)


# Desisdero creare 3 partizioni, come 3 sono i cluster individuati 
# Utilizzo i valori della tabella massimi  
part_diabetes_type1 <- load_diabetes_type1

# Determino i clusters, inizializzo a 0
# Time from HF to hospitalization (days) in [11,152]
part_diabetes_type1$partizioni <- 0

# free.testosterone in (342,854]  
part_diabetes_type1$partizioni[part_diabetes_type1$"free.testosterone" > 5.3 #& 
] <- 1

# Time from HF to hospitalization (days) in (289,342]
#part_diabetes_type1 <- part_diabetes_type1 %>%
#  mutate(partizioni = ifelse(`Age_years` > 56 & `Age_years` <= 62 #&
#                             , 2, partizioni))

# Time from HF to hospitalization (days) in (152,289]
#part_diabetes_type1 <- part_diabetes_type1 %>%
#  mutate(partizioni = ifelse("Time from HF to hospitalization (days)" > 152 & "Time from HF to hospitalization (days)" <= 289 #&
#                             , 3, partizioni))

# Modifico nome colonna
names(part_diabetes_type1)[names(part_diabetes_type1) == "sex_0man_1woman"] <- "Sex"
#names(part_diabetes_type1)[names(part_diabetes_type1) == "Death (1=Yes, 0=No)"] <- "Death"
#names(part_diabetes_type1)[names(part_diabetes_type1) == "Sex (1=Yes, 0=No)"] <- "Sex"
#names(part_diabetes_type1)[names(part_diabetes_type1) == "Time from HF to Death (days)"] <- "Time_min"

insulin_colors <- c("0" = "green", "1" = "red")

# Iperparametri
num_vicini = 3L
distanza_min = 0.001
r_s = 17L

# P 0
part_diabetes_type1_0 <- densvis::umap(part_diabetes_type1[part_diabetes_type1$partizioni==0,-ncol(part_diabetes_type1)], 
                                       densmap = FALSE, 
                                       n_neighbors = num_vicini, 
                                       min_dist = distanza_min,
                                       random_state = r_s)  

# Rinomina le colonne
colnames(part_diabetes_type1_0) <- c("UMAP1", "UMAP2")


# P 1
part_diabetes_type1_1 <- densvis::umap(part_diabetes_type1[part_diabetes_type1$partizioni==1,-ncol(part_diabetes_type1)], 
                                       densmap = FALSE, 
                                       n_neighbors = num_vicini, 
                                       min_dist = distanza_min,
                                       random_state = r_s)  

# Rinomina le colonne
colnames(part_diabetes_type1_1) <- c("UMAP1", "UMAP2")

# P 2
#part_diabetes_type1_2 <- densvis::umap(part_diabetes_type1[part_diabetes_type1$partizioni==2,-ncol(part_diabetes_type1)], 
#                                       densmap = FALSE, 
#                                       n_neighbors = num_vicini, 
#                                       min_dist = distanza_min,
#                                       random_state = r_s)  

# Rinomina le colonne
#colnames(part_diabetes_type1_2) <- c("UMAP1", "UMAP2")

# P 3
#part_diabetes_type1_3 <- densvis::umap(part_diabetes_type1[part_diabetes_type1$partizioni==3,-ncol(part_diabetes_type1)], 
#                                    densmap = FALSE, 
#                                    n_neighbors = num_vicini, 
#                                    min_dist = distanza_min,
#                                    random_state = 15L)  

# Rinomina le colonne
#colnames(part_diabetes_type1_3) <- c("UMAP1", "UMAP2")


# Devo farlo dopo aver proiettato UMAP
part_diabetes_type1$Sex[part_diabetes_type1$Sex == 1] <- "female"
part_diabetes_type1$Sex[part_diabetes_type1$Sex == 0] <- "male"


# Convertiamo la matrice in un data frame
part_diabetes_type1_0 <- as.data.frame(cbind(part_diabetes_type1_0, 
                                             part_diabetes_type1[part_diabetes_type1$partizioni==0,-ncol(part_diabetes_type1)]))
part_diabetes_type1_1 <- as.data.frame(cbind(part_diabetes_type1_1, 
                                             part_diabetes_type1[part_diabetes_type1$partizioni==1,-ncol(part_diabetes_type1)]))
#part_diabetes_type1_2 <- as.data.frame(cbind(part_diabetes_type1_2, 
#                                             part_diabetes_type1[part_diabetes_type1$partizioni==2,-ncol(part_diabetes_type1)]))
#part_diabetes_type1_3 <- as.data.frame(cbind(part_diabetes_type1_3, 
#                                          part_diabetes_type1[part_diabetes_type1$partizioni==3,-ncol(part_diabetes_type1)]))



# Convertiamo la colonna Asystole in un fattore 
part_diabetes_type1_0$'insulin_regimen_binary' <- as.factor(part_diabetes_type1_0$'insulin_regimen_binary')
part_diabetes_type1_1$'insulin_regimen_binary' <- as.factor(part_diabetes_type1_1$'insulin_regimen_binary')
#part_diabetes_type1_2$'Asystole' <- as.factor(part_diabetes_type1_2$'Asystole')
#part_diabetes_type1_3$'Asystole' <- as.factor(part_diabetes_type1_3$'Asystole')

# Esempio: Converti la colonna 'Sex' da numerica a carattere
#part_diabetes_type1_0$Sex <- as.character(part_diabetes_type1_0$Sex)
#part_diabetes_type1_1$Sex <- as.character(part_diabetes_type1_1$Sex)
#part_diabetes_type1_2$Sex <- as.character(part_diabetes_type1_2$Sex)

# Creazione del vettore che associa alle shape
genders <- c("female", "male")

# Creo gli oggetti grafici 
p0_grafico <- ggplot(part_diabetes_type1_0, 
                     aes(part_diabetes_type1_0[, 1], 
                         part_diabetes_type1_0[, 2], 
                         color = insulin_regimen_binary,
                         shape = Sex)) +
  #geom_point(size=part_diabetes_type1_0$`Time from HF to Asystole (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_diabetes_type1_0$bolus - input_start) , alpha=0.5) +
  scale_colour_manual(values = insulin_colors, name = "Insulin", 
                      labels = c("no", "si")) +
  scale_shape_manual(values=c(3, 20), breaks = genders) +
  ggtitle(paste("free.testosterone [0.4,5.3]")) +
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


p1_grafico <- ggplot(part_diabetes_type1_1, 
                     aes(part_diabetes_type1_1[, 1], 
                         part_diabetes_type1_1[, 2], 
                         color = insulin_regimen_binary,
                         shape = Sex)) +
  #geom_point(size=part_diabetes_type1_1$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_diabetes_type1_1$bolus - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = insulin_colors, name = "Insulin", 
                      labels = c("no", "si")) +
  scale_shape_manual(values=c(3, 20), breaks = genders) +
  ggtitle(paste("free.testosterone (5.3,18.1]")) +
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


#p2_grafico <- ggplot(part_diabetes_type1_2, 
#                     aes(part_diabetes_type1_2[, 1], 
#                         part_diabetes_type1_2[, 2], 
#                         color = Asystole,
#                         shape = Sex)) +
#  #geom_point(size=part_diabetes_type1_2$`Time from HF to Death (days)`/15 , alpha=0.5) +
#  geom_point(size=output_start + slope * (part_diabetes_type1_2$Time_min - input_start) , alpha=0.5) +
#  #scale_colour_discrete(name = "Gruppi") +
#  scale_colour_manual(values = asystole_colors, name = "Asystole", 
#                      labels = c("vivo", "morto")) +
#  ggtitle(paste("Age years (56,62]")) +
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



#p3_grafico <- ggplot(part_diabetes_type1_3, 
#                     aes(part_diabetes_type1_3[, 1], 
#                         part_diabetes_type1_3[, 2], 
#                         color = Mortality,
#                         shape = Sex)) +
#  #geom_point(size=part_diabetes_type1_3$`Time from HF to Death (days)`/15 , alpha=0.5) +
#  geom_point(size=output_start + slope * (part_diabetes_type1_3$`Time from HF to Death (days)` - input_start) , alpha=0.5) +
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


final_display <- ggarrange(p0_grafico, p1_grafico, 
                           ncol = 2, nrow = 1, 
                           common.legend = TRUE, legend = "bottom")

annotate_figure(final_display, top = text_grob("UMAP plot for diabetes type1 dataset", 
                                               color = "blue", face = "bold.italic", size = 15))




