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
#### Scelgo time_months l'intervallo di valori maggiore [1,100]. 

# b- una caratteristica categorica o binaria importante può essere usata per le forme dei punti
#### site

# c- una caratteristica categorica, binaria, o numerica importante può essere usata per il colore dei punti
#### outcome

# d- una caratteristica numerica ordinale o numerica importante può essere usata per le dimensioni dei punti
#### Scelgo `UH_or_FH` {0,1}
# Voglio mappare un numero dall’intervallo [1,50] nell’intervallo [2,5] 
# per ridurre l'intervallo della dimensione dei punti 
input_start <- 0
input_end <- 1
output_start <- 3   # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
output_end <- 6    # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
# Calcola la pendenza (slope)
slope <- (output_end - output_start) / (input_end - input_start)
# Mappa il valore di input specifico
#input_value <- 205
#output_value <- output_start + slope * (input_value - input_start)


# Desisdero creare 3 partizioni, come 3 sono i cluster individuati 
# Utilizzo i valori della tabella massimi  
part_neuroblastoma <- load_neuroblastoma

# Determino i clusters, inizializzo a 0
# Time from HF to hospitalization (days) in [11,152]
part_neuroblastoma$partizioni <- 0

# free.testosterone in (342,854]  
part_neuroblastoma$partizioni[part_neuroblastoma$"time_months" > 38 #& 
] <- 1

# Time from HF to hospitalization (days) in (289,342]
part_neuroblastoma <- part_neuroblastoma %>%
  mutate(partizioni = ifelse(`time_months` > 17 & `time_months` <= 38 #&
                             , 2, partizioni))

# Time from HF to hospitalization (days) in (152,289]
#part_neuroblastoma <- part_neuroblastoma %>%
#  mutate(partizioni = ifelse("Time from HF to hospitalization (days)" > 152 & "Time from HF to hospitalization (days)" <= 289 #&
#                             , 3, partizioni))

# Modifico nome colonna
names(part_neuroblastoma)[names(part_neuroblastoma) == "sex"] <- "Sex"
#names(part_neuroblastoma)[names(part_neuroblastoma) == "Death (1=Yes, 0=No)"] <- "Death"
#names(part_neuroblastoma)[names(part_neuroblastoma) == "Sex (1=Yes, 0=No)"] <- "Sex"
#names(part_neuroblastoma)[names(part_neuroblastoma) == "Time from HF to Death (days)"] <- "Time_min"

outcome_colors <- c("0" = "green", "1" = "red")

# Iperparametri
num_vicini = 15L
distanza_min = 0.1
r_s = 13L

# P 0
part_neuroblastoma_0 <- densvis::umap(part_neuroblastoma[part_neuroblastoma$partizioni==0,-ncol(part_neuroblastoma)], 
                                       densmap = FALSE, 
                                       n_neighbors = num_vicini, 
                                       min_dist = distanza_min,
                                       random_state = r_s)  

# Rinomina le colonne
colnames(part_neuroblastoma_0) <- c("UMAP1", "UMAP2")


# P 1
part_neuroblastoma_1 <- densvis::umap(part_neuroblastoma[part_neuroblastoma$partizioni==1,-ncol(part_neuroblastoma)], 
                                       densmap = FALSE, 
                                       n_neighbors = num_vicini, 
                                       min_dist = distanza_min,
                                       random_state = r_s)  

# Rinomina le colonne
colnames(part_neuroblastoma_1) <- c("UMAP1", "UMAP2")

# P 2
part_neuroblastoma_2 <- densvis::umap(part_neuroblastoma[part_neuroblastoma$partizioni==2,-ncol(part_neuroblastoma)], 
                                       densmap = FALSE, 
                                       n_neighbors = num_vicini, 
                                       min_dist = distanza_min,
                                       random_state = r_s)  

# Rinomina le colonne
#colnames(part_neuroblastoma_2) <- c("UMAP1", "UMAP2")

# P 3
#part_neuroblastoma_3 <- densvis::umap(part_neuroblastoma[part_neuroblastoma$partizioni==3,-ncol(part_neuroblastoma)], 
#                                    densmap = FALSE, 
#                                    n_neighbors = num_vicini, 
#                                    min_dist = distanza_min,
#                                    random_state = 15L)  

# Rinomina le colonne
#colnames(part_neuroblastoma_3) <- c("UMAP1", "UMAP2")


# Devo farlo dopo aver proiettato UMAP
part_neuroblastoma$Sex[part_neuroblastoma$Sex == 1] <- "female"
part_neuroblastoma$Sex[part_neuroblastoma$Sex == 0] <- "male"


# Convertiamo la matrice in un data frame
part_neuroblastoma_0 <- as.data.frame(cbind(part_neuroblastoma_0, 
                                             part_neuroblastoma[part_neuroblastoma$partizioni==0,-ncol(part_neuroblastoma)]))
part_neuroblastoma_1 <- as.data.frame(cbind(part_neuroblastoma_1, 
                                             part_neuroblastoma[part_neuroblastoma$partizioni==1,-ncol(part_neuroblastoma)]))
part_neuroblastoma_2 <- as.data.frame(cbind(part_neuroblastoma_2, 
                                             part_neuroblastoma[part_neuroblastoma$partizioni==2,-ncol(part_neuroblastoma)]))
#part_neuroblastoma_3 <- as.data.frame(cbind(part_neuroblastoma_3, 
#                                          part_neuroblastoma[part_neuroblastoma$partizioni==3,-ncol(part_neuroblastoma)]))



# Convertiamo la colonna outcome in un fattore 
part_neuroblastoma_0$'outcome' <- as.factor(part_neuroblastoma_0$'outcome')
part_neuroblastoma_1$'outcome' <- as.factor(part_neuroblastoma_1$'outcome')
part_neuroblastoma_2$'outcome' <- as.factor(part_neuroblastoma_2$'outcome')
#part_neuroblastoma_3$'Asystole' <- as.factor(part_neuroblastoma_3$'Asystole')

# Esempio: Converti la colonna 'Sex' da numerica a carattere
part_neuroblastoma_0$site <- as.character(part_neuroblastoma_0$site)
part_neuroblastoma_1$site <- as.character(part_neuroblastoma_1$site)
part_neuroblastoma_2$site <- as.character(part_neuroblastoma_2$site)

# Creazione del vettore che associa alle shape
site_vector <- c("0", "1", "2")

# Creo gli oggetti grafici 
p0_grafico <- ggplot(part_neuroblastoma_0, 
                     aes(part_neuroblastoma_0[, 1], 
                         part_neuroblastoma_0[, 2], 
                         color = outcome,
                         shape = site)) +
  #geom_point(size=part_neuroblastoma_0$`Time from HF to Asystole (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_neuroblastoma_0$UH_or_FH - input_start) , alpha=0.5) +
  scale_colour_manual(values = outcome_colors, name = "Outcome", 
                      labels = c("0", "1")) +
  scale_shape_manual(values=c(3, 20, 22), breaks = site_vector, name = "Site") +
  ggtitle(paste("Time months [1,17]")) +
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
        # Aumenta la distanza tra le legende, tesi 
        #legend.spacing.y = unit(2, "cm")
        # Aumenta la distanza tra le legende, slide 
        legend.spacing.y = unit(0.1, "cm")
  ) +
  xlab(NULL) + ylab(NULL)


# Crea una legenda separata per le slide
legend <- get_legend(p0_grafico)


p1_grafico <- ggplot(part_neuroblastoma_1, 
                     aes(part_neuroblastoma_1[, 1], 
                         part_neuroblastoma_1[, 2], 
                         color = outcome,
                         shape = site)) +
  #geom_point(size=part_neuroblastoma_1$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_neuroblastoma_1$UH_or_FH - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = outcome_colors, name = "Outcome", 
                      labels = c("0", "1")) +
  scale_shape_manual(values=c(3, 20, 22), breaks = site_vector, name = "Site") +
  ggtitle(paste("Time months (38,100]")) +
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


p2_grafico <- ggplot(part_neuroblastoma_2, 
                     aes(part_neuroblastoma_2[, 1], 
                         part_neuroblastoma_2[, 2], 
                         color = outcome,
                         shape = site)) +
  #geom_point(size=part_neuroblastoma_2$`Time from HF to Death (days)`/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_neuroblastoma_2$UH_or_FH - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = outcome_colors, name = "Outcome", 
                      labels = c("0", "1")) +
  scale_shape_manual(values=c(3, 20, 22), breaks = site_vector, name = "Site") +
  ggtitle(paste("Time months (17,38]")) +
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



#p3_grafico <- ggplot(part_neuroblastoma_3, 
#                     aes(part_neuroblastoma_3[, 1], 
#                         part_neuroblastoma_3[, 2], 
#                         color = Mortality,
#                         shape = Sex)) +
#  #geom_point(size=part_neuroblastoma_3$`Time from HF to Death (days)`/15 , alpha=0.5) +
#  geom_point(size=output_start + slope * (part_neuroblastoma_3$`Time from HF to Death (days)` - input_start) , alpha=0.5) +
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

# per la tesi 
#final_display <- ggarrange(p0_grafico, p2_grafico, 
#                           p1_grafico, 
#                           ncol = 2, nrow = 2, 
#                           common.legend = TRUE, legend = "bottom")

# per le slide
final_display <- ggarrange(p0_grafico + theme(legend.position = "none"), p2_grafico + theme(legend.position = "none"),  
                           p1_grafico + theme(legend.position = "none"), legend,
                           ncol = 2, nrow = 2)

annotate_figure(final_display, top = text_grob("UMAP plot for neuroblastoma dataset", 
                                               color = "blue", face = "bold.italic", size = 15))




