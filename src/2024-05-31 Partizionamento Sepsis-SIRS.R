# Partizionamento di Text_Sepsis_SIRS_EDITED
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
#### Scelgo PLTC l'intervallo di valori maggiore [11,854]. 
#### Con PLTC per le dimensioni ci sarebbe troppa differenza tra il triangolo + piccolo e il + grande 

# b- una caratteristica categorica o binaria importante può essere usata per le forme dei punti
#### sesso

# c- una caratteristica categorica, binaria, o numerica importante può essere usata per il colore dei punti
#### mortalità

# d- una caratteristica numerica ordinale o numerica importante può essere usata per le dimensioni dei punti
#### Scelgo EOC [0,410]
# Voglio mappare un numero dall’intervallo [0,410] nell’intervallo [5,50] 
# per ridurre l'intervallo della dimensione dei punti 
input_start <- 0
input_end <- 410
output_start <- 2   # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
output_end <- 20    # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
# Calcola la pendenza (slope)
slope <- (output_end - output_start) / (input_end - input_start)
# Mappa il valore di input specifico
#input_value <- 205
#output_value <- output_start + slope * (input_value - input_start)



# Desisdero creare 4 partizioni, come 4 sono i cluster individuati 
part_Sepsis_SIRS <- load_Text_Sepsis_SIRS_EDITED

# Determino i clusters, inizializzo a 0
# PLTC in [11,152]
part_Sepsis_SIRS$partizioni <- 0

# PLTC in (342,854]  
part_Sepsis_SIRS$partizioni[part_Sepsis_SIRS$PLTC > 342 #& 
                            ] <- 1

# PLTC in (289,342]
part_Sepsis_SIRS <- part_Sepsis_SIRS %>%
  mutate(partizioni = ifelse(PLTC > 289 & PLTC <= 342 #&
                               , 2, partizioni))

# PLTC in (152,289]
part_Sepsis_SIRS <- part_Sepsis_SIRS %>%
  mutate(partizioni = ifelse(PLTC > 152 & PLTC <= 289 #&
                             , 3, partizioni))

# Modifico nome colonna
names(part_Sepsis_SIRS)[names(part_Sepsis_SIRS) == "sex_woman"] <- "Sex"

mortality_colors <- c("0" = "green", "1" = "red")

# Iperparametri
num_vicini = 5L
distanza_min = 0.001

# P 0
part_Sepsis_SIRS_0 <- densvis::umap(part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==0,-ncol(part_Sepsis_SIRS)], 
                                         densmap = FALSE, 
                                         n_neighbors = num_vicini, 
                                         min_dist = distanza_min,
                                         random_state = 15L)  

# Rinomina le colonne
colnames(part_Sepsis_SIRS_0) <- c("UMAP1", "UMAP2")


# P 1
part_Sepsis_SIRS_1 <- densvis::umap(part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==1,-ncol(part_Sepsis_SIRS)], 
                                    densmap = FALSE, 
                                    n_neighbors = num_vicini, 
                                    min_dist = distanza_min,
                                    random_state = 15L)  

# Rinomina le colonne
colnames(part_Sepsis_SIRS_1) <- c("UMAP1", "UMAP2")

# P 2
part_Sepsis_SIRS_2 <- densvis::umap(part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==2,-ncol(part_Sepsis_SIRS)], 
                                    densmap = FALSE, 
                                    n_neighbors = num_vicini, 
                                    min_dist = distanza_min,
                                    random_state = 15L)  

# Rinomina le colonne
colnames(part_Sepsis_SIRS_2) <- c("UMAP1", "UMAP2")

# P 3
part_Sepsis_SIRS_3 <- densvis::umap(part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==3,-ncol(part_Sepsis_SIRS)], 
                                    densmap = FALSE, 
                                    n_neighbors = num_vicini, 
                                    min_dist = distanza_min,
                                    random_state = 15L)  

# Rinomina le colonne
colnames(part_Sepsis_SIRS_3) <- c("UMAP1", "UMAP2")


# Devo farlo dopo aver proiettato UMAP
part_Sepsis_SIRS$Sex[part_Sepsis_SIRS$Sex == 1] <- "female"
part_Sepsis_SIRS$Sex[part_Sepsis_SIRS$Sex == 0] <- "male"


# Convertiamo la matrice in un data frame
part_Sepsis_SIRS_0 <- as.data.frame(cbind(part_Sepsis_SIRS_0, 
                                          part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==0,-ncol(part_Sepsis_SIRS)]))
part_Sepsis_SIRS_1 <- as.data.frame(cbind(part_Sepsis_SIRS_1, 
                                          part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==1,-ncol(part_Sepsis_SIRS)]))
part_Sepsis_SIRS_2 <- as.data.frame(cbind(part_Sepsis_SIRS_2, 
                                          part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==2,-ncol(part_Sepsis_SIRS)]))
part_Sepsis_SIRS_3 <- as.data.frame(cbind(part_Sepsis_SIRS_3, 
                                          part_Sepsis_SIRS[part_Sepsis_SIRS$partizioni==3,-ncol(part_Sepsis_SIRS)]))



# Convertiamo la colonna mortalità in un fattore 
part_Sepsis_SIRS_0$Mortality <- as.factor(part_Sepsis_SIRS_0$Mortality)
part_Sepsis_SIRS_1$Mortality <- as.factor(part_Sepsis_SIRS_1$Mortality)
part_Sepsis_SIRS_2$Mortality <- as.factor(part_Sepsis_SIRS_2$Mortality)
part_Sepsis_SIRS_3$Mortality <- as.factor(part_Sepsis_SIRS_3$Mortality)


# Creo gli oggetti grafici 
p0_grafico <- ggplot(part_Sepsis_SIRS_0, 
                     aes(part_Sepsis_SIRS_0[, 1], 
                         part_Sepsis_SIRS_0[, 2], 
                         color = Mortality,
                         shape = Sex)) +
  #geom_point(size=part_Sepsis_SIRS_0$EOC/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_Sepsis_SIRS_0$EOC - input_start) , alpha=0.5) +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto ")) +
  ggtitle(paste("PLTC [11,152]")) +
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


p1_grafico <- ggplot(part_Sepsis_SIRS_1, 
                     aes(part_Sepsis_SIRS_1[, 1], 
                         part_Sepsis_SIRS_1[, 2], 
                         color = Mortality,
                         shape = Sex)) +
  #geom_point(size=part_Sepsis_SIRS_1$EOC/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_Sepsis_SIRS_1$EOC - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("PLTC (342,854]")) +
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


p2_grafico <- ggplot(part_Sepsis_SIRS_2, 
                     aes(part_Sepsis_SIRS_2[, 1], 
                         part_Sepsis_SIRS_2[, 2], 
                         color = Mortality,
                         shape = Sex)) +
  #geom_point(size=part_Sepsis_SIRS_2$EOC/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_Sepsis_SIRS_2$EOC - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("PLTC (289,342]")) +
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



p3_grafico <- ggplot(part_Sepsis_SIRS_3, 
                     aes(part_Sepsis_SIRS_3[, 1], 
                         part_Sepsis_SIRS_3[, 2], 
                         color = Mortality,
                         shape = Sex)) +
  #geom_point(size=part_Sepsis_SIRS_3$EOC/15 , alpha=0.5) +
  geom_point(size=output_start + slope * (part_Sepsis_SIRS_3$EOC - input_start) , alpha=0.5) +
  #scale_colour_discrete(name = "Gruppi") +
  scale_colour_manual(values = mortality_colors, name = "Mortalità", 
                      labels = c("vivo", "morto")) +
  ggtitle(paste("PLTC (152,289]")) +
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


final_display <- ggarrange(p0_grafico, p3_grafico, 
          p2_grafico, p1_grafico, 
          ncol = 2, nrow = 2, 
          common.legend = TRUE, legend = "left")

annotate_figure(final_display, top = text_grob("UMAP plot for sepsis dataset", 
                                      color = "blue", face = "bold.italic", size = 15))




