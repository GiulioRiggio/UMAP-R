# Considerato lo script "2024-05-19 creo 5 campioni.R"
# Tenendo costante min_dist = 0.5, 
# vario il numero dei vicini e prendo nota del tempo in secondi 
dataset_test_numero_vicini <- data.frame(
  numero_vicini = c(5, 15, 30, 60, 120, 150, 200, 250, 350, 450, 550, 650, 750, 1000),
  velocita_secondi = c(11, 30, 55, 108, 180, 197, 296, 328, 444, 550, 677, 859, 1307, 2248)
)

# Aggiungere la percentuale
dataset_test_numero_vicini$percentuale <- dataset_test_numero_vicini$numero_vicini * 100 / 5000
dataset_test_numero_vicini$percentuale <- paste0(dataset_test_numero_vicini$percentuale, "%")

#dataset_test_numero_vicini$numero_vicini <- factor(dataset_test_numero_vicini$numero_vicini)

library(ggplot2)

# Creare il grafico a linee
ggplot(dataset_test_numero_vicini, aes(x = numero_vicini, y = velocita_secondi, group = 1)) +
  geom_line(size = 0.6) +
  geom_point(shape = 1, colour = "black", size = 3) +
  geom_hline(yintercept = c(600, 1200, 1800), color = c("red", "blue", "red"), linetype = "dashed", size = 0.1) +
  scale_x_continuous(breaks = seq(0, max(dataset_test_numero_vicini$numero_vicini), by = 100), 
                     sec.axis = sec_axis(~ ., breaks = seq(0, max(dataset_test_numero_vicini$numero_vicini), by = 100), 
                                         labels = c("0%", "2%", "4%", "6%", "8%", "10%", "12%", "14%", "16%", "18%", "20%"))) + 
  scale_y_continuous(breaks = seq(0, max(dataset_test_numero_vicini$velocita_secondi), by = 200)) +
  labs(x = "Numero vicini", y = "t secondi") +
  ggtitle(paste("Tempo in secondi al variare del numero dei vicini")) +
  labs(subtitle='In alto le percentuali del "numero vicini" rispetto a 5000. Crescita lineare fino al 11%') +  
  theme(axis.text.x = element_text(size = 10))

# Tenendo costante il numero vicini = 50, 
# vario la distanza minima e prendo nota del tempo in secondi 
dataset_test_distanza_minima <- data.frame(
  distanza_minima = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1),
  velocita_secondi = c(85, 85, 87, 86, 85, 84, 84, 85, 85)
)


# Creare il grafico a linee
ggplot(dataset_test_distanza_minima, aes(x = distanza_minima, y = velocita_secondi, group = 1)) +
  geom_line(size = 0.6) +
  geom_point(shape = 1, colour = "black", size = 3) +
  #geom_hline(yintercept = c(600, 1200, 1800), color = c("red", "blue", "red"), linetype = "dashed", size = 0.1) +
  scale_x_continuous(trans = 'log10', breaks = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1), 
                    labels = c("0.0001", "0.0005", "0.001", "0.005", "0.01", "0.05", "0.1", "0.5", "1"),
                    sec.axis = sec_axis(~ ., breaks = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1), 
                                         labels = c("0.0001", "0.0005", "0.001", "0.005", "0.01", "0.05", "0.1", "0.5", "1"))) + 
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 100, by = 20)) +
  labs(x = "Distanza minima scala logaritmica", y = "t secondi") +
  ggtitle(paste("Tempo in secondi al variare della distanza minima di proiezione")) +
  labs(subtitle='numero vicini = 50. Tempo costante') +  
  theme(axis.text.x = element_text(size = 10))



