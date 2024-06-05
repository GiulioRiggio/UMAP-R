# Creo tabella dei minimi (DF statistiche) per uno dei datasets

cluster0 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 0,], 2, min)
cluster1 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 1,], 2, min)
cluster2 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 2,], 2, min)
cluster3 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 3,], 2, min)

statistiche <- as.data.frame(minimo_totale)
statistiche <- cbind(statistiche, as.data.frame(cluster0))
statistiche <- cbind(statistiche, as.data.frame(cluster1))
statistiche <- cbind(statistiche, as.data.frame(cluster2))
statistiche <- cbind(statistiche, as.data.frame(cluster3))

truncate <- function(x, digits) {
  trunc(x * 10^digits) / 10^digits
}

# Apply truncate to all columns except the indices
statistiche[,] <- lapply(statistiche[,], function(x) if (is.numeric(x)) truncate(x, 2) else x)
