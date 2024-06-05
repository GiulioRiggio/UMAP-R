# Creo tabella dei massimi (DF statistiche) per uno dei datasets

cluster0 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 0,], 2, max)
cluster1 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 1,], 2, max)
cluster2 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 2,], 2, max)
cluster3 <- apply(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 3,], 2, max)

statistiche <- as.data.frame(massimo_totale)
statistiche <- cbind(statistiche, as.data.frame(cluster0))
statistiche <- cbind(statistiche, as.data.frame(cluster1))
statistiche <- cbind(statistiche, as.data.frame(cluster2))
statistiche <- cbind(statistiche, as.data.frame(cluster3))

truncate <- function(x, digits) {
  trunc(x * 10^digits) / 10^digits
}

# Apply truncate to all columns except the indices
statistiche[,] <- lapply(statistiche[,], function(x) if (is.numeric(x)) truncate(x, 2) else x)
