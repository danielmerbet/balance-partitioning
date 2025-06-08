library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)

dir <- "~/Documents/balance-partitioning/"

target_var <- "pr"
nc <- nc_open(file.path(dir, "input/precipitation.nc"))
lat_pr <- ncvar_get(nc, "lat")
lon_pr <- ncvar_get(nc, "lon")
var_data_pr <- ncvar_get(nc, target_var)
nc_close(nc)
range(var_data_pr, na.rm = T)
dim(var_data_pr)

target_var <- "ro"
nc <- nc_open(file.path(dir, "input/runoff.nc"))
lat_ro <- ncvar_get(nc, "lat")
lon_ro <- ncvar_get(nc, "lon")
var_data_ro <- ncvar_get(nc, target_var)
var_data_ro <- (var_data_ro)*365*1000
nc_close(nc)
range(var_data_ro, na.rm = T)
dim(var_data_ro)

bal_par <- var_data_ro/var_data_pr

#balance partitioning
dir_out <- paste0(dir, "/output/")

# Prepare static coordinate grids
lon_grid <- rep(lon_ro, each = length(lat_ro))
lat_grid <- rep(lat_ro, times = length(lon_ro))
for (i in 1:70) {
  bal_decade <- bal_par[,,i]
  # Convertir a dataframe para ggplot
  df <- melt(bal_decade)
  colnames(df) <- c("lon_idx", "lat_idx", "balance")
  df$lon <- lon_pr[df$lon_idx]
  df$lat <- lat_pr[df$lat_idx]
  
  # Filtrar valores entre 0 y 1
  df_filtered <- df[df$balance >= 0 & df$balance <= 1, ]
  
  #p <- ggplot(df_filtered, aes(x = lon, y = lat, fill = balance)) +
  #  geom_raster() +
  #  coord_fixed() +
  #  scale_fill_viridis_c(option = "plasma", na.value = "transparent", limits = c(0,1)) +
  #  labs(title = paste0("Water balance partitioning (R/P): ", i+1949),
  #       x = "Longitude", y = "Latitude", fill = "Balance") +

    
    p <- ggplot(df_filtered, aes(x = lon, y = lat, fill = balance)) +
      geom_raster() +
      coord_fixed() +
      scale_fill_viridis_c(option = "plasma", na.value = "transparent", limits = c(0, 1)) +
      labs(title = paste0("Water balance partitioning (R/P): ", i + 1949),
           x = NULL, y = NULL, fill = "Balance") +  # Remove axis labels if not needed
      theme_minimal(base_size = 10) +
      theme(
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),  # Top, right, bottom, left
        plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = 5)),
        legend.position = "right"
      ) +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank()
)
  
  ggsave(filename = sprintf(paste0(dir, "frames/bal_par_%02d.png"), i), plot = p, 
         width = 8, height = 4, dpi = 300)
}

library(gifski)
png_files <- list.files(paste0(dir, "frames"), pattern = "bal_par_.*\\.png$", full.names = TRUE)
gifski(png_files, gif_file = paste0(dir,"output/bal_par.gif"), width = 800, height = 400, delay = 0.5)

library(av)
av::av_encode_video(
  input = png_files,
  output = paste0(dir,"output/bal_par.mp4"),
  framerate = 5,  # frames per second
  vfilter = "scale=1200:-2"  # resize if needed
)

# Función para crear y guardar plot de un rango de años
plot_decade <- function(start_year, end_year, bal_par, lon, lat, dir_out) {
  years_idx <- start_year:end_year
  # Promedio decenal (mean sobre la 3ra dimensión)
  bal_decade <- apply(bal_par[,,years_idx], c(1,2), mean, na.rm = TRUE)
  
  # Convertir a dataframe para ggplot
  df <- melt(bal_decade)
  colnames(df) <- c("lon_idx", "lat_idx", "balance")
  df$lon <- lon[df$lon_idx]
  df$lat <- lat[df$lat_idx]
  
  # Filtrar valores entre 0 y 1
  df_filtered <- df[df$balance >= 0 & df$balance <= 1, ]
  
  p <- ggplot(df_filtered, aes(x = lon, y = lat, fill = balance)) +
    geom_raster() +
    coord_fixed() +
    scale_fill_viridis_c(option = "plasma", na.value = "transparent", limits = c(0, 1)) +
    labs(title = paste0("Water balance partitioning (R/P): ", start_year+1949, "-", end_year+1949),
         x = "Longitude", y = "Latitude", fill = "Balance") +  # Remove axis labels if not needed
    theme_minimal(base_size = 10) +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),  # Top, right, bottom, left
      plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = 5)),
      legend.position = "right"
    ) +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank()
    )
  
  # Guardar en PDF
  file_out <- file.path(dir_out, paste0("bal_par_decade_", start_year, "_", end_year, ".pdf"))
  ggsave(file_out, p, width = 8, height = 5)
  
  file_out <- file.path(dir_out, paste0("bal_par_decade_", start_year, "_", end_year, ".png"))
  png(file_out, width = 900, height = 600, res = 150)
  print(p)
  dev.off()

}

# Crear plots para cada década
decades <- list(
  c(1,10),
  c(11,20),
  c(21,30),
  c(31,40),
  c(41,50),
  c(51,60),
  c(61,70)
)

walk(decades, ~plot_decade(.x[1], .x[2], bal_par, lon_pr, lat_pr, dir_out))

# Promedio primera década y última década
bal_first_decade <- apply(bal_par[,,1:10], c(1,2), mean, na.rm = TRUE)
bal_last_decade <- apply(bal_par[,,61:70], c(1,2), mean, na.rm = TRUE)

# Anomalía
bal_anomaly <- bal_last_decade - bal_first_decade

# Convertir a dataframe para ggplot
df_anomaly <- melt(bal_anomaly)
colnames(df_anomaly) <- c("lon_idx", "lat_idx", "anomaly")
df_anomaly$lon <- lon_pr[df_anomaly$lon_idx]
df_anomaly$lat <- lat_pr[df_anomaly$lat_idx]

# Calcular percentiles 1 y 99 ignorando NA
quantiles <- quantile(df_anomaly$anomaly, probs = c(0.01, 0.99), na.rm = TRUE)

# Recortar valores fuera de ese rango
df_anomaly <- df_anomaly %>%
  mutate(anomaly_clipped = pmin(pmax(anomaly, quantiles[1]), quantiles[2]))

# Plot con valores recortados
p_anomaly_clipped <- ggplot(df_anomaly, aes(x = lon, y = lat, fill = anomaly_clipped)) +
  geom_raster() +
  coord_fixed() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, na.value = "transparent",
                       limits = c(quantiles[1], quantiles[2])) +
  labs(title = "Anomaly of R/P (1950-1959 decade minus 2010-2019 decade)",
       x = "Longitude", y = "Latitude", fill = "Anomaly") +
  theme_minimal()

print(p_anomaly_clipped)

# Guardar en PDF
ggsave(filename = paste(dir_out, "anomaly.pdf"), 
       plot = p_anomaly_clipped, width = 8, height = 5)

file_out <- file.path(dir_out, paste0("anomaly.png"))
png(file_out, width = 900, height = 600, res = 150)
print(p_anomaly_clipped)
dev.off()

#porcentaje de cambio
# Anomalía
bal_anomaly <- (bal_last_decade - bal_first_decade)/bal_first_decade

# Convertir a dataframe para ggplot
df_anomaly <- melt(bal_anomaly)
colnames(df_anomaly) <- c("lon_idx", "lat_idx", "anomaly")
df_anomaly$lon <- lon_pr[df_anomaly$lon_idx]
df_anomaly$lat <- lat_pr[df_anomaly$lat_idx]

# Calcular percentiles 1 y 99 ignorando NA
quantiles <- quantile(df_anomaly$anomaly, probs = c(0.01, 0.99), na.rm = TRUE)

# Recortar valores fuera de ese rango
df_anomaly <- df_anomaly %>%
  mutate(anomaly_clipped = pmin(pmax(anomaly, quantiles[1]), quantiles[2]))

# Plot con valores recortados
p_anomaly_clipped <- ggplot(df_anomaly, aes(x = lon, y = lat, fill = anomaly_clipped)) +
  geom_raster() +
  coord_fixed() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, na.value = "transparent",
                       limits = c(quantiles[1], quantiles[2])) +
  labs(title = "Anomaly of R/P (1950-1959 decade minus 2010-2019 decade)",
       x = "Longitude", y = "Latitude", fill = "Anomaly") +
  theme_minimal()

print(p_anomaly_clipped)

# Guardar en PDF
ggsave(filename = paste(dir_out, "anomaly.pdf"), 
       plot = p_anomaly_clipped, width = 8, height = 5)

file_out <- file.path(dir_out, paste0("anomaly.png"))
png(file_out, width = 900, height = 600, res = 150)
print(p_anomaly_clipped)
dev.off()
