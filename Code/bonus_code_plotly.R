library(plotly)
midwest

fig <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
fig

#---- 

# 3D plotting


library(plotly)

# Sample galaxy data: RA (deg), DEC (deg), Z (redshift)
galaxies <- data.frame(
  RA = c(230.62, 230.65, 230.70, 230.75, 230.80),
  DEC = c(27.70, 27.72, 27.68, 27.75, 27.73),
  Z = c(0.07, 0.072, 0.068, 0.071, 0.069)
)

# Convert spherical coords (RA, DEC, redshift) to Cartesian (Mpc)
spherical_to_cartesian <- function(ra, dec, z) {
  distance <- z * 3000  # Approximate distance in megaparsecs (Mpc)
  ra_rad <- ra * pi / 180
  dec_rad <- dec * pi / 180
  x <- distance * cos(dec_rad) * cos(ra_rad)
  y <- distance * cos(dec_rad) * sin(ra_rad)
  zc <- distance * sin(dec_rad)
  return(data.frame(x = x, y = y, z = zc))
}

coords <- spherical_to_cartesian(galaxies$RA, galaxies$DEC, galaxies$Z)
galaxies_3d <- cbind(galaxies, coords)

# Create interactive 3D scatter plot
plot_ly(
  data = galaxies_3d,
  x = ~x, y = ~y, z = ~z,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 7, color = ~Z, colorscale = "Viridis", colorbar = list(title = "Redshift Z")),
  text = ~paste("RA:", RA, "<br>DEC:", DEC, "<br>Z:", Z)
) %>%
  layout(
    title = "Interactive 3D Galaxy Map",
    scene = list(
      xaxis = list(title = "X (Mpc)"),
      yaxis = list(title = "Y (Mpc)"),
      zaxis = list(title = "Z (Mpc)")
    )
  )

