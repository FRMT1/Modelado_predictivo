df_precio <- data.frame(
  año = c(2024,2024,2024,2024,2024,2024,2024),
  mes = c(1,2,3,4,5,6,7),
  precio = c(1145.06,1230.06,1349.79,1353.37,1362.98,1368.42,1370.27)
)
df_precio
plot(df_precio$precio, type = "l")
