# Parámetros
C0 <- 10000      # Capital inicial
i <- 0.05        # Tasa de interés anual
m <- 12          # Capitalizaciones por año
n <- 3           # Número de años
P <- 10          # Aportación mensual

# Cálculo del capital final usando la fórmula
r <- i / m       # Tasa por período
periods <- m * n # Total de períodos
VF_initial <- C0 * (1 + r)^periods
VF_deposits <- P * ((1 + r)^periods - 1) / r
VF <- VF_initial + VF_deposits

# Cálculo mes a mes para la tabla
balance <- C0
data <- data.frame(
  Mes = integer(),
  Capital_inicial = numeric(),
  Interes = numeric(),
  Aportacion = numeric(),
  Capital_final = numeric()
)
data 
for (t in 0:periods) {
  if (t == 0) {
    data <- rbind(data, data.frame(
      Mes = t,
      Capital_inicial = balance,
      Interes = 0,
      Aportacion = 0,
      Capital_final = balance
    ))
  } else {
    interest <- balance * r
    balance <- balance * (1 + r) + P
    data <- rbind(data, data.frame(
      Mes = t,
      Capital_inicial = round(data$Capital_final[nrow(data)], 2),
      Interes = round(interest, 2),
      Aportacion = P,
      Capital_final = round(balance, 2)
    ))
  }
}

# Imprimir resultados
cat(sprintf("Capital final (fórmula): $%.2f\n", VF))
cat(sprintf("Total aportado: $%.2f\n", C0 + P * periods))
cat(sprintf("Interés generado: $%.2f\n", VF - (C0 + P * periods)))
cat("\nTabla de evolución mes a mes:\n")
print(data, row.names = FALSE)
