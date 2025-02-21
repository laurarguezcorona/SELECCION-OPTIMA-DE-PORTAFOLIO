# SELECCION-OPTIMA-DE-PORTAFOLIO
Este código descarga los datos de las acciones de NAFTRACISHRS, SPY y CETES 28 dias, de los últimos 12meses en periodicidad semanal

Este proyecto consiste en:   

Hacer una página web en donde realiza la selección óptima de un portafolio invertido tanto en CETES de 28 días (activo libre de riesgo), en NAFTRACISHRS: Tracker del S&P/IPC y SPY: Tracker del S&P500

Tomare esos tres instrumentos y voy a descargar de yahoo finance los precios del último año, en periodicidad semanal, este portafolio lo voy a trabajar en moneda nacional, pesos.

En este trabajo explicaré brevemente utilizando notación matricial la matematica de portafolios:

El desempeño o retorno de un portafolio de inversion es una combinacion lineal del desempeño de los activos que lo componen, es la suma ponderada de los activos.

sigmap = w^2a * sigmaa + w^2b * sigmab + 2 wawb * sigmaab
por lo que   mup = w T *mu      y  sigmap =  wT * sigma * w

   - Explicación teórica del proceso de selección de portafolio óptimo para una persona con grado de aversion al riesgo de A=4
Una persona con nivel de aversin al riesgo A=4 tiene una aversiona al riesgo moderadamenta alta, pr lo que penaliza el riesgo, el término 1/2 A* la desviacion estandar de la posición inicial, esto se refiere a que
valor mas alto de A implica que el inversionista esta menos dispuesto a asumir riesgos y prefiere portafolios con menos volatilidad.
  Tendra perferencia por portafolios conservadores y eso genra un impacto en la seleccion del portafolio óptimo, mientras más alta la A, la asignacion de activos mas conservadores, con un mauor peso en activos de bajo riesgo como cetes y menor peros en activos de mayor riesgo como acciones o ETF's.

   - Explicación de las fórmulas y notación matricial utilizada.
Matriz de varianzas y covarianzas: La varianza de los cetes es cero, (activo libre de riesgo), las covarianzas entre los cetes y los otros dos activos NAFTRACISHRS Y SPY) son cero ya que los cetes no tiene riesgo y las covarianzas entre NAFTRACISHRS Y SPY pueden ser positvas, negaticas o cercanas a cero, depenmdiendo de la correlacion entre los mercados mexicano y estadounidnese.

* La frontera eficiente, que representa los portafolios óptimos en termnos de retorno y riesgo, el coeficiente de aversion al riesgo A, determina que punto de la frontera es mas adecuado para el inversionista, en el ejempo de una persona A=4, el portafolio óptimo estará mas cerca del ectremo de menor riesgo de la frontera eficiente.
formula de la funcion de utilidad media varianza    U= E(Rp)-1/2 A* sigma p

   - Explicación del cálculo del VaR y CVaR.
El valor en riesgo es una medida estaditica de riesgo de mercado que estima la pérdida máxima que prodría registrar un portafolio en un intervalo de tiempo con cieto nivel de probabilidad o confianza.
Se puede calcular con *metodos parametricos*, por ejemplo el VaR de un activo individual se estima VaR = F x S x Sigma x raiz del tiempo.
donde F es el factor que determina el nivel de confianza, asi el 95% es F=1.65 y para 98% F=2.23. S el monto a invertir, Sigma es la desviación estandar de los rendiemientos del activo y t es el horizonte de tiempo en que se desea calcular el VaR.
Formula matricial: sigmap = raiz de [w]T * [SIGMA] * [w]
donde SIGMA = [sigma]*[C]*[sigma]

El valor en riesgo (VaR) y el valor en riesgo condicional (CVaR) son medidas de riesgo que estiman las perdidas potenciales del portafolio:
VaR: indica la perdida maxima esperada con un nivel de confianza del 95% o 98%
CVaR: indica la perdida promedio en el peor alfa% de los casos ( donde alfa es el nivel de confianza)
Estas medidas se calculan utilizando distribuciones estadisticas (normal, t-student, GED) y la matriz de varianzas y covarianzas,
Formula de la matriz de varianzas y covarianzas: [SIGMA] = [sigma][C][sigma]
*Metodos no parameticos* Método de simulación histórica, consite en utilizar una serie histórica de precios de la posición de riesgo par construie una serie de tiempo de precios y/o rendimientos simulados o hipotéticos, para utilizar este método se deben identificar los componentes de los activos y reunir los datos hostricos considerando un periodo entre 250 y 500 datos y a aprtir del histograma de frecuencias de los rendimientos simulados se calcula el cuantil correspondiente al primer percentil si buscaramos el nivel de confianza 99%





```{r}

# Cargar librerías necesarias
install.packages(“quantmod”)
library(quantmod)
library(PerformanceAnalytics)
library(quadprog)


# Descargar datos de Yahoo Finance
getSymbols("NAFTRACISHRS.MX", src="yahoo", from="2024-02-20", to="2025-02-20")
getSymbols("SPY", src="yahoo", from="2024-02-20", to="2025-02-20")
getSymbols("CETES28.MX", src="yahoo", from="2024-02-20", to="2025-02-20")


# Calcular retornos semanales
returns_naftrac <- weeklyReturn(NAFTRACISHRS.MX$NAFTRACISHRS.MX.Close)
returns_spy <- weeklyReturn(SPY$SPY.Close)
returns_cetes <- weeklyReturn(CETES28.MX$CETES28.MX.Close)


# Combinar retornos en una matriz
returns <- cbind(returns_naftrac, returns_spy, returns_cetes)
colnames(returns) <- c("NAFTRAC", "SPY", "CETES")


# Calcular matriz de varianzas y covarianzas
cov_matrix <- cov(returns)


# Función para calcular la frontera eficiente
efficient_frontier <- function(returns, cov_matrix, n_portfolios=100) {
  n_assets <- ncol(returns)
  portfolio_returns <- numeric(n_portfolios)
  portfolio_risks <- numeric(n_portfolios)
  weights_list <- list()
  
  for (i in 1:n_portfolios) {
    weights <- runif(n_assets)
    weights <- weights / sum(weights)
    portfolio_returns[i] <- sum(weights * colMeans(returns))
    portfolio_risks[i] <- sqrt(t(weights) %*% cov_matrix %*% weights)
    weights_list[[i]] <- weights
  }
  
  return(list(returns=portfolio_returns, risks=portfolio_risks, weights=weights_list))
}


# Calcular la frontera eficiente
frontier <- efficient_frontier(returns, cov_matrix)


# Selección óptima del portafolio para un inversionista con A=4
A <- 4
optimal_portfolio <- function(returns, cov_matrix, A) {
  n_assets <- ncol(returns)
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, n_assets)
  Amat <- cbind(rep(1, n_assets), colMeans(returns))
  bvec <- c(1, mean(returns))
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq=2)
  weights <- result$solution
  return(weights)
}


weights_optimal <- optimal_portfolio(returns, cov_matrix, A)


# Calcular VaR y CVaR
calculate_var_cvar <- function(returns, weights, alpha=0.05) {
  portfolio_returns <- returns %*% weights
  var <- quantile(portfolio_returns, alpha)
  cvar <- mean(portfolio_returns[portfolio_returns <= var])
  return(list(VaR=var, CVaR=cvar))
}


var_cvar_normal <- calculate_var_cvar(returns, weights_optimal, 0.05)
var_cvar_t <- calculate_var_cvar(returns, weights_optimal, 0.02)


# Resultados
results <- data.frame(
  Method = c("Normal 95%", "Normal 98%", "t-Student 95%", "t-Student 98%"),
  VaR = c(var_cvar_normal$VaR, var_cvar_t$VaR),
  CVaR = c(var_cvar_normal$CVaR, var_cvar_t$CVaR)
)


# Guardar resultados en un archivo
write.csv(results, "results/var_cvar_results.csv", row.names=FALSE)


# Generar gráficos
png("results/efficient_frontier.png")
plot(frontier$risks, frontier$returns, main="Frontera Eficiente", xlab="Riesgo", ylab="Retorno")
dev.off()
```

