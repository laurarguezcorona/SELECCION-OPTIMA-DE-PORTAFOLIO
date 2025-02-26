# SELECCION-OPTIMA-DE-PORTAFOLIO

Para tener exito en la elección de un portafolio de inversion es necesario contar con una estrategia de inversion y el conocimiento del mercado así como el niverl de aversión al riesgo que tiene nuestro cliente o socio comercial. Datos muy valiosos como el horizonte de inversión, los montos disponibles y el análisi minucioso del prospecto de perdidas potenciales así como la volatilidad de los instrumentos a elegir, serán determinantes para lograr un portafolio exitoso.

En este ejemplo vamos a elaborar un proyecto de inversion de dos acciones y cetes:

   Selección óptima de un portafolio invertido tanto en CETES de 28 días (activo libre de riesgo), en NAFTRACISHRS: Tracker del S&P/IPC y SPY: Tracker del S&P500

Tomare esos tres instrumentos y voy a descargar de yahoo finance los precios del último año, en periodicidad semanal, este portafolio lo voy a trabajar en moneda nacional, pesos.

En este trabajo explicaré brevemente utilizando notación matricial la matematica de portafolios:

El desempeño o retorno de un portafolio de inversion es una combinacion lineal del desempeño de los activos que lo componen, es la suma ponderada de los activos.

                                               σp = w^2a * σa + w^2b * σb + 2 wawb * σab
                       por lo que              μp = w T *μu      y  σp =  wT * σ * w

   - Explicación teórica del proceso de selección de portafolio óptimo para una persona con grado de aversion al riesgo de A=4
Una persona con nivel de aversin al riesgo A=4 tiene una aversiona al riesgo moderadamenta alta, pr lo que penaliza el riesgo, el término 1/2 A* la desviacion estandar de la posición inicial, esto se refiere a que
valor mas alto de A implica que el inversionista esta menos dispuesto a asumir riesgos y prefiere portafolios con menos volatilidad.
  Tendra perferencia por portafolios conservadores y eso genra un impacto en la seleccion del portafolio óptimo, mientras más alta la A, la asignacion de activos mas conservadores, con un mauor peso en activos de bajo riesgo como cetes y menor peros en activos de mayor riesgo como acciones o ETF's.

   - Explicación de las fórmulas y notación matricial utilizada.
Matriz de varianzas y covarianzas: La varianza de los cetes es cero, (activo libre de riesgo), las covarianzas entre los cetes y los otros dos activos NAFTRACISHRS Y SPY) son cero ya que los cetes no tiene riesgo y las covarianzas entre NAFTRACISHRS Y SPY pueden ser positvas, negaticas o cercanas a cero, dependiendo de la correlacion entre los mercados.

* La frontera eficiente, que representa los portafolios óptimos en termnos de retorno y riesgo, el coeficiente de aversion al riesgo A, determina que punto de la frontera es mas adecuado para el inversionista, en el ejempo de una persona A=4, el portafolio óptimo estará mas cerca del ectremo de menor riesgo de la frontera eficiente.
formula de la funcion de utilidad media varianza    U= E(Rp)-1/2 A* σ p
                                       Indice de Sharpe = Ei -rF / σ
que nos indica que la recta tangencial en la que su derivada sea menor, otorga el punto optimo del portafolio = Ep -rF / σp               

   - Explicación del cálculo del VaR y CVaR.
El valor en riesgo es una medida estaditica de riesgo de mercado que estima la pérdida máxima que prodría registrar un portafolio en un intervalo de tiempo con cieto nivel de probabilidad o confianza.
Se puede calcular con *metodos parametricos*, por ejemplo el VaR de un activo individual se estima VaR = F x S x  σ  x raiz cuadrada del tiempo.
donde F es el factor que determina el nivel de confianza, asi el 95% es F=1.65 y para 98% F=2.23. S el monto a invertir,  σ  es la desviación estandar de los rendiemientos del activo y t es el horizonte de tiempo en que se desea calcular el VaR.

Formula matricial:                 σp = raiz de [w]T * [Σ] * [w]
                             donde Σ = [σ]*[C]*[σ]

El valor en riesgo (VaR) y el valor en riesgo condicional (CVaR) son medidas de riesgo que estiman las perdidas potenciales del portafolio:
VaR: indica la perdida maxima esperada con un nivel de confianza del 95% o 98%
CVaR: indica la perdida promedio en el peor alfa% de los casos ( donde alfa es el nivel de confianza)
Estas medidas se calculan utilizando distribuciones estadisticas (normal, t-student, GED) y la matriz de varianzas y covarianzas.

Formula de la matriz de varianzas y covarianzas: [Σ] = [σ][C][σ]

*Metodos no parameticos* Método de simulación histórica, consite en utilizar una serie histórica de precios de la posición de riesgo par construie una serie de tiempo de precios y/o rendimientos simulados o hipotéticos, para utilizar este método se deben identificar los componentes de los activos y reunir los datos histricos considerando un periodo entre 250 y 500 datos y a aprtir del histograma de frecuencias de los rendimientos simulados se calcula el cuantil correspondiente al primer percentil si buscaramos el nivel de confianza 99%

[var_cvar_results.csv](https://github.com/user-attachments/files/18976860/var_cvar_results.csv)

[Upload"Method","VaR","CVaR"
"Normal 95%",-0.0238145489757356,-0.0273799487810273
"Normal 98%",-0.0278590175695967,-0.0286172936796462
"t-Student 95%",-0.0238145489757356,-0.0273799487810273
"t-Student 98%",-0.0278590175695967,-0.0286172936796462
ing var_cvar_results.csv…]()






```{r}

title: "Selección de Portafolio Óptimo"
author: "Laura Rodríguez Corona"
date: "2025-02-26"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#instalar paquetes necesarios

```{r}
if(!require("quantmod")) install.packages("quantmod")
```

```{r}
if(!require("PerformanceAnalytics")) install.packages("PerformaceAnalytics")
  
```


```{r}
# Cargar librerías
library(quantmod)
library(PerformanceAnalytics)
library(quadprog)

```

```{r}
install.packages("PortfolioAnalytics")
```

```{r}
library(PortfolioAnalytics)
```


```{r}
# Descargar datos de Yahoo Finance (NAFTRACISHRS y SPY)
getSymbols("NAFTRACISHRS.MX", src="yahoo", from="2024-02-20", to="2025-02-20")
getSymbols("SPY", src="yahoo", from="2024-02-20", to="2025-02-20")
```


```{r}
# Calcular retornos semanales de NAFTRACISHRS y SPY
returns_naftrac <- weeklyReturn(NAFTRACISHRS.MX$NAFTRACISHRS.MX.Close)
returns_spy <- weeklyReturn(SPY$SPY.Close)
```


```{r}
# Definir el retorno de CETES (tasa anualizada de CETES 28 días)
tasa_cetes_anual <- 0.11  # Ejemplo: 11% anual
retorno_cetes_semanal <- (1 + tasa_cetes_anual)^(1/52) - 1  # Convertir a retorno semanal
print(retorno_cetes_semanal)
```


```{r}
# Crear un vector de retornos semanales para CETES (constante)
#returns_cetes <- rep(retorno_cetes_semanal, length(returns_naftrac))
#print(returns_cetes)
```

```{r}
#crear un objeto xts con el retorno semanal de cetes
returns_cetes <- xts(rep(retorno_cetes_semanal, length(returns_naftrac)), order.by=index(returns_naftrac))  
```


```{r}
# Combinar retornos en una matriz usando merge.xts
returns  <- merge.xts(returns_naftrac, returns_spy , returns_cetes)
colnames(returns) <- c("NAFTRAC", "SPY", "CETES")
```

```{r}
#Eliminar filas con valores faltantes si es necesario
returns <- na.omit(returns)
```


```{r}
# Calcular matriz de varianzas y covarianzas
cov_matrix <- cov(returns)
print(cov_matrix)
```


```{r}
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
    portfolio_risks[i] <- sqrt(t(weights) %% cov_matrix %% weights)
    weights_list[[i]] <- weights
  }
  
  return(list(returns=portfolio_returns, risks=portfolio_risks, weights=weights_list))
}

print(returns)
```
```

