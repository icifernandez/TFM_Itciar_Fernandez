library(dplyr)
library(readxl)
library(ineq)
library(data.table)
library(DT)

#Importamos nuestra base de datos.
#La base de datos se encuentra disponible en el siguiente enlace: https://digibug.ugr.es/handle/10481/93386

setwd("C:/Users/icife/OneDrive/Escritorio/Escritorio/Master Universitario en Matemáticas/TFM")
datos <- read_excel("ITCIAR_variablesTFM.xlsx", sheet = "Datos")
head(datos)

#Podemos ver a continuación la clasificación final de La Liga en la temporada pasada.

clasificacion = read_excel("ITCIAR_variablesTFM.xlsx", sheet= "Clasificacion")
clasificacion

#Vamos a calcular ahora las medias, desviaciones típicas, mínimos y máximos de cada una de las variables para cada equipo.

attach(datos)
# Definir las variables y equipos
variables <- c("LV", "GF", "GC", "xGF", "xGC", "GP", "PB", "R", "RP", "RC", "RPC", "RF", "CF", "CC", "FJ", "Ps", "FTF", "FTC", "FG", "FC", "TA", "TR", "AF", "AFP", "AC", "ACP", "CA", "RB", "P", "I", "CE", "FMI", "PI", "GF/RP", "RP/AFP", "FTF/FTC", "PB/(FTF+FTC)", "GF/xGF", "FTF/(FTF+FTC)", "RP/(RP+RPC)", "FJ/AF", "FTF/PB", "GF/PB")
equipos <- c("Osasuna", "Sevilla", "Celta de Vigo", "Espanyol", "Real Valladolid", "Villarreal", "Barcelona", "Rayo Vallecano", "Cádiz", "Real Sociedad", "Valencia", "Girona", "Almería", "Real Madrid", "Athletic Club", "Mallorca", "Getafe", "Atletico de Madrid", "Real Betis", "Elche")

# Crear un dataframe vacío para las salidas
salida <- data.frame(Equipo = equipos)

# Función para calcular y formatear los datos
calcular_datos <- function(equipo, variable) {
  data <- datos %>% filter(Equipo == equipo) %>% pull(variable)
  media <- mean(data, na.rm = TRUE)
  sd <- sd(data, na.rm = TRUE)
  sd_rounded <- round(sd, 4)
  min_val <- min(data, na.rm = TRUE)
  max_val <- max(data, na.rm = TRUE)
  return(sprintf("%.4f (%.4f)<br>(%.2f, %.2f)", media, sd_rounded, min_val, max_val))
}

# Iterar sobre las variables y equipos para obtener los datos
for (variable in variables) {
  resultados <- sapply(equipos, function(equipo) calcular_datos(equipo, variable))
  salida[[variable]] <- resultados
}
datatable(salida, escape = FALSE, 
          options = list(
            pageLength = 20,
            lengthMenu = c(10, 20, 50),
            columnDefs = list(
              list(targets = 0, className = 'dt-center'), # Centro la primera columna (Equipo)
              list(targets = '_all', className = 'dt-center') # Centro todas las columnas
            ),
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          ),
          class = 'compact display nowrap')

# El resultado, en este caso toma dos valores, ganar 1 o no ganar 0.

n =length(Resultado)
Resultado2=array(0,n)
for (i in 1:n){
  if(Resultado[i] == 3){Resultado2[i]=1}
  if(Resultado[i] != 3){Resultado2[i]=0}
}
  
# Comparación de medias según los resultados
  attach(datos)
  # Definir las variables de interés
  variables2 =  c("LV", "GF", "GC", "GP", "PB", "R", "RP", "RC", "RPC", "RF", "CF", "CC", "FJ", "Ps", "FTF", "FTC", "FG", "FC", "TA", "TR", "AF", "AFP", "AC", "ACP", "CA", "RB", "P", "I", "CE", "FMI", "PI", "GF/RP", "RP/AFP", "FTF/FTC", "PB/(FTF+FTC)", "FTF/(FTF+FTC)", "RP/(RP+RPC)", "FJ/AF", "FTF/PB", "GF/PB")
  resultados2 = Resultado2
  
  # Crear un dataframe con las variables de interés y el resultado
  datos_analisis <- datos %>%
    select(all_of(variables2), Resultado)
  
  columna_extra =resultados2 
  
  # Agregar la columna extra al dataframe de análisis
  datos_analisis <- mutate(datos_analisis, columna_extra = columna_extra)
  
  datos_analisis2 <- datos_analisis %>%
    select(all_of(variables2), columna_extra)
  
  
  # Calcular las medias por grupo (victoria, empate y derrota)
  medias_por_resultado <- datos_analisis %>%
    group_by(Resultado) %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  # Calcular las medias por grupo (victoria/derrota)
  medias_por_resultado2 <- datos_analisis2 %>%
    group_by(columna_extra) %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  # Mostrar la tabla de medias por resultado
  print(medias_por_resultado)
  print(medias_por_resultado2)
  
  
#Realizamos ahora un test de comparación de medias.
  
  for (variable in variables2) {
    # Verifica si la variable existe en los datos
    if (!(variable %in% colnames(datos_analisis))) {
      cat("Variable", variable, "no encontrada en los datos.\n")
      next
    }
    
    # Extraer los datos de la variable y de resultados2
    variable_data <- datos_analisis[[variable]]
    
    # Verificar si hay valores faltantes
    if (any(is.na(variable_data))) {
      cat("Variable", variable, "contiene valores faltantes. Omitiendo.\n")
      next
    }
    
    # Verificar si los datos son constantes
    if (length(unique(variable_data)) == 1) {
      cat("Variable", variable, "es constante. Omitiendo.\n")
      next
    }
    
    # Crear una fórmula usando as.formula()
    formula <- as.formula(paste(variable, "~ resultados2"))
    
    # Manejo de excepciones durante el test t
    tryCatch({
      # Realizar el test t
      t_test_result <- t.test(formula, data = datos_analisis)
      
      # Mostrar el resultado del test t
      print(t_test_result)
    }, error = function(e) {
      cat("Error al realizar el test t para la variable", variable, ": ", e$message, "\n")
    })
  }
  
#Veamos ahora como se distribuyen las variables, centrándonos en aquellas que vamos a incluir en el modelo.
  
  momentos = function(x, r, mp = 0) {
    m = sum((x-mp)^r)/length(x)
    return(m)
  }
  
  TODO = function(x, alfa = 0.95, graf = F) {
    n = length(x)
    media = mean(x, na.rm=TRUE)
    tabla.estadistica = table(x)
    posicion = which.max(table(x))
    moda = as.double(rownames(tabla.estadistica)[posicion])
    varianza = var(x, na.rm =TRUE)
    desv.tip = sqrt(varianza)
    cv = desv.tip / media
    m3 = momentos(x, 3, mean(x, na.rm=TRUE))
    simetria = m3/(desv.tip^3)
    m4 = momentos(x, 4, mean(x, na.rm=TRUE))
    kurtosis = m4/(varianza^2) - 3
    gini = ineq(x, type="Gini", na.rm=TRUE)
    if(graf == T) {plot(Lc(x), col="red", lwd=2)}
    salida = data.frame(n, cv, simetria, kurtosis, gini)
    rownames(salida) = c("")
    return(salida)
  }  
  resultados = list()
  
  for (variable in variables) {
    EQUIPO = datos %>% pull(variable)
    resultados[[variable]] = t(TODO(EQUIPO))
  }
  
  # Combinar los resultados en un marco de datos
  tabla_resultados <- do.call(cbind, resultados)
  colnames(tabla_resultados) <- variables
  round(tabla_resultados, digits = 3)
  
#Veamos ahora gráficamente la distribución de las variables elegidas.
  
  library(ggplot2)
  
  # Gráficos de barras
  ggplot(datos, aes(x = LV, y = LV)) + geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Histograma de Goles a Favor", x ="Goles a Favor") + theme_minimal() 
  ggplot(datos, aes(x = GF, y = GF)) + geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Histograma de Goles a Favor", x ="Goles a Favor") + theme_minimal() 
  ggplot(datos, aes(x = RP, y = RP)) + geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Histograma de Remates a Puerta", x ="Remates a Puerta") + theme_minimal()
  ggplot(datos, aes(x = Ps, y = Ps)) + geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Histograma de Paradas", x ="Paradas") + theme_minimal()
  ggplot(datos, aes(x = TR, y = TR)) + geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Histograma de Tarjetas Rojas", x ="Tarjetas Rojas") + theme_minimal()
  ggplot(datos, aes(x = CA, y = CA)) + geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Histograma de Centros al Área", x ="Centros al Área") + theme_minimal()
  ggplot(datos, aes(x = CA, y = CA)) +
    geom_bar(stat = "identity", fill = "skyblue") + # Barras con relleno azul cielo
    labs(title = "Distribución de valores en Centros al Área", x ="Valores", y = "Frecuencia") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), # Eliminar la malla principal
          panel.grid.minor = element_blank(),  # Eliminar la malla secundaria
          panel.border = element_rect(color = "black", fill = NA)) 
  
# Vamos a calcular a continuación la matriz de correlación de las variables seleccionadas
  
  library(corrplot)
  # Seleccionar el subconjunto de variables
  subset_datos <- datos[, c( "GF", "R", "RP", "TR", "CA", "GF/RP", "RP/AFP", "GF/PB")]
  
  # Calcular la matriz de correlación
  matriz_correlacion <- cor(subset_datos, use = "complete.obs")
  
  # Imprimir la matriz de correlación
  print(matriz_correlacion)
  corrplot(matriz_correlacion, method = "color", type = "upper", 
           tl.col = "black", tl.srt = 8, tl.cex = 0.8, addCoef.col = "black", number.cex = 0.8, cl.cex = 0.8)
  
#Ajustamos el modelo Logit
  logit = glm(Resultado2~LV+GF+RP+Ps+TR+CA,family=binomial("logit"))
  summary(logit)
  
#Calculamos el VIF

  library(car)
  vif(logit)

#Calculemos el oddratio.
  
  exp(logit$coefficients[-1]) # quito el coeficiente estimado de la constante
  l=c(1/0.8807420,1/0.3126082,1/0.8979107)
  l
#Veamos las tasas de aciertos

  TASA_ACIERTOS <- function(umbral, valores_estimados, valores_observados)
  {
    A = 0
    B = 0
    C = 0
    D = 0
    n = length(valores_observados)
    for (i in 1:n){
      if ((valores_estimados[i]>=umbral) & valores_observados[i]==1){A = A + 1}
      if ((valores_estimados[i]>=umbral) & valores_observados[i]==0){B = B + 1}
      if ((valores_estimados[i]<umbral) & valores_observados[i]==1){C = C + 1}
      if ((valores_estimados[i]<umbral) & valores_observados[i]==0){D = D + 1}
    }
    exito_aciertos_A = (A/n)*100
    exito_errores_B = (B/n)*100
    fracaso_errores_C = (C/n)*100
    fracaso_aciertos_D = (D/n)*100
    tasa_aciertos = ((A+D)/n)*100
    salida = data.frame(exito_aciertos_A, exito_errores_B, fracaso_errores_C, fracaso_aciertos_D, tasa_aciertos)
    return(salida)
  }
  
  TASA_ACIERTOS(mean(Resultado2), fitted.values(logit), Resultado2)
  
  
