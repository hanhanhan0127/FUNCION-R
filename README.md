# FUNCION-R
agruparDatosConFor <- function(datos, num_intervalos) {
  # Paso 1: Ordenar los datos en orden ascendente
  datos_ordenados <- sort(datos)
  
  # Paso 2: Calcular el rango de los datos
  rango <- max(datos_ordenados) - min(datos_ordenados)
  
  # Paso 3: Calcular el ancho del intervalo
  ancho_intervalo <- rango / num_intervalos
  
  # Paso 4: Inicializar la matriz de intervalos
  intervalos <- matrix(0, nrow = num_intervalos, ncol = 2)
  
  # Paso 5: Calcular los límites de los intervalos
  for (i in 1:num_intervalos) {
    inicio_intervalo <- min(datos_ordenados) + (i - 1) * ancho_intervalo
    fin_intervalo <- min(datos_ordenados) + i * ancho_intervalo
    intervalos[i,] <- c(inicio_intervalo, fin_intervalo)
  }
  
  # Paso 6: Contar las frecuencias de los datos en cada intervalo
  frecuencias <- rep(0, num_intervalos)
  for (i in 1:num_intervalos) {
    frecuencias[i] <- sum(datos_ordenados >= intervalos[i, 1] & datos_ordenados <= intervalos[i, 2])
  }
  
  # Paso 7: Crear un data frame con los resultados
  resultado <- data.frame(Inicio = intervalos[,1], Fin = intervalos[,2], Frecuencia = frecuencias)
  
  return(resultado)
}

#Probando la funciòn 
datos <- c(10, 15, 25, 30, 35, 40, 45, 50, 55, 60)
num_intervalos <- 3

resultado <- agruparDatosConFor(datos, num_intervalos)
print(resultado)
