# Trabajo Practico #1 de Probabilidades y Estadistica (Computacion)
#
# Alumnos:
# - white
# - jack
#
# Ejercicios.
# 1 - Album de 6 figuritas, sorteo una al azar, y "la pego" en ese lugar
cant_figuritas <- 6
# album vacio de 6 cant_figuritas
album <- rep((FALSE), cant_figuritas)
# figuritas de ese album
figus_posibles <- 1:cant_figuritas
figurita <- sample(figus_posibles,1)
album[figurita] <- TRUE

# 2 - album_lleno:
#     Recibe un album, devuelve TRUE si esta completo, FALSE si no
album_lleno = function (album) {
  for(i in 1:length(album)){
    if(!album[i]){
        return(FALSE)
    }
  }
  return(TRUE)
}

# 3 - generar_sobre:
#     Dados cant de figuritas por album y sobre,
#     devuelve un sobre aleatorio (incluyendo repetidas)
generar_sobre = function(cant_figuritas, cant_sobre, repetidas=TRUE){
  return(sample(1:cant_figuritas, cant_sobre, replace=repetidas))
}

# 3.b - Genero un sobre de 3 figuritas para un album de 6, y pego sus figuritas
cant_figuritas <- 6
cant_sobre <- 3
sobre <- generar_sobre(cant_figuritas, cant_sobre)
album <- rep((FALSE), cant_figuritas)
# busco cada figurita en mi album, y la pego
for(fig in sobre){
  album[fig] <- TRUE
}

 # 4 - pegar_sobre:
 #     Dado un album y un sobre, devuelve ese album con las figuritas del sobre tambien pegadas
pegar_sobre = function (album, sobre){
  for(fig in sobre){
    album[fig] <- TRUE
  }
  return(album)
}

# 4.b - Genere un album vacio.
cant_figuritas <- 6
album <- rep((FALSE), cant_figuritas)
# Genere un sobre y pegue las figuritas obtenidas. 
cant_sobre <- 3
sobre <- generar_sobre(cant_figuritas, cant_sobre)
album <- pegar_sobre(album, sobre)
# Genere otro sobre y pegue estas nuevas figuritas.
otro_sobre <- generar_sobre(cant_figuritas, cant_sobre)
album <- pegar_sobre(album, otro_sobre)
# Imprima el album en pantalla y compruebe si las ha pegado correctamente.
print("4b. Imprima el album en pantalla y compruebe si las ha pegado correctamente")
print("Album:")
print(album)
print("Sobre:")
print(sobre)
print("Otro sobre:")
print(otro_sobre)

# 5 - Dado cant de figuritas por sobre y album, simula el completado de
#     un album. Devuelve la cantidad de sobres que fueron necesarios.
cuantas_figuritas = function(cant_figuritas, cant_sobre, repetidas=TRUE){
  album <- rep((FALSE), cant_figuritas)
  cont <- 0
  while(!album_lleno(album)){
    sobre <- generar_sobre(cant_figuritas, cant_sobre, repetidas)
    album <- pegar_sobre(album, sobre)
    cont <- cont+1
  }
  return(cont)
}

# 6 - 3 simulaciones de completado de un album de 6 figuritas con sobres de 1.
print("Tres simulaciones de completado de un album de 6 figuritas con sobres de 1.")
print("Sobres necesarios para completar el album en cada simulacion:")
print(cuantas_figuritas(6,1))
print(cuantas_figuritas(6,1))
print(cuantas_figuritas(6,1))
# No son siempre las mismas, pues la cantidad de sobres necesarios depende de
# las figuritas que nos toquen, llenandolo mas rapido si suelen ser diferentes,
# y mas lento contra mas repetidas.

# 7 - Simulacion Montecarlo de1000 experimentos de llenado de
#     albunes de 670 y sobres de 5 figuritas.
cant_figuritas <- 670
cant_sobre <- 5
Nrep <- 1000

# Objetivos.
# Observaciones: 
#    Los algoritmos fueron pensados intentando mantener una correspondencia con la forma de
#    resolver este tipo de problemas a mano con las herramientas vistas en clase.
#    Por eso trabajamos directamente con los datos que se iban obteniendo en cada simulacion,
#    y optamos por evitar guardar todos los valores y trabajar con el array.
#    De todas formas, para la varianza estimada (D.) fue necesario guardar los valores primero.

# A. La probabilidad de completar el album con 800 sobres o menos
# Observaciones:
#     Si llamamos X a 'La probabilidad de completar el album con 800 sobres o menos',
#     lo que nos pide A. es: P(X <= 800), que es F_X(800): la funcion de acumulacion de X.
#     Definimos la funcion de forma general, y elegimos como valor 800 por defecto como pide
#     el ejercicio. 
funcion_de_acumulacion = function(Nrep, cant_figuritas, cant_sobre, hasta=800, repetidas=TRUE){
  cant <- 0
  casos_favorables <- 0
  for(i in 1:Nrep){
    # simulo llenado de un album
    cant <- cuantas_figuritas(cant_figuritas, cant_sobre, repetidas)
    if(cant < hasta){
      casos_favorables <- casos_favorables + 1
    }
  }
  return(casos_favorables/Nrep)
}
print("A. Probabilidad de completar el album con 800 o menos: ")
print(funcion_de_acumulacion(Nrep, cant_figuritas, cant_sobre))

# B. La cantidad de sobres que hacen falta comprar para completar
#    el album con probabilidad mayor o igual a 0.9
# Observaciones:
#     Esto es exactamente el percentil 0.90, por lo que definimos la funcion
#     de forma general, y agregamos el valor 0.9 por defecto para que responda a lo
#     pedido por el ejercicio.
percentil = function(Nrep, cant_figuritas, cant_sobre, p=0.9, repetidas=TRUE){
  #quiero: casos_favorables/Nrep = 0.9 =>  casos_fav = 0.9 * Nrep
  resultados <- rep(0, Nrep)
  # Guardo los Nrep experimentos
  for(i in 1:Nrep){
    resultados[i] <- cuantas_figuritas(cant_figuritas, cant_sobre, repetidas)
  }
  # Los ordeno y busco el 90% (p=0.9) del tamaño del vector (idx=900)
  # el cual indica el percentil 0.90
  resultados = sort(resultados)
  # me aseguro que en el indice haya un integer
  return (resultados[floor(p*Nrep)])
}
print("B. Cantidad de sobres para completar el album con 90% de chances: ")
print(percentil(Nrep, cant_figuritas, cant_sobre))

# C. El valor esperado de la cantidad de sobres necesarios
#    para completar el album del mundial de Rusia.
# Observaciones:
#     A medida que la cantidad de simulaciones tiende a infinito, el promedio de los resultados
#     obtenidos SERÁ la esperanza de X, ya que corresponde al promedio ponderado de los resultados
#     obtenidos (el 'ponderado' viene implicito en la distribucion de los resultados obtenidos)
esperanza_estimada = function(Nrep, cant_figuritas, cant_sobre, repetidas=TRUE){
  cant <- 0
  for(i in 1:Nrep){
    # el promedio de la suma de los resultados cuando Nrep es grande
    # se acerca al valor de la esperanza, siendo su limite en el infinito
    cant <- cant + cuantas_figuritas(cant_figuritas, cant_sobre, repetidas)
  }
  return(cant/Nrep)
}
print("C. Valor esperado de la cantidad de sobres necesarios: ")
print(esperanza_estimada(Nrep, cant_figuritas, cant_sobre))

# D. El desvio estandar de la cantidad de sobres necesarios para completar el album.
# Usando la ecuacion de la varianza que calcula una a una las diferencias entre 
# la esperanza de una va. y su valor particular en cada simulacion:
# eq: V(X)=E((x-u)^2), u=E(X)
# Observaciones:
#     Primero definimos la varianza de la forma pedida.
#     Para eso simulamos Nrep experimentos y obtenemos SU esperanza estimada (mu).
#     Usamos esta esperanza para calcular la varianza de una nueva serie de Nrep experimentos.
#     Como el limite de la esperanza estimada para Nrep tendiendo a infinito es el mismo para
#     cualquier serie de experimentos, es valido que usemos el mu de una serie, para calcular
#     la esperanza de otra.
#     Si la cantidad de experimentos estuviera limitada a una cantidad mucho menor, deberiamos
#     modificar el algoritmo para que calcule la varianza a partir de LOS MISMOS experimentos
#     que fueron usados para calcular la esperanza.
varianza_estimada = function(Nrep, cant_figuritas, cant_sobre, repetidas=TRUE){
  acum <- 0
  mu <- esperanza_estimada(Nrep, cant_figuritas, cant_sobre, repetidas)
  for(i in 1:Nrep){
    result <- cuantas_figuritas(cant_figuritas, cant_sobre, repetidas)
    acum <- acum + (result - mu)^2
  }
  return (acum/Nrep)
}

desvio_estandar = function(Nrep, cant_figuritas, cant_sobre, repetidas=TRUE){
  varianza <- varianza_estimada(Nrep, cant_figuritas, cant_sobre, repetidas=TRUE)
  # devuelve la raiz cuadrada de la varianza
  return (sqrt(varianza))
}
print("D. Desvio estandar de la cantidad de sobres necesarios: ")
print(desvio_estandar(Nrep, cant_figuritas, cant_sobre))

# 8 - Repetir lo mismo que en 7, pero los sobres traen figuritas no repetidas
# Para eso agregamos una variable booleana en las funciones, para que el proceso
# de simulacion corresponda a si queremos sobres con figuritas que puedan repetirse o no

print("A. Probabilidad de completar el album con 800 o menos (sin repetidas): ")
print(funcion_de_acumulacion(Nrep, cant_figuritas, cant_sobre, hasta=800, repetidas=FALSE))

print("B. Cantidad de sobres para completar el album con 90% de chances (sin repetidas): ")
print(percentil(Nrep, cant_figuritas, cant_sobre, p=0.9, repetidas=FALSE))

print("C. Valor esperado de la cantidad de sobres necesarios (sin repetidas): ")
print(esperanza_estimada(Nrep, cant_figuritas, cant_sobre, repetidas=FALSE))

print("D. Desvio estandar de la cantidad de sobres necesarios (sin repetidas): ")
print(desvio_estandar(Nrep, cant_figuritas, cant_sobre, repetidas=FALSE))

# 9 - Do something cool
# Graficamos los histogramas de #Nrep experimentos con repetidos y sin repetidos
# En general notamos un ligero desplazamiento del plot sin repetidos hacia la izquierda, 
# correspondiente con la estimacion de la esperanza, lo que para este problema significa que si los paquetes
# de figuritas vienen sin repetidos, es mas facil llenar el album (menos sobres necesarios)
# Con sobres de mayor cantidad de figuritas, esta diferencia es mas notable.
histograma = function(Nrep, cant_figuritas, cant_sobre, hasta=800){
  resultados_con_repetidas <- rep(0, Nrep)
  resultados_sin_repetidas <- rep(0, Nrep)
  # Guardo los Nrep experimentos
  for(i in 1:Nrep){
    # simulo llenado de un album permitiendo que salgan repetidas y guardo resultado
    resultados_con_repetidas[i] <- cuantas_figuritas(cant_figuritas, cant_sobre, repetidas=TRUE)
    # lo mismo, con sobres de figuritas no repetidas
    resultados_sin_repetidas[i] <- cuantas_figuritas(cant_figuritas, cant_sobre, repetidas=FALSE)
  }
  #Armo un histograma de los resultados con repetidas
  plot1 <- hist(resultados_con_repetidas)
  #Armo otro histograma pero con los casos sin repetidas
  plot2 <- hist(resultados_sin_repetidas)
  # Agrego hists a un mismo plot
  plot(plot1, col=rgb(0,0,1, 1/3),
        main="Frecuencia de sobres necesarios",
        xlab="Sobres necesarios para completar el album",
        ylab="Frecuencia")
  plot(plot2, col=rgb(1,0,0, 1/3), add=T)
  # Esperanza(mu) y desvio estandar(sigma) con repetidas
  mu_con_repes = mean(resultados_con_repetidas)
  abline(v = mu_con_repes, col=rgb(0,0,1,0.7), lwd = 5)
  sigma_con_repes = sqrt(var(resultados_con_repetidas))
  # mu + sigma
  abline(v = mu_con_repes + sigma_con_repes, col=rgb(0,0,1,1/2), lwd = 1)
  # mu - sigma
  abline(v = mu_con_repes - sigma_con_repes, col=rgb(0,0,1,1/2), lwd = 1)
  # Esperanza sin repetidas
  mu_sin_repes = mean(resultados_sin_repetidas)
  abline(v = mu_sin_repes, col=rgb(1,0,0,0.7), lwd = 5)
  sigma_sin_repes = sqrt(var(resultados_sin_repetidas))
  # mu + sigma
  abline(v = mu_sin_repes + sigma_sin_repes, col=rgb(1,0,0,1/2), lwd = 1)
  # mu - sigma
  abline(v = mu_sin_repes - sigma_sin_repes, col=rgb(1,0,0,1/2), lwd = 1)
  # leyenda de todas las lineas y colores en el plot
  legend('topright', c("Con repetidas", "Sin repetidas", "mu c/repes", "mu+/-sigma c/repes", "mu s/repes", "mu+/-sigma s/repes"),
         cex=.8, col=c(rgb(0,0,1,1/3), rgb(1,0,0,1/3), rgb(0,0,1,0.7), rgb(0,0,1,1/2), rgb(1,0,0,0.7), rgb(1,0,0,1/2)),
         lty=c(NA,NA,1,1,1,1), lwd=c(10,10,5,1,5,1), pch=c(0,0,NA,NA,NA,NA))
  # marcas del eje x
  # axis(1, pch=18, las=2, at=c(mu_con_repes, mu_sin_repes,
  #              round(mu_con_repes + sigma_con_repes,2), round(mu_con_repes - sigma_con_repes,2),
  #              round(mu_sin_repes + sigma_sin_repes,2), round(mu_sin_repes - sigma_sin_repes)))
  text(x=round(mu_con_repes, 2), y=-0.3, cex=0.7,pos=3, c(round(mu_con_repes, 2)))
  text(x=round(mu_con_repes + sigma_con_repes,2), y=-0.3, cex=0.7,pos=3, c(round(mu_con_repes + sigma_con_repes, 2)))
  text(x=round(mu_con_repes - sigma_con_repes,2), y=-0.3, cex=0.7,pos=3, c(round(mu_con_repes - sigma_con_repes, 2)))
  text(x=round(mu_sin_repes, 2), y=0.5, cex=0.7,pos=1, c(round(mu_sin_repes, 2)))
  text(x=round(mu_sin_repes + sigma_sin_repes,2), y=0.5, cex=0.7,pos=1, c(round(mu_sin_repes + sigma_sin_repes, 2)))
  text(x=round(mu_sin_repes - sigma_sin_repes,2), y=0.5, cex=0.7,pos=1, c(round(mu_sin_repes - sigma_sin_repes, 2)))
}
cant_figuritas <- 670
cant_sobre <- 5
print("Ejercicio 9.")
print("Mostrando histograma de los experimentos obtenidos:")
histograma(1000, cant_figuritas, cant_sobre)
