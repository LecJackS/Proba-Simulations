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
  return(sample(1:cant_figuritas, cant_sobre, repetidas))
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
print(album)
print(sobre)
print(otro_sobre)

# 5 - Dado cant de figuritas por sobre y album, simula el completado de
#     un album. Devuelve la cantidad de sobres que fueron necesarios.
cuantas_figuritas = function(cant_figuritas, cant_sobre, repetidas=TRUE){
  album <- rep((FALSE), cant_figuritas)
  cont <- 0
  while(!esta_lleno(album)){
    sobre <- generar_sobre(cant_figuritas, cant_sobre, repetidas)
    album <- pegar_sobre(album, sobre)
    cont <- cont+1
  }
  return(cont)
}

# 6 - 3 simulaciones de completado de un album de 6 figuritas con sobres de 1.
cuantas_figuritas(6,1)
cuantas_figuritas(6,1)
cuantas_figuritas(6,1)
# No son siempre las mismas, pues la cantidad de sobres necesarios depende de
# las figuritas que nos toquen, llenandolo mas rapido si suelen ser diferentes,
# y mas lento contra mas repetidas.

# 7 - Simulacion Montecarlo de1000 experimentos de llenado de
#     albunes de 670 y sobres de 5 figuritas.
cant_figuritas <- 670
cant_sobre <- 5
Nrep <- 1000

# Objetivos

# A. La probabilidad de completar el album con 800 sobres o menos

funcion_de_acumulacion = function(Nrep, cant_figuritas, cant_sobre, hasta=800, repetidas=TRUE){
  cant <- 0
  casos_favorables <- 0
  for(i in 1:Nrep){
    # simulo llenado de un album
    cant <- cuantas_figuritas(cant_figuritas, cant_sobre)
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
percentil = function(Nrep, cant_figuritas, cant_sobre, p=0.9, repetidas=TRUE){
  #quiero: casos_favorables/Nrep = 0.9 =>  casos_fav = 0.9 * Nrep
  resultados <- rep(0, Nrep)
  # Guardo los Nrep experimentos
  for(i in 1:Nrep){
    resultados[i] <- cuantas_figuritas(cant_figuritas, cant_sobre)
  }
  # Los ordeno y busco el 90% (p=0.9) del tamaño del vector (idx=900)
  # el cual indica el percentil 0.90
  # (me aseguro que en el indice haya un integer)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> PREGUNTAR
  return (resultados[floor(p*Nrep)])
}
print("B. Cantidad de sobres para completar el album con 90% de chances: ")
print(percentil(Nrep, cant_figuritas, cant_sobre))

# C. El valor esperado de la cantidad de sobres necesarios
#    para completar el album del mundial de Rusia.
esperanza_estimada = function(Nrep, cant_figuritas, cant_sobre, repetidas=TRUE){
  cant <- 0
  for(i in 1:Nrep){
    # el promedio de la suma de los resultados cuando Nrep es grande
    # se acerca al valor de la esperanza, siendo su limite en el infinito
    cant <- cant + cuantas_figuritas(cant_figuritas, cant_sobre)
  }
  return(cant/Nrep)
}
print("C. Valor esperado de la cantidad de sobres necesarios: ")
print(esperanza_estimada(Nrep, cant_figuritas, cant_sobre))

# D. El desvio estandar de la cantidad de sobres necesarios para completar el album.
# Usando la ecuacion de la varianza que calcula una a una las diferencias entre 
# la esperanza de una va. y su valor particular en cada simulacion:
# eq: V(X)=E((x-u)^2), u=E(X)
varianza_estimada = function(Nrep, cant_figuritas, cant_sobre, repetidas=TRUE){
  acum <- 0
  mu <- esperanza_estimada(Nrep, cant_figuritas, cant_sobre)
  for(i in 1:Nrep){
    result <- cuantas_figuritas(cant_figuritas, cant_sobre)
    acum <- acum + (result - mu)^2
  }
  return (acum/Nrep)
}
print("D. Varianza estimada de la cantidad de sobres necesarios: ")
print(varianza_estimada(Nrep, cant_figuritas, cant_sobre))

# 8 - Repetir lo mismo que en 7, pero los sobres traen figuritas no repetidas
# Para eso agregamos una variable booleana en las funciones, para que el proceso
# de simulacion corresponda a si queremos sobres con figuritas que puedan repetirse o no

print("A. Probabilidad de completar el album con 800 o menos (sin repetidas): ")
print(funcion_de_acumulacion(Nrep, cant_figuritas, cant_sobre, hasta=800, repetidas=FALSE))

print("B. Cantidad de sobres para completar el album con 90% de chances (sin repetidas): ")
print(percentil(Nrep, cant_figuritas, cant_sobre, p=0.9, repetidas=FALSE))

print("C. Valor esperado de la cantidad de sobres necesarios (sin repetidas): ")
print(esperanza_estimada(Nrep, cant_figuritas, cant_sobre, repetidas=FALSE))

print("D. Varianza estimada de la cantidad de sobres necesarios (sin repetidas): ")
print(varianza_estimada(Nrep, cant_figuritas, cant_sobre, repetidas=FALSE))
