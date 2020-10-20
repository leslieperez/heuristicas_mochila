#############################################################################
##             Heuristicas constructivas - Problema de la mochila          ##
##                                                                         ##
##                        Investigacion de Operaciones                     ##
##                      Profesora: Leslie Perez Caceres                    ##
##                  Escuela de Ingenieria Informatica - PUCV               ##
##                                                                         ##
#############################################################################

## Heuristica del mejor valor: añade objetos, comenzando por los
## objetos con mayor valor, hasta que no es posible añadir mas
##
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##
##  retorno: vector binario 0/1 con los objetos seleccionados
mejor_valor <- function (instancia) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max
  peso_seleccionado <- 0
  id_seleccionado <- c() 
  
  id_objetos_ordenados <- order(valores, decreasing=TRUE)
  
  for( index in id_objetos_ordenados){
    if ((peso_seleccionado + pesos[index]) <= peso_max) {
      id_seleccionado <- c(id_seleccionado, index)
      peso_seleccionado <- peso_seleccionado + pesos[index]
    }
  }  
  id_seleccionado <- sort(id_seleccionado)
  cat("\nSolucion heuristica mejor valor:\n")
  cat (" Cantidad objetos", length(id_seleccionado), "\n")
  cat (" Peso total: ", peso_seleccionado, "\n")
  cat (" Valor total:", sum(valores[id_seleccionado]),"\n")
  cat (" Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}

## Heuristica del mejor peso: añade objetos, comenzando por los
## objetos con menor peso, hasta que no es posible añadir mas
##
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##
##  retorno: vector binario 0/1 con los objetos seleccionados
mejor_peso <- function (instancia) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max
  peso_seleccionado <- 0
  id_seleccionado <- c() 
  
  id_objetos_ordenados <- order(pesos)
  
  for( i in 1:length(id_objetos_ordenados)){
    index <- id_objetos_ordenados[i]
    if ((peso_seleccionado+pesos[index]) <= peso_max) {
      id_seleccionado <- c(id_seleccionado, index)
      peso_seleccionado <- peso_seleccionado + pesos[index]
    }
  }  
  id_seleccionado<- sort(id_seleccionado)
  cat("\nSolucion heuristica mejor peso:\n")
  cat (" Cantidad objetos", length(id_seleccionado), "\n")
  cat (" Peso total: ", peso_seleccionado, "\n")
  cat (" Valor total:", sum(valores[id_seleccionado]),"\n")
  cat (" Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}

## Heuristica de la mejor razon: añade objetos, comenzando por los
## objetos con mayor razon valor/peso, hasta que no es posible añadir mas
##
##   instancia: lista con los siguientes elementos:
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##
##  retorno: vector binario 0/1 con los objetos seleccionados
mejor_razon <- function(instancia) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max
  peso_seleccionado <- 0
  id_seleccionado <- c() 
  
  id_objetos_ordenados <- order((valores / pesos), decreasing=TRUE)
  
  for( index in id_objetos_ordenados) {
    if ((peso_seleccionado+pesos[index]) <= peso_max) {
      id_seleccionado <- c(id_seleccionado, index)
      peso_seleccionado <- peso_seleccionado + pesos[index]
    }
  }  
  id_seleccionado<- sort(id_seleccionado)
  cat("\nSolucion heuristica mejor razon:\n")
  cat (" Cantidad objetos", length(id_seleccionado), "\n")
  cat (" Peso total: ", peso_seleccionado, "\n")
  cat (" Valor total:", sum(valores[id_seleccionado]),"\n")
  cat (" Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}

## Generacion de solucion aleatoria
##
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##
##  retorno: vector binario 0/1 con los objetos seleccionados
solucion_random <- function (instancia) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max
  peso_seleccionado = 0
  id_seleccionado = c()
  id_no_seleccionado = seq(1,length(valores))
  
  while (length(id_no_seleccionado)>0) {
    i = sample(seq(1,length(id_no_seleccionado)), size = 1)
    index = id_no_seleccionado[i]
    id_no_seleccionado = id_no_seleccionado[-i]
    if (peso_seleccionado + pesos[index] < peso_max){
      id_seleccionado = c(id_seleccionado, index)
      peso_seleccionado = peso_seleccionado + pesos[index]
    }
  }
  
  id_seleccionado<- sort(id_seleccionado)
  cat("\nSolucion aleatoria:\n")
  cat (" Cantidad objetos", length(id_seleccionado), "\n")
  cat (" Peso total: ", peso_seleccionado, "\n")
  cat (" Valor total:", sum(valores[id_seleccionado]),"\n")
  cat (" Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}

## Generacion de solucion vacia
##
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##
##  retorno: vector binario 0/1 con los objetos seleccionados
solucion_vacia <- function (instancia) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max

  cat("\nSolucion vacia:\n")
  cat (" Cantidad objetos: 0\n")
  cat (" Peso total: 0 \n")
  cat (" Valor total: 0 \n")
  cat (" Objetos seleccionados: ninguno\n")
  
  solucion = rep(0, length(valores))
  names(solucion) = names(valores)
  return(solucion)
}


## Busqueda local primera mejora utilizando el movimiento bitflip
##
##   - solucion: vector binario 0/1 con los objetos seleccionados en una la 
##               solucion inicial
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##
##  retorno: vector binario 0/1 con los objetos seleccionados
busqueda_local_bitflip <- function(solucion, instancia) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max
  rho = max(valores/pesos)
  mejor_valor = feval(valores, pesos, peso_max, rho, which(solucion==1))
  mejor_solucion = solucion
  n_vecinos = length(solucion)
  cat ("\n# Busqueda local bitflip\n")
  cat ("# solucion inicial: ", which(solucion==1),"\n")
  cat ("# funcion de evaluacion: ", mejor_valor, "\n")
  
  i = 1
  ridx <- 1:n_vecinos
  ridx <- sample(ridx, size=n_vecinos)
  while (i <= n_vecinos) {
    vecino = mejor_solucion
    if (vecino[ridx[i]] == 1) {
      vecino[ridx[i]] = 0
      op <- "1->0"
    } else { 
      vecino[ridx[i]] = 1 
      op <- "0->1"
    }
    vecino_valor = feval(valores, pesos, peso_max, rho, which(vecino==1))
    #cat ("#   vecino ", i, ", objeto:", ridx[i] , " " , op, " feval: ", vecino_valor, "\n")
    
    if (vecino_valor > mejor_valor) {
      cat ("#   mejora vecino ", i, ", objeto:", ridx[i] , " " , op, " feval: ", vecino_valor, "\n")
      mejor_valor = vecino_valor
      mejor_solucion = vecino
      i = 1
    } else {
      i = i + 1
    }
  }
  cat("# optimo local encontrado: ", which(mejor_solucion==1), " funcion objetivo: ", mejor_valor,"\n")
  names(mejor_solucion) = names(valores)
  return(mejor_solucion)
}

## Busqueda local iterativa utilizando el movimiento bitflip y perturbacion
## bitflip aplicado aleatoriamente 3 veces
##
##   - solucion: vector binario 0/1 con los objetos seleccionados en una la 
##               solucion inicial
##   - instancia: lista con los siguientes elementos:
##     * valores: vector de valores de los objetos
##     * pesos: vector dew pesos de los objetos
##     * peso_max: peso maximo para los objetos seleccionado
##   - max_evals: numero maximo de soluciones evaluadas
##
##  retorno: vector binario 0/1 con los objetos seleccionados
ils_bitflip <- function(solucion, instancia, max_evals=10000) 
{
  valores <- instancia$valores
  pesos <- instancia$pesos
  peso_max <- instancia$peso_max
  rho = max(valores/pesos)
  mejor_valor = incumbente_valor = feval(valores, pesos, peso_max, rho, which(solucion==1))
  mejor_solucion = incumbente_solucion = solucion
  n_vecinos = length(solucion)
  cat ("# Busqueda local iterativa bitflip\n")
  cat ("# solucion inicial: ", which(solucion==1),"\n")
  cat ("# funcion de evaluacion: ", mejor_valor, "\n")
  n_evals = 0
  iteracion = 1
  
  while (n_evals < max_evals){
    cat ("# Iteracion ILS ", iteracion, " evals:", n_evals,"\n")
    i = 1
    ridx <- 1:n_vecinos
    ridx <- sample(ridx, size=n_vecinos)
    while (i <= n_vecinos && n_evals < max_evals) {
      n_evals = n_evals + 1
      vecino = incumbente_solucion
      if (vecino[ridx[i]] == 1) {
        vecino[ridx[i]] = 0
        op <- "1->0"
      } else { 
        vecino[ridx[i]] = 1 
        op <- "0->1"
      }
      vecino_valor = feval(valores, pesos, peso_max, rho, which(vecino==1))
      #cat ("#   vecino ", i, ", objeto:", ridx[i] , " " , op, " feval: ", vecino_valor, "\n")
    
      if (vecino_valor > incumbente_valor) {
        cat ("#   mejora vecino ", i, ", objeto:", ridx[i] , " " , op, " feval: ", vecino_valor, "\n")
        incumbente_valor = vecino_valor
        incumbente_solucion = vecino
        i = 1
      } else {
        i = i + 1
      }
    }
    if (incumbente_valor > mejor_valor ) {
      mejor_solucion = incumbente_solucion
      mejor_valor = incumbente_valor
    }
    incumbente_solucion = mejor_solucion
    for (i in 1:3) {
      index = sample(1:length(incumbente_solucion), size = 1)
      if (incumbente_solucion[index] == 1) incumbente_solucion[index] = 0
      else incumbente_solucion[index] =1
    }
    incumbente_valor = feval(valores, pesos, peso_max, rho, which(incumbente_solucion==1))
    iteracion = iteracion + 1
  }
  cat("# mejor solucion encontrada: ", which(mejor_solucion==1), " funcion objetivo: ", mejor_valor,"\n")
  names(mejor_solucion) = names(valores)
  return(mejor_solucion)
}

