
mejor_valor <- function(valores, pesos, peso_max) {
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
  cat("\nSolucion:\n")
  cat ("Cantidad objetos", length(id_seleccionado), "\n")
  cat ("Peso total: ", peso_seleccionado, "\n")
  cat("Valor total:", sum(valores[id_seleccionado]),"\n")
  cat("Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}


mejor_peso <- function(valores, pesos, peso_max) {
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
  cat("\nSolucion:\n")
  cat ("Cantidad objetos", length(id_seleccionado), "\n")
  cat ("Peso total: ", peso_seleccionado, "\n")
  cat("Valor total:", sum(valores[id_seleccionado]),"\n")
  cat("Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}


mejor_razon <- function(valores, pesos, peso_max) {
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
  cat("\nSolucion:\n")
  cat ("Cantidad objetos", length(id_seleccionado), "\n")
  cat ("Peso total: ", peso_seleccionado, "\n")
  cat("Valor total:", sum(valores[id_seleccionado]),"\n")
  cat("Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}

solucion_random <- function(valores, pesos, peso_max) {
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
  cat("\nSolucion:\n")
  cat ("Cantidad objetos", length(id_seleccionado), "\n")
  cat ("Peso total: ", peso_seleccionado, "\n")
  cat("Valor total:", sum(valores[id_seleccionado]),"\n")
  cat("Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n")
  
  solucion = rep(0, length(valores))
  solucion[id_seleccionado] = 1
  names(solucion) = names(valores)
  return(solucion)
}

busqueda_local_bitflip <- function(solucion, valores, pesos, peso_max) {
  rho = max(valores/pesos)
  mejor_valor = feval(valores, pesos, peso_max, rho, which(solucion==1))
  mejor_solucion = solucion
  n_vecinos = length(solucion)
  cat ("# Busqueda local bitflip\n")
  cat ("# solucion inicial: ", which(solucion==1),"\n")
  cat ("# funcion de evaluacion: ", mejor_valor, "\n")
  
  
  i = 1
  while (i <= n_vecinos) {
    vecino = mejor_solucion
    if (vecino[i] == 1) 
      vecino[i] = 0
    else 
      vecino[i] = 1 
    vecino_valor = feval(valores, pesos, peso_max, rho, which(vecino==1))
    cat ("#   vecino ", i, " funcion de evaluacion: ", vecino_valor, "\n")
    
    if (vecino_valor > mejor_valor) {
      cat ("# mejor solucion encontrada: ", which(vecino==1), " funcion de evaluacion: ", vecino_valor, "\n")
      mejor_valor = vecino_valor
      mejor_solucion = vecino
      i = 1
    } else {
      i = i + 1
    }
  }
  cat("# mejor solucion encontrada: ", which(mejor_solucion==1), " funcion objetivo: ", mejor_valor,"\n")
  names(mejor_solucion) = names(valores)
  return(mejor_solucion)
}

feval <- function(valores, pesos, peso_max, rho, id_seleccionado) {
  peso_seleccionado = sum(pesos[id_seleccionado])
  if (peso_seleccionado > peso_max) {
    penalizacion = rho * (peso_seleccionado - peso_max)
  } else {
    penalizacion = 0
  }
  fx = sum(valores[id_seleccionado]) - penalizacion
  return(fx)
}

ils_bitflip <- function(solucion, valores, pesos, peso_max, max_evals=10000) {
  rho = max(valores/pesos)
  mejor_valor = incumbente_valor = feval(valores, pesos, peso_max, rho, which(solucion==1))
  mejor_solucion = incumbente_solucion = solucion
  n_vecinos = length(solucion)
  cat ("# Iterated local search bitflip\n")
  cat ("# solucion inicial: ", which(solucion==1),"\n")
  cat ("# funcion de evaluacion: ", mejor_valor, "\n")
  n_evals = 0
  iteracion = 1
  
  while (n_evals < max_evals){
    cat ("# Iteracion ILS ", iteracion, " evals:", n_evals,"\n")
    i = 1
    while (i <= n_vecinos && n_evals < max_evals) {
      n_evals = n_evals + 1
      vecino = incumbente_solucion
      if (vecino[i] == 1) 
        vecino[i] = 0
      else 
        vecino[i] = 1 
      vecino_valor = feval(valores, pesos, peso_max, rho, which(vecino==1))
      cat ("#   vecino ", i, " funcion de evaluacion: ", vecino_valor, "\n")
    
      if (vecino_valor > incumbente_valor) {
        cat ("# mejor solucion encontrada: ", which(vecino==1), " funcion de evaluacion: ", vecino_valor, "\n")
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