valores <- c(5000,2500,10,2500,2500,200,3000,500,100,10)
names(valores) <- c("cofre", "armadura", "basura", "escudo", "espada", "tridente", "casco", "arco", "varita", "pocion")
pesos <- c(25,20,20,12.5,10,10,7.5,4,1,1)peso_max = 30

mejor_valor <- function(valores, pesos, peso_max) {  
  peso_seleccionado <- 0  id_seleccionado <- c()     
  id_objetos_ordenados <- order(valores, decreasing=TRUE)    
  for( i in 1:length(id_objetos_ordenados)) {    
    index <- id_objetos_ordenados[i]    
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
  cat("Objetos seleccionados:", names(valores)[id_seleccionado] ,"\n
}
