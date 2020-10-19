## función que lee una instancia del problema de la mochila en el formato:
## https://github.com/likr/kplib
##  - file: string con la ruta al archivo de instancias
##
##  retorno: lista que contiene:
##           - valores: vector de valores de los objetos
##           - pesos: vector de pesos de los objetos
##           - n: cantidad de objetos
##           - peso_max: peso maximo para seleccion de objetos
leer_instancia <- function(file) {
  data <- read.table(file=file, skip = 3, blank.lines.skip = TRUE, header=FALSE)
  valores <- data[,1]
  pesos <- data[,2]
  data <- read.table(file=file, blank.lines.skip = TRUE, header=FALSE, nrows=2)
  n <- data[1,1]
  peso_max <- data[2,1]
  names(valores) <- 1:n
  names(pesos) <- 1:n
  ret <- list(valores = valores, pesos= pesos, n=n, peso_max=peso_max)
  return(ret)
}

## Funcion de evaluacion del problema de la mochila con penalizacion 
##
##   - valores: vector de valores de los objetos
##   - pesos: vector dew pesos de los objetos
##   - peso_max: peso maximo para los objetos seleccionado
##   - rho: valor del parametro rho
##   - id_seleccionado: vector de los indices de los objetos seleccionados
##                      en la solucion
##
##  retorno: valor de la funcion de evaluacion de la solucion id_seleccionado
feval <- function(valores, pesos, peso_max, rho, id_seleccionado) 
{
  peso_seleccionado = sum(pesos[id_seleccionado])
  if (peso_seleccionado > peso_max) {
    penalizacion = rho * (peso_seleccionado - peso_max)
  } else {
    penalizacion = 0
  }
  fx = sum(valores[id_seleccionado]) - penalizacion
  return(fx)
}

## 
##  instancias para pruebas
## ##############################
## instancia 1
instancia1 <- list()
instancia1$valores <- c(5000, 2500, 10, 2500, 2500, 200, 3000, 500, 100, 10)
instancia1$ pesos <- c(25, 20, 20, 12.5, 10, 10, 7.5, 4, 1, 1)
names(instancia1$valores) <- names(instancia1$pesos) <- 
                                  c("cofre", "armadura", "basura", "escudo", 
                                    "espada", "tridente", "casco", "arco", 
                                    "varita", "pocion")
instancia1$peso_max = 30
instancia1$n       <- length(instancia1$valores) 

## instancia 2
instancia2 <- list()
instancia2$valores <- c(301, 502, 1282, 426, 997, 901, 490, 220, 250, 40,
                        1843, 220, 100, 30, 21, 2000, 22, 10000, 2, 2500)

instancia2$pesos <- c(165, 320, 700, 180, 400, 180, 40, 19, 21, 4,
                       777, 12, 4, 1, 1, 800, 1, 418, 20, 20000)
names(instancia2$valores) <- names(instancia2$pesos) <- 
                                  c("A", "B", "C", "D", "E", "F", "G", "H",
                                    "I", "J", "K", "L", "M", "N", "O", "P",
                                    "Q", "R", "S", "T")
instancia2$peso_max <- 2000
instancia2$n       <- length(instancia2$valores) 


## instancia 3
instancia3 <- list()
instancia3$valores<- c(50, 21000, 30000, 320000, 770000, 20000, 1000, 1000, 12000, 
                       700000, 3990, 165000, 1000, 240000, 4000, 40000, 180000, 
                       10, 2500000, 19000, 1000, 900, 1000, 50, 70000)

instancia3$pesos <- c(0.08, 2.5, 15, 5, 18, 0.02, 0.2, 0.2, 2.2, 12.8, 0.4, 3, 0.3, 8, 1,
           4.9, 4.2, 0.01, 25, 2.2, 0.6, 2, 2, 0.01, 10)
names(instancia3$valores) <- names(instancia3$pesos) <- 
                                  c("gema", "dinero", "cofre", "arco", "armadura",
                                    "ruby", "pocion_verde", "pocion_gris", "regalo", 
                                    "espada_pesada", "roca", "espada_liviana",
                                    "pala", "tridente", "botas", "casco", "hacha",
                                    "manzana", "tesoro", "lampara", "baculo",
                                    "pocion_lila", "caña", "moneda", "tronco")

instancia3$peso_max <- 15
instancia3$n       <- length(instancia3$valores) 
