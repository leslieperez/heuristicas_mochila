valores<- c(50, 21000, 30000, 320000, 770000, 20000, 1000, 1000, 12000, 700000,
           3990, 165000, 1000, 240000, 4000, 40000, 180000, 10, 2500000, 19000,
           1000, 900, 1000, 50, 70000)

pesos <- c(0.08, 2.5, 15, 5, 18, 0.02, 0.2, 0.2, 2.2, 12.8, 0.4, 3, 0.3, 8, 1,
        4.9, 4.2, 0.01, 25, 2.2, 0.6, 2, 2, 0.01, 10)
names(valores) <- names(pesos) <- c("gema", "dinero", "cofre", "arco", "armadura",
                                    "ruby", "pocion_verde", "pocion_gris", "regalo", 
                                    "espada_pesada", "roca", "espada_liviana",
                                    "pala", "tridente", "botas", "casco", "hacha",
                                    "manzana", "tesoro", "lampara", "baculo",
                                    "pocion_lila", "caña", "moneda", "tronco")

peso_max <- 15
n       <- length(valores) # Numero de variables