valores <- c(5000,2500,10,2500,2500,200,3000,500,100,10)
pesos <- c(25,20,20,12.5,10,10,7.5,4,1,1)
names(valores) <- names(pesos) <- c("cofre", "armadura", "basura", "escudo", "espada", "tridente", "casco", "arco", "varita", "pocion")
peso_max = 30
n       <- length(valores) # Numero de variables