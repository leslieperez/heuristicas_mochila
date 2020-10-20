source("mochila.R")
source("heuristicas.R")


# Leamos una instancia mas grande
instancia <- leer_instancia("instancias/kp-500.txt")

#Visualizar la instancia
#print(instancia)

# generemos una solucion aleatoria
solucion <- solucion_random(instancia)

# Apliquemos busqueda local
solucion <- busqueda_local_bitflip(solucion, instancia)