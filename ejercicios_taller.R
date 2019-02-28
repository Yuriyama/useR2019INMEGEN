#Taller Análisis de Redes

library(igraph)

#Leamos la red usando read.graph
g <- read.graph(file = "les_miserables.gml", format = "gml")

#Examinemos nuestra red
g

#Visualización
plot(g)

#guardar
dir.create("data")

saveRDS(object = g, 
        file = "data/red_mis.RDS")

saveRDS(object = g, 
        file = "data/red_mis.RDS")

#volver a leer
g.rds <- readRDS(file = "data/red_mis.RDS")
g.rds




#escribir
write.graph(graph = g, 
            file = "red_mis.graphml", 
            format = "graphml"
)
#volver a leer
g.graphml <- read.graph(file = "red_mis.graphml", 
                        format = "graphml")
g.graphml

#################################################################################
movies   <- read.csv("movies.csv")
head(movies)

g.movies <- graph_from_data_frame(d = movies, directed = FALSE)
g.movies

mis_componentes <- components(g)
mis_componentes #1.no? 2.tamaño de los componentes 3.núm de componentes

#mis componentes es una lista
mis_componentes$no

#redes reales gralmt necesita pocos pasos para ir de un nodo a otro

mis_caminos <- average.path.length(g)
mis_caminos

mi_clustering <- transitivity(g, type = "global")
mi_clustering

mi_diametro <- diameter(g)
mi_diametro

mi_radio    <- radius(g)
mi_radio

#Analizar propiedades de los nodos
#Accedemos al diccionario de nodos V
V(g) #da un vector de nodos

V(g)$label #vector de caracteres con el nombre de los nodos

#Extraigo data frame de nodos
mi_df_nodos <- get.data.frame(x = g, 
                              what = "vertices") #what es si de vertices o aristas
mi_df_nodos

#Podemos asignar nuevas propiedades a los nodos
#hagamos una nueva propiedad, name, que sea una copia de label
V(g)$name <- V(g)$label

mi_df_nodos <- get.data.frame(x = g, 
                              what = "vertices")
mi_df_nodos

#Cálculo de parámetros de centralidad
#Grado
V(g)$grado <- degree(g)
#Intermediación o Betweenness
V(g)$intermediacion <- betweenness(g)

#Podemos tener también el coeficiente de agrupamiento local
#Si es grande, todos conectados con todos
V(g)$agrupamiento <- transitivity(g, type = "local", isolates = "zero")

mi_df_nodos <- get.data.frame(x = g, 
                              what = "vertices")
mi_df_nodos

#Distribución de grado MUY importante, el descriptor de la red más completo y fundamental
library(tidyverse)

#uso dplyr para sacar frecuencias de valores de grado
my_df <-
  mi_df_nodos %>% 
  group_by(grado) %>% 
  summarize(frecuencia = n()) %>% 
  mutate(acumulada = cumsum(frecuencia)) %>% 
  mutate(relativa = acumulada/max(acumulada)) %>% #residual relativa
  mutate(res.rel = 1 - relativa) 

my_df

#grafico las frecuencias contra el valor de grado
my_df %>%   
  ggplot(mapping = aes(x = grado, 
                       y = frecuencia)) +
  geom_point() #scatter plot
#tienes muchos nodos con pocos vecinos, ie, grado 1
#y tienes 1 con muchos vecinos
#distribución de cola larga

#a veces me gusta ver la 1-acumulada
my_df %>%   
  ggplot(mapping = aes(x = grado, 
                       y = res.rel)) +
  geom_line() 



#en escala log
#a veces me gusta verla en semilog()
my_df %>%   
  ggplot(mapping = aes(x = grado, 
                       y = res.rel)) +
  geom_line() + 
  scale_y_log10()
#si se le ajusta una recta, puede ser serie de potencias, sería libre de escala


#############################################################
#Accedemos a enlaces
E(g)

#podemos tener un data frame 
mi_df_links <- get.data.frame(g, what = "edges")
mi_df_links

#Definimos una propiedad de enlaces
#Intermediación de enlace.
E(g)$intermediacion <- edge.betweenness(g)

#Y obtenemos el data.frame
mi_df_links <- get.data.frame(g, what = "edges")
mi_df_links %>% head()
#máximo sacarlo: max(mi_df_links)?

##############################################################
#Clustering -> Comunidades
#ejemplo para detectar comunidades, se usa info de enlaces y vértices
#algoritmo de louvin
comm.louvain <- cluster_louvain(g)
comm.louvain #devuelve objeto de comunidades

#podemos agregar la comunidad a la que pertenecen a cada nodo 
V(g)$comm.louvain <- membership(comm.louvain)


#Tenemos varios algoritmos implementados
comm.infomap <- cluster_infomap(g)
comm.walktrp <- cluster_walktrap(g)
comm.fstgred <- cluster_fast_greedy(g)

V(g)$comm.infomap <- membership(comm.infomap)
V(g)$comm.walktrp <- membership(comm.walktrp)
V(g)$comm.fstgred <- membership(comm.fstgred)

#Extraigamos una vez más la tabla de nodos
mi_df_nodos <- get.data.frame(x = g, 
                              what = "vertices")
mi_df_nodos %>% head()


###################################################################
#Hagamos Plots
plot(g, 
     vertex.label.cex = 0.75, #3/4 tamaño letra
     vertex.size = degree(g), #nodos proporcionales al  grado
     layout = layout_nicely #decide mejor posición, nunca es igual
)
#Genera una visualización con el tamaño de los nodos proporcional al grado, 
#escogiendo heurísticamente el “mejor acomodo”, y con el tamaño de las etiquetas más chico

plot(g, 
     vertex.label.cex = 0.75, 
     edge.curved = TRUE, #enlaces curveados
     layout = layout_in_circle
)

#También podemos plotear objetos de comunidades
plot(comm.walktrp, #objeto comunidad
     g,  #red
     vertex.label.cex = 0.75, #letra
     layout = layout_nicely #layout
)
#sombra límite de la comunidad
#color nodo, perteneciente a una comunidad

plot(comm.louvain, #objeto comunidad
     g,  #red
     vertex.label.cex = 0.75, #letra
     layout = layout_nicely #layout
)

plot(comm.infomap, #objeto comunidad
     g,  #red
     vertex.label.cex = 0.75, #letra
     layout = layout_nicely #layout
)

##############################################################
#Más tidy
library(tidygraph)
library(ggraph)

#Convertimos nuestra red de igraph en una red de tidygraph
gt <- as_tbl_graph(g)
gt

#Ahora tenemos el verbo activate para acceder a los datos de nodos o de aristas, y trabajar con ellos
#Agreguemos los agrupamientos de otro algoritmo de comunidades, label propagation
gt <- gt %>% 
  activate(what = "nodes") %>% #activate para seleccionar nodos
  mutate(comm.label_prop = group_label_prop()) #mutate para agregar una nueva columna
gt

#O filtremos por la red para quedarnos con nodos que tengan al menos cinco vecinos
gt_5 <- gt %>% 
  activate(what = "nodes") %>% 
  filter(grado > 5) #me quedo sólo con los que tenga grado mayor a 5
gt_5

#Ahora hagamos plots con ggraph
gt %>% 
  ggraph(layout = 'kk') + #nombre layout
  geom_edge_link(aes(), show.legend = FALSE) + #geom para los enlaces
  geom_node_point(aes(colour = as.factor(comm.louvain)), size = 7) + #color según comunidades
  guides(colour=guide_legend(title="Louvain community")) +
  theme_graph()

gt %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(), show.legend = FALSE) + 
  geom_node_point(aes(colour = as.factor(comm.infomap)), size = 7) + 
  guides(colour=guide_legend(title="Infomap community")) +
  theme_graph()

gt_5%>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(), show.legend = FALSE) + 
  geom_node_point(aes(colour = as.factor(comm.walktrp)), size = 7) + 
  guides(colour=guide_legend(title="Walktrap community")) +
  theme_graph()
