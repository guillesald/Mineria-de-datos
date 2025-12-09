# Limpiamos la memoria
rm( list=ls() )
gc()

# Librerías necesarias
library(dplyr)
library(ggplot2)
library(ggthemes)
library(arules)
library(arulesSequences)
library(treemapify)
library(stringr)

# Ruta donde esta guardado  el archivo
ruta_archivo <- "e-shop clothing 2008.csv"

# Leemos el archivo
archivo <- read.csv(ruta_archivo, header = TRUE, sep = ';') 


#A)

# Visualizamos
head(archivo)

# Exploramos los  datos
str(archivo)

# Estadísticos
summary(archivo)

# Pasar a factor las variables (Excepto YEAR, PRICE AND SESSION.ID)
archivo$month <- as.factor(archivo$month)
archivo$day <- as.factor(archivo$day)
archivo$page.1..main.category. <- as.factor(archivo$page.1..main.category.)
archivo$page.2..clothing.model. <- as.factor(archivo$page.2..clothing.model.)
archivo$colour <- as.factor(archivo$colour)
archivo$location <- as.factor(archivo$location)
archivo$model.photography <- as.factor(archivo$model.photography)
archivo$price.2 <- as.factor(archivo$price.2)
archivo$page <- as.factor(archivo$page)

# Verficamos los cambios realizados
str(archivo)

# Verificamos si hay datos nulos en nuestro df
any(is.na(archivo)) # No posee

#-------------------------------------------------------------------------------

#B)
# Visualización de los datos


#Clicks por sesión

# Contamos los clicks por sesión
clicks_por_sesion <- archivo %>%
  count(session.ID)

# Graficamos un histograma de cantidad de clicks por sesión
ggplot(clicks_por_sesion, aes(x = n)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribución de clicks por sesión",
    x = "Cantidad de clicks por sesión",
    y = "Número de sesiones"
  ) +
  theme_minimal()


clicks_por_sesion <- clicks_por_sesion %>%
  mutate(rango_clicks = cut(n,
                            breaks = c(0, 2, 5, 10, 20, 50, Inf),
                            labels = c("1-2", "3-5", "6-10", "11-20", "21-50", "50+")))

# Graficamos un histograma de cantidad sesiones por rango de clicks
ggplot(clicks_por_sesion, aes(x = rango_clicks)) +
  geom_bar(fill = "mediumpurple3") +
  labs(
    title = "Cantidad de sesiones por rango de clicks",
    x = "Rango de clicks",
    y = "Número de sesiones"
  ) +
  theme_minimal()

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))# Sesiones por país

  #---------------------------------------------
#Sesiones por país
  
  
# Top 10 sin incluir Polonia y dominios
# Realizamos un vector de nombres de países + dominios (que vamos a filtrar)
nombres_paises <- c(
  "Australia", "Austria", "Belgium", "British Virgin Islands", "Cayman Islands", 
  "Christmas Island", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
  "Estonia", "unidentified", "Faroe Islands", "Finland", "France", 
  "Germany", "Greece", "Hungary", "Iceland", "India", 
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
  "Mexico", "Netherlands", "Norway", "Poland", "Portugal", 
  "Romania", "Russia", "San Marino", "Slovakia", "Slovenia", 
  "Spain", "Sweden", "Switzerland", "Ukraine", "United Arab Emirates", 
  "United Kingdom", "USA", "biz (*.biz)", "com (*.com)", "int (*.int)", 
  "net (*.net)", "org (*.org)"
)

# Reemplazamos el código por el nombre
archivo$country <- factor(archivo$country, levels = 1:47, labels = nombres_paises)

# Filtramos para excluir dominios y Polonia
pais_no_validos <- c("Poland", "biz (*.biz)", "com (*.com)", "int (*.int)", "net (*.net)", "org (*.org)")

sesiones_por_pais <- archivo %>%
  filter(!country %in% pais_no_validos) %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session.ID)) %>%
  arrange(desc(sesiones)) %>%
  slice_head(n = 10)

# Graficamos 
ggplot(sesiones_por_pais, aes(x = reorder(country, -sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "mediumpurple3") +
  labs(
    title = "Top 10 países por número de sesiones (excluyendo Polonia y dominios)",
    x = "País",
    y = "Cantidad de sesiones"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Productos vistos por sesion
productos_por_sesion <- archivo %>%
  group_by(session.ID) %>%
  summarise(productos_vistos = n_distinct(page.2..clothing.model.))

# Boxplot de productos vistos por sesión vertical
ggplot(productos_por_sesion, aes(x = "", y = productos_vistos)) +
  geom_boxplot(fill = "#E12CB0", color = "black") +
  labs(title = "Boxplot de productos vistos por sesión", x = "", y = "N° de productos vistos") +
  theme_stata()+scale_color_stata()


# Calculamos el percencil 80 (valor por debajo del cual se encuentra el 80% de los datos) 
limite_superior <- quantile(productos_por_sesion$productos_vistos, 0.80)

# Filtramos las sesiones que no exceden ese valor
productos_filtrados <- productos_por_sesion %>%
  filter(productos_vistos <= limite_superior)

# Realizamos un boxplot sin los outliers extremos
ggplot(productos_filtrados, aes(y = productos_vistos)) +
  geom_boxplot(fill = "darkorange") +
  labs(
    title = "Boxplot filtrado de productos vistos por sesión",
    y = "Productos únicos vistos"
  ) +
  theme_minimal()


# Productos vistos por sesión por rangos
productos_por_sesion <- productos_por_sesion %>%
  mutate(rango_productos = case_when(
    productos_vistos <= 2 ~ "1-2",
    productos_vistos <= 5 ~ "3-5",
    productos_vistos <= 10 ~ "6-10",
    productos_vistos <= 20 ~ "11-20",
    productos_vistos <= 50 ~ "21-50",
    TRUE ~ "51+"
  ))

resumen_rangos <- productos_por_sesion %>%
  group_by(rango_productos) %>%
  summarise(cantidad_sesiones = n()) %>%
  mutate(rango_productos = factor(rango_productos, levels = c("1-2", "3-5", "6-10", "11-20", "21-50", "51+")))

# Graficamos
ggplot(resumen_rangos, aes(x = rango_productos, y = cantidad_sesiones)) +
  geom_bar(stat = "identity", fill = "mediumseagreen") +
  labs(
    title = "Sesiones agrupadas por cantidad de productos únicos vistos",
    x = "Rango de productos vistos",
    y = "Cantidad de sesiones"
  ) +
  theme_minimal()


archivo$page.1..main.category. <- factor(archivo$page.1..main.category.,
                                         levels = 1:4,
                                         labels = c("Trousers", "Skirts", "Blouses", "Sale"),
                                         ordered = TRUE)
#----------------------------------------------

# Categorías de productos más vistas por sesión  
categorias_mas_vistas <- archivo %>%
  group_by(session.ID) %>%
  summarise(categoría_producto = n_distinct(page.1..main.category.))

# Creamos un nuevo dataframe con las frecuencias de cada categoría
categorias_mas_vistas <- categorias_mas_vistas %>%
  group_by(categoría_producto) %>%
  summarise(frecuencia = n())

# Treemap
ggplot(categorias_mas_vistas, aes(area = frecuencia, fill = as.factor(categoría_producto))) +
  geom_treemap() + geom_treemap_text(aes(label = frecuencia), colour = "white") +
  labs(title = "Treemap de categorías de productos más vistas por sesión", fill = "Cantidad de categorias", size = "Frecuencia") +
  theme_stata()+scale_fill_stata()



# Sesiones por categoría
categorias_sesion <- archivo %>%
  group_by(session.ID, page.1..main.category.) %>%
  summarise(.groups = "drop") %>%
  count(page.1..main.category.)

# Gráfico
ggplot(categorias_sesion, aes(
  x = reorder(page.1..main.category., -n),
  y = n,
  fill = page.1..main.category.
)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = n),
    hjust = 1.1,
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_stata() +
  labs(
    title = "Sesiones que vieron cada categoría de producto",
    x = "Categoría",
    y = "Cantidad de sesiones"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, face = "bold")
  )

#-----------------------------------------------

#C)
# Evolución de la cantidad de clicks por sesión a través de los meses
# Agrupamos la cantidad de clicks por mes
clicks_por_mes <- archivo %>%
  group_by(month) %>%
  summarise(clicks = n())

clicks_por_mes$month <- factor(clicks_por_mes$month,
                               levels = 4:8,
                               labels = c("Abril", "Mayo", "Junio", "Julio", "Agosto"),
                               ordered = TRUE)

# Gráfico de línea
ggplot(clicks_por_mes, aes(x = month, y = clicks, group = 1)) +
  geom_line(color = "mediumpurple3", linewidth = 1.2) +
  geom_point(color = "mediumpurple3", size = 3) +
  geom_text(aes(label = clicks), vjust = -0.5, size = 4) +
  labs(
    title = "Evolución de la cantidad de clicks por sesión",
    x = "Mes",
    y = "Cantidad de clicks"
  ) +
  theme_minimal()



#-----------------------------------------------
#D)

#Número de transacciones e items
datos_agrupados <- archivo %>%
  select(session.ID, page.2..clothing.model.) %>%
  group_by(session.ID) %>%
  summarise(productos = list(page.2..clothing.model.))

#Número de transacciones
length(unique(datos_agrupados$session.ID))
#Número de items
n_distinct(archivo$page.2..clothing.model.)
#-----------------------------------------------
#E)

# Convertir la lista de productos en objeto "transactions"
transacciones <- as(datos_agrupados$productos, "transactions")

# Ejecutar apriori para itemsets frecuentes con soporte mínimo 2% y mínimo 2 ítems
itemsets_frecuentes <- apriori(
  transacciones,
  parameter = list(supp = 0.02, minlen = 2, target = "frequent itemsets")
)

# Mostrar resultados
inspect(sort(itemsets_frecuentes, by = "support", decreasing = TRUE))

#-----------------------------------------------

#F)
#Reglas de asociación para Polonia en categoria blusas

#Filtramos 
archivo_polonia_blusas <- archivo %>%
  filter(country == "Poland", page.1..main.category. == "Blouses")

#Agrupamos
datos_polonia_blusas <- archivo_polonia_blusas %>%
  select(session.ID, page.2..clothing.model.) %>%
  group_by(session.ID) %>%
  summarise(productos = list(page.2..clothing.model.))

#Generamos un objeto de tipo transactions
transacciones_polonia_blusas <- as(datos_polonia_blusas$productos, "transactions")

#Reglas de asociación
reglas <- apriori(transacciones_polonia_blusas,
                    parameter = list(support = 0.02,
                                     confidence = 0.2,
                                     target = "rules"),
                    control = list(verbose = FALSE))

#Ordebamos y mostramos las 10 primeras
inspect(sort(reglas, by = "support", decreasing = TRUE)[1:10])

#-----------------------------------------------
#G)
#Reglas de asociación para República Checa en categoria blusas

#Filtramos 
archivo_czech_blusas <- archivo %>%
  filter(country == "Czech Republic", page.1..main.category. == "Blouses")

#Agrupamos
datos_czech_blusas <- archivo_czech_blusas %>%
  select(session.ID, page.2..clothing.model.) %>%
  group_by(session.ID) %>%
  summarise(productos = list(page.2..clothing.model.))

#Generamos un objeto de tipo transactions
transacciones_czech_blusas <- as(datos_czech_blusas$productos, "transactions")

#Reglas de asociación
reglas_2 <- apriori(transacciones_czech_blusas,
                                            parameter = list(support = 0.04,
                                                             confidence = 0.25,
                                                             target = "rules"),
                                            control = list(verbose = FALSE))

#Ordebamos y mostramos las 10 primeras
inspect(sort(reglas_2, by = "support", decreasing = TRUE)[1:7])


#------------------------------------------------------------------
#I)
# Secuencias mas frecuentes
#Seleccionamos las columnas a utilizar
datos_secuencias <- archivo %>%
  select(session.ID, order, page.2..clothing.model.) %>%
  arrange(session.ID, order)  # Asegurar orden dentro de cada sesión

colnames(datos_secuencias) <- c("sequenceID", "eventID", "itemID")

# Crear transacciones solo con items
transacciones_secuencias <- as(datos_secuencias %>% transmute(items = itemID), "transactions")

# Asignar info de secuencia y evento
transactionInfo(transacciones_secuencias)$sequenceID <- datos_secuencias$sequenceID
transactionInfo(transacciones_secuencias)$eventID <- datos_secuencias$eventID

# Limpiar etiquetas
itemLabels(transacciones_secuencias) <- str_replace_all(itemLabels(transacciones_secuencias), "items=", "")

# Revisar primeras transacciones
inspect(transacciones_secuencias[1:10])

# Aplicar cSPADE con soporte 2%
secuencias_frecuentes <- cspade(transacciones_secuencias,
                                parameter = list(support = 0.02),
                                control = list(verbose = FALSE))

summary(secuencias_frecuentes)

# Filtrar secuencias de más de un ítem
secuencias_frecuentes_mayores <- subset(secuencias_frecuentes, size(secuencias_frecuentes) > 1)

# Ordenar y mostrar
secuencias_frecuentes_mayores <- sort(secuencias_frecuentes_mayores, by = "support", decreasing = TRUE)
inspect(secuencias_frecuentes_mayores)






