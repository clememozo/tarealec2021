# Esto verifica si todos los paquete necesarios estan instalados,
# de no ser asi, los instala 
# Si no se esta trabajando con Rstudio, reemplazar dirname(rstudioapi::getSourceEditorContext()$path) por getwd()
packages = c("readr", "data.table",
             "tibble", "moments", "rstudioapi","ggplot2")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
# Verfica si existe una carpeta "CSVs", de no ser asi, crea una.
dir.create(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "CSVs"))
# Se descargan los archivos de los suigientes urls
urlfile="https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv"
urlfile2="https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo.csv"
CasoTotalesAcumulativos<-read_csv(url(urlfile), col_types = cols())
FallecidosAcumulativos <-read_csv(url(urlfile2), col_types = cols())


x <-seq(from = 1, to = 17)
for (val in x) {
  # Index de la region a la que se extraeran los datos
  index_de_region = val
  # Data frameworks con los datos de la region seleccionada
  df1 = (setDT(as.data.frame(t(subset(CasoTotalesAcumulativos[index_de_region,], select = -c(1)))), keep.rownames = TRUE)[])
  colnames(df1)[1] <- "Fecha"
  colnames(df1)[2] <- "Casos.acumulados.a.la.fecha"
  df2 = (setDT(as.data.frame(t(subset(FallecidosAcumulativos[index_de_region,], select = -c(1)))), keep.rownames = TRUE)[])
  colnames(df2)[1] <- "Fecha"
  colnames(df2)[2] <- "Muertes.acumuladas.a.la.fecha"
  df1 = merge(df1, df2, by.y = "Fecha", by.x = "Fecha", all.x = TRUE, all.y = TRUE)
  #Calculo de mortalidad
  df1[is.na(df1)] <- 0
  df1 <- transform(df1, 
                   Mortalidad.del.virus = df1$Muertes.acumuladas.a.la.fecha/df1$Casos.acumulados.a.la.fecha)
  # Si existen n/a, se reemplazan con 0's
  df1[is.na(df1)] <- 0
  # Calculo de promedio por fecha de las columnas muertes acumuladas y casos acumulados
  a <- c()
  b <-c()
  for (i in 1:length(df1$Muertes.acumuladas.a.la.fecha)) {
    a = append(a, mean(df1$Muertes.acumuladas.a.la.fecha[c(1:i)]), i)
    b = append(b, mean(df1$Casos.acumulados.a.la.fecha[c(1:i)]), i)
  }
  # Si hay n/a, lo transformo a 0's
  df1[is.na(df1)] <- 0
  k <- c()
  j <- c()
  # Calculamos medidas de dispersion, desviacion esandar de muertes acumuladas a la fecha por
  # cada fecha y desviacion estandar de casos acumulados a la fecha por cada fecha
  for (i in 1:length(df1$Muertes.acumuladas.a.la.fecha)) {
    k = append(k, sd(df1$Muertes.acumuladas.a.la.fecha[c(1:i)]), i)
    j = append(j, sd(df1$Casos.acumulados.a.la.fecha[c(1:i)]), i)
  }

  # Calculamos medidas de forma, kurtosis y skewness por cada fecha
  s <- c()
  q <- c()
  r <- c()
  t <- c()
  for (i in 1:length(df1$Muertes.acumuladas.a.la.fecha)) {
    s = append(s, kurtosis(df1$Muertes.acumuladas.a.la.fecha[c(1:i)]), i)
    q = append(q, skewness(df1$Casos.acumulados.a.la.fecha[c(1:i)]), i)
    r = append(r, skewness(df1$Muertes.acumuladas.a.la.fecha[c(1:i)]), i)
    t = append(t, kurtosis(df1$Casos.acumulados.a.la.fecha[c(1:i)]), i)
  }
  # Agregamos todo lo anterior a "df1"
  df1 <- transform(df1, 
                   Promedio.casos.acumulados.a.la.fecha = b,
                   Promedio.muertes.acumuladas.a.la.fecha = a,
                   Mortalidad.del.virus = df1$Muertes.acumuladas.a.la.fecha/df1$Casos.acumulados.a.la.fecha,
                   SD.muertes.acumuladas.a.la.fecha = k,
                   SD.casos.acumulados.a.la.fecha = j,
                   skewness.muertes.acumuladas.a.la.fecha = r,
                   skewness.casos.acumulados.a.la.fecha = q,
                   kurtosis.muertes.acumuladas.a.la.fecha = s,
                   kurtosis.casos.acumulados.a.la.fecha = t
  )
  # Si hay n/a, lo transformo a 0's
  df1[is.na(df1)] <- 0
  # Guardo mi df1 en la carpeta CSVs con el nombre de la region y extension .csv
  carpeta = paste(dirname(rstudioapi::getSourceEditorContext()$path),"/CSVs/", FallecidosAcumulativos[index_de_region,]$Region,".csv", sep="")
  write.csv(df1, carpeta, row.names = FALSE)
}
# Creamos carpeta de Graficos.
dir.create(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Graficos"))
# USA
url_us = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
df_us<-read_csv(url(url_us), col_types = cols())
# Calculo de mortalidad
mortalidad = df_us$deaths/df_us$cases
foo <-c()
bar <-c()
# Calculo de promedio
for (i in 1:length(df_us$cases)) {
  foo = append(foo, mean(df_us$deaths[c(1:i)]), i)
  bar = append(bar, mean(df_us$cases[c(1:i)]), i)
}
df_us[is.na(df_us)] <- 0
# Calculo de desviacion estandar 
bao <- c()
sim <-c()
for (i in 1:length(df_us$cases)) {
  bao = append(bao, sd(df_us$deaths[c(1:i)]), i)
  sim = append(sim, sd(df_us$cases[c(1:i)]), i)
}
df_us[is.na(df_us)] <- 0
# Calculo de kurtosis y skewness
p <- c()
o <- c()
u <-c()
m <-c()
for (i in 1:length(df_us$cases)) {
  p = append(p, kurtosis(df_us$deaths[c(1:i)]), i)
  o = append(o, skewness(df_us$cases[c(1:i)]), i)
  u = append(u, skewness(df_us$deaths[c(1:i)]), i)
  m = append(m, kurtosis(df_us$cases[c(1:i)]), i)
}
# Se agrega todo a df_us
df_us <- transform(df_us, 
                   Promedio.casos.acumulados.a.la.fecha = bar,
                   Promedio.muertes.acumuladas.a.la.fecha = foo,
                   Mortalidad.del.virus = df_us$deaths/df_us$cases,
                   SD.muertes.acumuladas.a.la.fecha = bao,
                   SD.casos.acumulados.a.la.fecha = sim,
                   skewness.muertes.acumuladas.a.la.fecha = u,
                   skewness.casos.acumulados.a.la.fecha = o,
                   kurtosis.muertes.acumuladas.a.la.fecha = p,
                   kurtosis.casos.acumulados.a.la.fecha = m
                   
)
# Si hay n/a, se reemplazan por 0's
df_us[is.na(df_us)] <- 0
# Cambiamos y traducimos los nombre de las columnas 
colnames(df_us)[1] <- "Fecha"
colnames(df_us)[2] <- "Casos.acumulados.a.la.fecha"
colnames(df_us)[3] <- "Muertes.acumuladas.a.la.fecha"
write.csv(df_us, paste(dirname(rstudioapi::getSourceEditorContext()$path),"/CSVs/", "USA",".csv", sep=""), row.names = FALSE)

# Generamos un grafico de cada columna con respecto al tiempo 
data <- read_csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/CSVs/", "USA",".csv", sep=""), col_types = cols())
data_ggp <- data.frame(x = data$Fecha,
                       y = c(data$Casos.acumulados.a.la.fecha, 
                             data$Muertes.acumuladas.a.la.fecha, 
                             data$Promedio.casos.acumulados.a.la.fecha,
                             data$Promedio.muertes.acumuladas.a.la.fecha,
                             data$SD.muertes.acumuladas.a.la.fecha,
                             data$SD.casos.acumulados.a.la.fecha,
                             data$skewness.muertes.acumuladas.a.la.fecha,
                             data$skewness.casos.acumulados.a.la.fecha,
                             data$kurtosis.muertes.acumuladas.a.la.fecha,
                             data$kurtosis.casos.acumulados.a.la.fecha),
                       group = c(rep("Casos Acumulados a La fecha", nrow(data)),
                                 rep("Muertes acumuladas a la fecha", nrow(data)),
                                 rep("Promedio casos acumulados", nrow(data)),
                                 rep("Promedio muertes acumuladas", nrow(data)),
                                 rep("SD muertes acumuladas", nrow(data)),
                                 rep("SD casos acumulados", nrow(data)),
                                 rep("Skewness muertes acumuladas", nrow(data)),
                                 rep("Skewness casos acumulados", nrow(data)),
                                 rep("Kurtosis muertes acumuladas", nrow(data)),
                                 rep("Kurtosis casos acumulados", nrow(data))
                       ))
# Para que no se pierdan las escalas de los graficos, agrego 
# + facet_grid(group ~ ., scales="free"), lo que hace esto es que 
# se grafique mas de una linea pero en distintos planos, pero en el mismo archivo.
ggp <- ggplot(data_ggp, aes(x, y, col = group)) +             
  geom_line()+ facet_grid(group ~ ., scales="free")
# Se guarda el grafico en la carpeta "Graficos que se creo anteriormente "
ggsave(filename = "USA.jpg", path=paste(dirname(rstudioapi::getSourceEditorContext()$path),"/Graficos", sep=""),height = 11.7, width = 8.5)


# GRAFICOS
# Se ubica el directorio de la carpeta "Graficos"
path = paste(dirname(rstudioapi::getSourceEditorContext()$path),"/Graficos", sep="")
for (val in x){
  # Recupero el nombre de la region segun su index
  nombre_region = FallecidosAcumulativos[val,]$Region
  # Busco el csv de la region en la carpeta "CSVs" creada anteriormente
  path2 = paste(dirname(rstudioapi::getSourceEditorContext()$path),"/CSVs/", nombre_region,".csv", sep="")
  # data va a ser mi df con la que trabajare la region seleccionada 
  data <- read_csv(path2, col_types = cols())
  # uso ggplot2 para graficar todas la columnas respecto al tiempo (fecha)
  data_ggp <- data.frame(x = data$Fecha,
                         y = c(data$Casos.acumulados.a.la.fecha, 
                               data$Muertes.acumuladas.a.la.fecha, 
                               data$Promedio.casos.acumulados.a.la.fecha,
                               data$Promedio.muertes.acumuladas.a.la.fecha,
                               data$SD.muertes.acumuladas.a.la.fecha,
                               data$SD.casos.acumulados.a.la.fecha,
                               data$skewness.muertes.acumuladas.a.la.fecha,
                               data$skewness.casos.acumulados.a.la.fecha,
                               data$kurtosis.muertes.acumuladas.a.la.fecha,
                               data$kurtosis.casos.acumulados.a.la.fecha),
                         group = c(rep("Casos Acumulados a La fecha", nrow(data)),
                                   rep("Muertes acumuladas a la fecha", nrow(data)),
                                   rep("Promedio casos acumulados", nrow(data)),
                                   rep("Promedio muertes acumuladas", nrow(data)),
                                   rep("SD muertes acumuladas", nrow(data)),
                                   rep("SD casos acumulados", nrow(data)),
                                   rep("Skewness muertes acumuladas", nrow(data)),
                                   rep("Skewness casos acumulados", nrow(data)),
                                   rep("Kurtosis muertes acumuladas", nrow(data)),
                                   rep("Kurtosis casos acumulados", nrow(data))
                         ))
  # Para que no se pierdan las escalas de los graficos, agrego 
  # + facet_grid(group ~ ., scales="free"), lo que hace esto es que 
  # se grafique mas de una linea pero en distintos planos, pero en el mismo archivo
  ggp <- ggplot(data_ggp, aes(x, y, col = group)) +             # Create ggplot2 plot
    geom_line()+ facet_grid(group ~ ., scales="free")
  # Genero el nombre del archivo que quiero guardar
  name = paste(nombre_region, ".jpg")
  # Lo guardo en la carpeta Graficos generada anteriormente 
  ggsave(filename = name, path=path,height = 11.7, width = 8.5)
  # repito los anterior con todas la CSVs usando el loop for
}

url_re = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/TotalesPorRegion.csv"
Total<-read_csv(url(url_re), col_types = cols())

dir.create(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "BoxPlot"))
path3 = file.path(dirname(rstudioapi::getSourceEditorContext()$path))

x <-seq(from = 1, to = 17)
for (val in x){
  df = (Total[Total$Categoria=="Casos nuevos totales",])
  region = (df$Region[val:val])
  df = (t(df[df$Region==region,]))
  df = (setDT(as.data.frame(df), keep.rownames = TRUE)[])
  df = df[c(113:length(df$V1))]
  colnames(df)[1] <- "Fecha"
  colnames(df)[2] <- "Casos nuevos totales"
  df$`Casos nuevos totales`=as.numeric(as.character(df$`Casos nuevos totales`))
  jpeg(paste(path3, "/BoxPlot/", region, ".jpg", sep = ""), width = 1280, height = 1280)
  boxplot(df$`Casos nuevos totales`)
  dev.off()
}
