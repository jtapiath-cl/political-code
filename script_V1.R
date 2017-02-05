# Revision de documentos de principios de Chile Vamos
## Todos los documentos obtenidos aqui son de libre disposicion.

## Listado de partidos de Chile Vamos
### Renovacion Nacional
### Principios: http://www.rn.cl/principios/
### Evopoli
### Principios : http://www.evopoli.cl/principios/
### UDI
### Principios y Doctrina: http://www.udi.cl/somos-udi/
### PRI
### Nuevo Centro Social Cristiano: http://pricentro.cl/NCSC.php
### Sitio Web ChileVamos: http://www.chilevamos.cl

## 1.0: Librerias y limpieza de entorno
library(pdftools)
library(rvest)
library(tm)
library(wordcloud)
rm(list=(ls()))

## 2.0: Obtener documentos desde internet
### No hay archivos PDF en este batch de documentos

## 3.0: Obtener textos desde internet (no PDFs)
rn.htxt <- read_html("http://www.rn.cl/principios/")
ev.htxt <- read_html("http://www.evopoli.cl/principios/")
ud.htxt <- read_html("http://www.udi.cl/somos-udi/")
pr.htxt <- read_html("http://pricentro.cl/NCSC.php")


## 3.1: Obtener texto relevante
rn.txt <- rn.htxt %>% html_node(".column_container , .true") %>% html_text()
ev.txt <- ev.htxt %>% html_node(".nonhundred-percent-fullwidth , .img-responsive") %>% html_text()
ud.txt <- ud.htxt %>% html_node("#accordion2") %>% html_text()
pr.txt <- pr.htxt %>% html_node("img") %>% html_text()

## 3.1.1: Obtener textos de Evopoli y de PRI
### Como ambos entornos tienen imagenes en vez de texto parseable, las imagenes
### fueron transcritas a documentos TXT. Estos seran parseados al entorno
### de trabajo
### Los textos fueron transcritos y revisados al momento del parseo (2017-02-05)
ev.ftxt <- paste(getwd(),"/evopoli.txt", sep="", collapse="")
pr.ftxt <- paste(getwd(),"/pri.txt", sep="", collapse="")
ev.txt <- readChar(ev.ftxt, file.info(ev.ftxt)$size)
pr.txt <- readChar(pr.ftxt, file.info(pr.ftxt)$size)

## 4.0: Limpiar los documentos de caracteres irregulares
### 4.0.1: Limpiar entorno
rm(rn.htxt, ev.ftxt, ud.htxt, pr.ftxt)
### 4.0.2

### 4.1: Crea funcion de limpieza
limpiarTexto <- function(x)
{
  x <- chartr('áéíóú', 'aeiou', x)
  x <- gsub("\r?\n|\r", " ", x)
  x <- gsub("[ |\t]{2,}", " ", x)
  x <- gsub("^ ", "", x)
  x <- gsub(" $", "", x)
  x <- gsub("<", "", x)
  x <- gsub(">", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:digit:]]", " ", x)
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", " ", x, perl=TRUE)
}

### 4.2: Aplica funcion de limpieza a textos
rn.txt <- limpiarTexto(rn.txt)
ev.txt <- limpiarTexto(ev.txt)
ud.txt <- limpiarTexto(ud.txt)
pr.txt <- limpiarTexto(pr.txt)

## 5.0: Crear Corpus para todos los textos
corpus.todos <- VCorpus(VectorSource(c(rn.txt, ev.txt, ud.txt, pr.txt)))

### 5.1: Transformaciones apropiadas
corpus.todos <- tm_map(corpus.todos, stripWhitespace)
corpus.todos <- tm_map(corpus.todos, content_transformer(tolower))
corpus.todos <- tm_map(corpus.todos, removeWords, stopwords("spanish"))

### 5.2: Crear Matriz Termino-Documento
tdm.todos <- TermDocumentMatrix(corpus.todos)
tdm.todos.dos <- removeSparseTerms(tdm.todos, 0.4)
tdm.final <- as.matrix(tdm.todos)
tdm.final.dos <- as.matrix(tdm.todos.dos)

### 5.3: Nombres de columna
colnames(tdm.final) <- c("RN", "Evopoli", "UDI", "PRI")
colnames(tdm.final.dos) <- colnames(tdm.final)

## 6.0: Nubes de palabras
plotear <- function(matriz)
{
  partidos <- colnames(matriz)
  wcs <- paste(paste("wordcloud",partidos,sep="-"),".png",sep="")
  ncol.mat <- ncol(matriz)
  for(i in 1:ncol.mat)
  {
    png(wcs[[i]])
    wordcloud(words=rownames(tdm.final), freq=tdm.final[,i], random.order=TRUE, min.freq=3, max.words=200)
    dev.off()
  }
}

plotear(tdm.final)


### 6.1: Nube de comparacion
png("comparison-plot-%d.png")
comparison.cloud(tdm.final, random.order=FALSE, title.size=0.8, max.words=500)
comparison.cloud(tdm.final.dos, random.order=FALSE, title.size=0.8, max.words=500)
dev.off()

### 6.2: Nube común
png("commonality-plot.png")
commonality.cloud(tdm.final, random.order=FALSE, max.words=500)
dev.off()