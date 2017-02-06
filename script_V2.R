# Revision de documentos de estatutos de la Nueva Mayoria
## Todos los documentos obtenidos aqui son de libre disposicion.

## Listado de partidos de la Nueva Mayoria
### Partido Democrata Cristiano
### Estatutos: 
### Partido Radical Social Democrata
### Estatutos: http://www.partidoradical.cl/declaracion-de-principios/
### Partido Por la Democracia
### Estatutos: http://www.ppd.cl/wp-content/uploads/2012/05/DDDD-PPD2.pdf
### Partido Comunista
### Estatutos: http://www.pcchile.cl/about-3/
### Partido Socialista
### Estatutos: http://web.pschile.cl/pschile/?upf=dl&id=3284

## 1.0: Librerias y limpieza de entorno
library(pdftools)
library(rvest)
library(tm)
library(wordcloud)
rm(list=(ls()))

## 2.0: Obtener documentos desde internet
download.file("http://web.pschile.cl/pschile/?upf=dl&id=3284", "ps.pdf", mode="wb")
download.file("http://www.ppd.cl/wp-content/uploads/2012/05/DDDD-PPD2.pdf", "pd.pdf", mode="wb")

## 2.1: Convertir PDFs con pdftools y borrar archivos del disco
ps.ptxt <- pdf_text("ps.pdf")
pd.ptxt <- pdf_text("pd.pdf")

## 2.2: Convertir cadenas de PDF en una sola cadena de texto
ps.txt <- paste(ps.ptxt, sep="", collapse="")
pd.txt <- paste(pd.ptxt, sep="", collapse="")

## 3.0: Obtener textos desde internet (no PDFs)
pr.htxt <- read_html("http://www.partidoradical.cl/declaracion-de-principios/")
pc.htxt <- read_html("http://www.pcchile.cl/about-3/")

## 3.1: Obtener texto relevante
pr.txt <- pr.htxt %>% html_node(".td-page-content") %>% html_text()
pc.txt <- pc.htxt %>% html_node(".entry-content") %>% html_text()

## 4.0: Limpiar los documentos de caracteres irregulares
### 4.0.1: Limpiar entorno
file.remove(c("ps.pdf", "pd.pdf"))
rm(pc.htxt, ps.ptxt, pr.htxt, pd.ptxt)

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
ps.txt <- limpiarTexto(ps.txt)
pr.txt <- limpiarTexto(pr.txt)
pd.txt <- limpiarTexto(pd.txt)
pc.txt <- limpiarTexto(pc.txt)

## 5.0: Crear Corpus para todos los textos
corpus.todos <- VCorpus(VectorSource(c(pc.txt, ps.txt, pd.txt, pr.txt)))

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
colnames(tdm.final) <- c("Comunista", "Socialista", "PPD", "Radicales")
colnames(tdm.final.dos) <- colnames(tdm.final)

## 6.0: Nubes de palabras
plotear <- function(matriz)
{
  partidos <- colnames(matriz)
  wcs <- paste(paste("wordcloud2",partidos,sep="-"),".png",sep="")
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
png("comparison-plot2-%d.png")
comparison.cloud(tdm.final, random.order=FALSE, title.size=0.8, max.words=500)
comparison.cloud(tdm.final.dos, random.order=FALSE, title.size=0.8, max.words=500)
dev.off()

### 6.2: Nube común
png("commonality-plot2.png")
commonality.cloud(tdm.final, random.order=FALSE, max.words=500)
dev.off()