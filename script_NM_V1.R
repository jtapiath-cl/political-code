# Revision de documentos de valores, principios y objetivos del Frente Amplio
## Todos los documentos obtenidos aqui son de libre disposicion.

## Listado de movimientos del Frente Amplio Chileno y sus documentos
### Partido Democrata Cristiano
### Estatutos: https://sites.google.com/a/pdc.cl/pdc/transparencia/marco-normativo/estatuto-pdc
### Partido Radical Social Democrata
### Estatutos: http://www.partidoradical.cl/wp-content/uploads/2016/06/estatuto-pr-2010.pdf
### Partido Por la Democracia
### Estatutos: http://www.ppd.cl/wp-content/uploads/2014/08/VER-DOCUMENTO.pdf
### Partido Comunista
### Estatutos: http://educacion.pcchile.cl/sn/wp-content/uploads/2006/06/cuadernos_estatutos.pdf
### Partido Socialista
### Estatutos: http://americo.usal.es/oir/opal/Documentos/Chile/Partido%20Socialista%20de%20Chile/Estatuto2000.pdf

## 1.0: Librerias y limpieza de entorno
library(pdftools)
library(rvest)
library(tm)
library(wordcloud)
rm(list=(ls()))

## 2.0: Obtener documentos desde internet
download.file("http://educacion.pcchile.cl/sn/wp-content/uploads/2006/06/cuadernos_estatutos.pdf", "pc.pdf", mode="wb")
download.file("http://americo.usal.es/oir/opal/Documentos/Chile/Partido%20Socialista%20de%20Chile/Estatuto2000.pdf", "ps.pdf", mode="wb")
download.file("http://www.partidoradical.cl/wp-content/uploads/2016/06/estatuto-pr-2010.pdf", "pr.pdf", mode="wb")
download.file("http://www.ppd.cl/wp-content/uploads/2014/08/VER-DOCUMENTO.pdf", "pd.pdf", mode="wb")

## 2.1: Convertir PDFs con pdftools y borrar archivos del disco
pc.ptxt <- pdf_text("pc.pdf")
ps.ptxt <- pdf_text("ps.pdf")
pr.ptxt <- pdf_text("pr.pdf")
pd.ptxt <- pdf_text("pd.pdf")

## 2.2: Convertir cadenas de PDF en una sola cadena de texto
pc.txt <- paste(pc.ptxt, sep="", collapse="")
ps.txt <- paste(ps.ptxt, sep="", collapse="")
pr.txt <- paste(pr.ptxt, sep="", collapse="")
pd.txt <- paste(pd.ptxt, sep="", collapse="")

## 3.0: Obtener textos desde internet (no PDFs)
dc.htxt <- read_html("https://sites.google.com/a/pdc.cl/pdc/transparencia/marco-normativo/estatuto-pdc")

## 3.1: Obtener texto relevante
dc.txt <- dc.htxt %>% html_node(".sites-layout-vbox") %>% html_text()

## 4.0: Limpiar los documentos de caracteres irregulares
### 4.0.1: Limpiar entorno
file.remove(c("pc.pdf", "ps.pdf", "pr.pdf", "pd.pdf"))
rm(pc.ptxt, ps.ptxt, pr.ptxt, pd.ptxt, dc.htxt)

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
dc.txt <- limpiarTexto(dc.txt)
ps.txt <- limpiarTexto(ps.txt)
pr.txt <- limpiarTexto(pr.txt)
pd.txt <- limpiarTexto(pd.txt)
pc.txt <- limpiarTexto(pc.txt)

## 5.0: Crear Corpus para todos los textos
corpus.todos <- VCorpus(VectorSource(c(pc.txt, ps.txt, pd.txt, pr.txt, dc.txt)))

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
colnames(tdm.final) <- c("Comunista", "Socialista", "PPD", "Radicales", "DC")
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
