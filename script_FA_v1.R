# Revision de documentos de valores, principios y objetivos del Frente Amplio
## Todos los documentos obtenidos aqui son de libre disposicion.

## Listado de movimientos del Frente Amplio Chileno y sus documentos
## Revolucion Democratica
### Declaracion de principios: http://revoluciondemocratica.cl/wp-content/uploads/2016/05/Nuestra-Declaracion-de-Principios.pdf
## Partido Humanista
### Declaracion de principios: http://www.partidohumanista.cl/conoce-al-partido-humanista/declaracion-de-principios/
## Partido Igualdad
### Estatutos del partido: http://partidoigualdad.cl/wp-content/uploads/2016/09/ESTATUTOS-DEL-PARTIDO-IGUALDAD-Fundacionales.pdf
## Izquierda Libertaria
### "Nuestra politica": http://izquierdalibertaria.cl/nuestra-politica/
## Partido Liberal
### Manifiesto: http://www.losliberales.cl/manifiesto/
## Partido Poder Ciudadano
### Manifiesto: http://www.partidopoder.cl/manifiesto-poder-ciudadano.pdf
## Movimiento Autonomista
### Organica del partido: http://www.construyendoalternativa.cl/web/docs/Orgánica%20Movimiento%20Autonomista.pdf
## Izquierda Autonoma
### Quienes somos: http://www.izquierdaautonoma.cl/1725-2/
## El Partido Ecologista no tiene sitio web activo en el momento del scraping (2017-01-27)

## 1.0: Librerias y limpieza de entorno
library(pdftools)
library(rvest)
library(tm)
library(wordcloud)
rm(list=(ls()))

## 2.0: Obtener documentos desde internet
download.file("http://www.partidopoder.cl/manifiesto-poder-ciudadano.pdf", "ciudadano.pdf", mode="wb")
download.file("http://www.construyendoalternativa.cl/web/docs/Orgánica%20Movimiento%20Autonomista.pdf", "autonomistas.pdf", mode="wb")
download.file("http://revoluciondemocratica.cl/wp-content/uploads/2016/05/Nuestra-Declaracion-de-Principios.pdf", "rd.pdf", mode="wb")
download.file("http://partidoigualdad.cl/wp-content/uploads/2016/09/ESTATUTOS-DEL-PARTIDO-IGUALDAD-Fundacionales.pdf", "partidoigualdad.pdf", mode="wb")

## 2.1: Convertir PDFs con pdftools y borrar archivos del disco
ciudadano.ptxt <- pdf_text("ciudadano.pdf")
igualdad.ptxt <- pdf_text("partidoigualdad.pdf")
autonomista.ptxt <- pdf_text("autonomistas.pdf")
rd.ptxt <- pdf_text("rd.pdf")
file.remove(c("ciudadano.pdf","autonomistas.pdf","partidoigualdad.pdf","rd.pdf"))

## 2.2: Convertir cadenas de PDF en una sola cadena de texto
autonomista.texto <- paste(autonomista.ptxt, sep="", collapse="")
ciudadano.texto <- paste(ciudadano.ptxt, sep="", collapse="")
igualdad.texto <- paste(igualdad.ptxt, sep="", collapse="")
rd.texto <- paste(rd.ptxt, sep="", collapse="")
rm(autonomista.ptxt, ciudadano.ptxt, igualdad.ptxt, rd.ptxt)

## 3.0: Obtener textos desde internet (no PDFs)
izqlibertaria <- read_html("http://izquierdalibertaria.cl/nuestra-politica/")
liberales <- read_html("http://www.losliberales.cl/manifiesto/")
izqautonoma <- read_html("http://izquierdaautonoma.cl/1725-2/")
humanistas <- read_html("http://www.partidohumanista.cl/conoce-al-partido-humanista/declaracion-de-principios/")

## 3.1: Obtener texto relevante y eliminar listados HTML
izqlibertaria.texto <- izqlibertaria %>% html_node("#post-18") %>% html_text()
izqautonoma.texto <- izqautonoma %>% html_node(".et_section_regular") %>% html_text()
liberales.texto <- liberales %>% html_node(".main-content .row") %>% html_text()
humanistas.texto <- humanistas %>% html_node(".vc_column_container") %>% html_text()
rm(izqlibertaria, izqautonoma, liberales, humanistas)

## 4.0: Limpiar los documentos de caracteres irregulares
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
autonomista.texto <- limpiarTexto(autonomista.texto)
ciudadano.texto <- limpiarTexto(ciudadano.texto)
humanistas.texto <- limpiarTexto(humanistas.texto)
igualdad.texto <- limpiarTexto(igualdad.texto)
izqautonoma.texto <- limpiarTexto(izqautonoma.texto)
izqlibertaria.texto <- limpiarTexto(izqlibertaria.texto)
liberales.texto <- limpiarTexto(liberales.texto)
rd.texto <- limpiarTexto(rd.texto)

### 4.3: Limpiar espacio de trabajo, dejado solo los textos
rm(limpiarTexto)

## 5.0: Crear Corpus para todos los textos
corpus.todos <- VCorpus(VectorSource(c(autonomista.texto, ciudadano.texto, humanistas.texto, igualdad.texto, izqautonoma.texto, izqlibertaria.texto, liberales.texto, rd.texto)))

### 5.1: Transformaciones apropiadas
corpus.todos <- tm_map(corpus.todos, stripWhitespace)
corpus.todos <- tm_map(corpus.todos, content_transformer(tolower))
corpus.todos <- tm_map(corpus.todos, removeWords, stopwords("spanish"))

### 5.2: Crear Matriz Termino-Documento
# dtm.todos <- DocumentTermMatrix(corpus.todos)
# dtm.todos.dos <- removeSparseTerms(dtm.todos, 0.4)
tdm.todos <- TermDocumentMatrix(corpus.todos)
tdm.todos.dos <- removeSparseTerms(tdm.todos, 0.4)
tdm.final <- as.matrix(tdm.todos)
tdm.final.dos <- as.matrix(tdm.todos.dos)

### 5.3: Nombres de columna
colnames(tdm.final) <- c("Autonomistas", "Poder Ciudadano", "Humanista", "Igualdad", "Izq Autonoma", "Izq Libertaria", "Liberales", "RD")
colnames(tdm.final.dos) <- c("Autonomistas", "Poder Ciudadano", "Humanista", "Igualdad", "Izq Autonoma", "Izq Libertaria", "Liberales", "RD")

## 6.0: Nubes de palabras
# wordcloud(tdm.final, random.order=FALSE, title.size=0.8, max.words=200)

### 6.1: Nube de comparacion
png("comparison-plot-%d.png")
comparison.cloud(tdm.final, random.order=FALSE, title.size=0.8, max.words=500)
comparison.cloud(tdm.final.dos, random.order=FALSE, title.size=0.8, max.words=500)
dev.off()

### 6.2: Nube común
png("commonality-plot.png")
commonality.cloud(tdm.final, random.order=FALSE, max.words=500)
dev.off()
