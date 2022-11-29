## Cargar librerías----------

library(dplyr) 
library(rvest) # Web scrapping
library(stringr) 
library(purrr) 
library(lubridate)
library(readr)
library(rtweet)
library(tidyverse)
library(lubridate)
library(igraph)
library(ggraph)
library(tm)
library(SnowballC)
library(wordcloud2)
library(dplyr)
library(textreadr)

## Configurar busqueda-------

news_pag = "https://news.google.com/"
news_pag

parametro_busqueda = "search?q="
parametro_busqueda

busqueda = "CONAFIPS"
busqueda_no_espacios = gsub(" ","%20", busqueda)
busqueda_no_espacios

parametro_final = "&hl=es-419&gl=US&ceid=US:es-419"
parametro_final

html_dir = paste0(news_pag,parametro_busqueda,busqueda_no_espacios,parametro_final)
html_dir

google_news = read_html(html_dir)
class(google_news)

noticias = google_news %>% 
  html_nodes(css = ".GndZbb") %>% 
  html_children()
noticias

noticia = noticias[[1]]
noticia

noticia %>% html_nodes("*") %>% html_text()

nodos = noticia %>% html_nodes("*")
nodos[c(7,8,15)]

titular = noticia %>% html_node("h3") %>% html_text()
titular

diario = noticia %>% html_node("a.wEwyrc.AVN2gc.uQIVzc.Sksgp") %>% html_text()
diario

noticia %>% html_nodes("*") %>% html_attr("datetime")

nodos[16]

fecha = noticia %>% html_node("time") %>% html_attr("datetime")
fecha


noticia %>% html_nodes("*") %>% html_attr("href")

nodos[c(1,6,8)]

noticia %>% html_node("a") %>% html_attr("href")

nodos[1]

link_enmascarado = noticia %>% html_node("a") %>% html_attr("href")
link_enmascarado

link_enmascarado = paste0(news_pag,substring(link_enmascarado,3))  
link_enmascarado

link_leido = read_html(link_enmascarado)
link_leido %>% html_nodes("a") %>% html_attr("href")

nodos = link_leido %>% html_nodes("a") 
nodos[length(nodos)]


link = link_leido %>% html_nodes("a") %>% tail(1) %>% html_attr("href")
link


library('xml2')

noticia_leida = read_html(link)

noticia_leida %>% html_nodes(".field-name-body") %>% html_text() %>% cat()

noticia_leida %>% html_nodes("div div article div div div ") %>% html_text()

noticia_leida %>% html_nodes("div div article div div div") %>% html_children() 

texto = noticia_leida %>% html_nodes("div div article div div div") %>% html_nodes("p") %>% html_text()
texto = paste0(texto, collapse = " ")
texto

obtieneNoticiasBusqueda = function(busqueda){
  news_pag = "https://news.google.com/"
  parametro_busqueda = "search?q="
  busqueda_no_espacios = gsub(" ","%20", busqueda)
  parametro_final = "&hl=es-419&gl=US&ceid=US:es-419"
  html_dir = paste0(news_pag,parametro_busqueda,busqueda_no_espacios,parametro_final)
  google_news = read_html(html_dir)
  noticias = google_news %>% 
    html_nodes(css = ".GndZbb") %>% 
    html_children()
  noticiasDF = map(noticias,obtieneNoticiasData)
  noticiasDF = bind_rows(noticiasDF)
  noticiasDF = noticiasDF[!is.na(noticiasDF$Titular),]
  return(noticiasDF)
}

obtieneNoticiasData = function(noticia){
  news_pag = "https://news.google.com/"
  titular = noticia %>% html_node("h3") %>% html_text()
  fecha = noticia %>% html_node("time") %>% html_attr("datetime")
  diario = noticia %>% html_node("a.wEwyrc.AVN2gc.uQIVzc.Sksgp") %>% html_text()
  link_enmascarado = noticia %>% html_node("h3 a") %>% html_attr("href")
  link_enmascarado = paste0(news_pag,substring(link_enmascarado,3))  
  link_leido = read_html(link_enmascarado)
  link = link_leido %>% 
    html_nodes(css='a') %>% 
    tail(1) %>% 
    html_attr("href")
  noticiaDF = data.frame(Titular=titular, Fecha=fecha, Diario=diario, Link=link, stringsAsFactors = F)
  return(noticiaDF)
}


obtenerNoticiaNacional = function(link_noticia, diario, diccionario_css){
  
  noticia_leida = read_html(link_noticia)
  css = diccionario_css$CSS[diccionario_css$Diario==diario]
  
  text_nodes = noticia_leida %>% 
    html_nodes(css = css) %>% 
    html_nodes("p")
  
  text = text_nodes %>% 
    html_text()
  
  text = paste0(text, collapse = " ")
  
  return(text)
  
}


CONAFIPSDF = obtieneNoticiasBusqueda(busqueda = "CONAFIPS")

# Busqueda de noticias--------

BanEcuador= obtieneNoticiasBusqueda(busqueda = "Banecuador,BanEcuador when:540d")
CFN= obtieneNoticiasBusqueda(busqueda = "CFN,Corporación Financiera Nacional when:540d")
BIESS= obtieneNoticiasBusqueda(busqueda = "BIESS when:540d")
BDE= obtieneNoticiasBusqueda(busqueda = "BDE,Ecuador when:540d")
CONAFIPS = obtieneNoticiasBusqueda(busqueda = "CONAFIPS when:540d")
#noticiasCONAFIPS = obtieneNoticiasBusqueda(busqueda="CONAFIPS when:41d")# Buscar desde cierta fecha

library(rio)

rio::export(noticiasCONAFIPS,file = "NC.xlsx")

# Tweet por cada BP

BP <-c("CONAFIPS","CFN","BanEcuador","BIESS","BDE")
NOTICIAS<-c(24,82,100,100,51)
BPNOTICIAS <-cbind(BP,NOTICIAS)

Time <- data.frame(BP,NOTICIAS)

p<-Time %>%
  mutate(BP = fct_reorder(BP, NOTICIAS)) %>%
  ggplot( aes(x=BP,NOTICIAS)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = NOTICIAS), data = Time,size=10)+
  coord_flip() +
  ggtitle("Comparativo de noticias período presidencial",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  xlab("") +
  theme_classic()

p+theme(text = element_text(size = 30))

## Poner nombre a los dataframes de la BP-------
#BanEcuador

Banco=rep("BanEcuador",100)
N.BanEcuador= cbind(BanEcuador,Banco)

#N.BanEcuador=N.BanEcuador %>% select(-6)

# CFN

Banco=rep("CFN",82)
N.CFN=cbind(CFN,Banco)

#N.CFN=N.CFN %>% select(-6)

#BIESS

Banco=rep("BIESS",100)
N.BIESS=cbind(BIESS,Banco)

#N.BIESS=N.BIESS %>% select(-6)

#BDE

Banco=rep("BDE",50)
N.BDE=cbind(BDE,Banco)

#N.BDE=N.BDE %>% select(-6)

#CONAFIPS

Banco=rep("CONAFIPS",24)
N.CONAFIPS=cbind(CONAFIPS,Banco)

#CONAFIPS=CONAFIPS %>% select(-Banco)

BP=rbind(N.BanEcuador,N.CFN, N.BDE, N.BIESS,N.CONAFIPS)


# Hacer el ranking de diarios------

BP %>% select(Fecha, Diario, Titular, Link)

names(BP)

my_tab_sort1=table(BP$Diario)

my_tab_sort1 <- my_tab_sort1[order(my_tab_sort1)]# Ordenar tabla
my_tab_sort1                               


ggplot(data = BP, aes(x = Banco, fill = Diario))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ggtitle("Noticias por cada Diario",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  facet_wrap(~Diario)+theme(text = element_text(size = 10))



### Distribución por separado ###
ggplot(BP, aes(x = as.Date(Fecha), fill = Banco)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 month") +
  labs(x = "Fecha de publicacion", y = "NÚmero de noticias") +
  ggtitle("Noticias sobre cada Banco en el tiempo",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  facet_wrap(~ Banco, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 13))


BP$Diario




# Diarios
Diarios = c("El Comercio (Ecuador)", "El Universo", "Primicias","Diario La Hora","expreso.ec","Ecuavisa")
Estructura = data.frame(Diario=Diarios)
Estructura$CSS = NA
Estructura$CSS[Estructura$Diario=='EL COMERCIO (Ecuador)'] = '.entry__content' #Revisar
Estructura$CSS[Estructura$Diario=='El Universo'] = '.article-body'
Estructura$CSS[Estructura$Diario=='Primicias'] = '#entry-content-inarticle'
Estructura$CSS[Estructura$Diario=='Diario La Hora'] = '.content'
Estructura$CSS[Estructura$Diario=='expreso.ec'] = '.box3x1.flo-left.con'
Estructura$CSS[Estructura$Diario=='Ecuavisa'] = '.paragraph'




# CONAFIPS
BP= BP%>% filter(Diario %in% Diarios)
news = map2_chr(BP$Link, BP$Diario, obtenerNoticiaNacional, diccionario_css=Estructura)
BP$Noticia = news
print(BP$Link[1])

print(BP$Titular[1])

print(news[1])

head(BP %>% select(Fecha, Diario, Titular, Link, Noticia))

#Diarios con CSS------

ggplot(data = BP, aes(x = Banco, fill = Diario))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ggtitle("Noticias por cada Diario",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  facet_wrap(~Diario)+theme(text = element_text(size = 15))


#agrupo por nombre de usuario y totalizo
BP %>% group_by(Banco) %>% summarise(Titular = n()) 

#Columnas disponibles
colnames(BP) %>% view()


#### Limpieza de texto y tokenizaciÃÂ³n ####
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minÃÂºsculas
  nuevo_texto <- tolower(texto)
  # EliminaciÃÂ³n de pÃÂ¡ginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # EliminaciÃÂ³n de signos de puntuaciÃÂ³n
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # EliminaciÃÂ³n de nÃÂºmeros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # EliminaciÃÂ³n de espacios en blanco mÃÂºltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # TokenizaciÃÂ³n por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # EliminaciÃÂ³n de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

test = "Esto es 1 ejemplo de l'limpieza de6 TEXTO  https://medium.com/restevesd @restevesd #textmining"
limpiar_tokenizar(texto = test)

## Libreria TM ####
library(tm)
test <- tolower(test) #convertimos todo a minÃÂºsculas
print(test)
test <- removeWords(test, words = stopwords("spanish"))
print(test)
test <- removePunctuation(test)
print(test)
test <- removeNumbers(test)
print(test)
test <- stripWhitespace(test)
print(test)

#tokenizar con el paquete TM
test <- MC_tokenizer(test)
print(test)

# Se aplica la funciÃÂ³n de limpieza y tokenizaciÃÂ³n a cada tweet
BP <- BP %>% mutate(texto_tokenizado = map(.x =Noticia,
                                                   .f = limpiar_tokenizar))

BP %>% select(texto_tokenizado) %>% head() %>% view()

BP %>% slice(1) %>% select(texto_tokenizado) %>% pull()

#### Análisis exploratorio #####
## Ir a la ppt ##
BP_tidy <- BP %>% select(-Noticia) %>% unnest()
BP_tidy <- BP_tidy %>% rename(token = texto_tokenizado)
head(BP_tidy) 

## La funciÃÂ³n unnest_tokens() del paquete tidytext permite, entre otras cosas, automatizar el
## proceso tokenizaciÃÂ³n y almacenamiento en formato tidy en un ÃÂºnico paso.
## https://www.tidytextmining.com/

### DistribuciÃÂ³n temporal de los tweets ####





#### Frecuencia de palabras ####

## Total de palabras escritas por cada usuario ##
BP_tidy %>% group_by(Banco) %>% summarise(n = n()) 
BP_tidy %>%  ggplot(aes(x = Banco)) + geom_bar() +ggtitle("Total de palabras por Banco",subtitle = "Elaborado por: Aníbal Garrido Meneses")+ 
  coord_flip() + theme_classic()+theme(text = element_text(size = 30))


### Palabras distintas utilizadas por cada usuario ###
BP_tidy %>% select(Banco, token) %>% distinct() %>%  group_by(Banco) %>%
  summarise(palabras_distintas = n()) 

BP_tidy %>% select(Banco, token) %>% distinct() %>%
  ggplot(aes(x = Banco)) + geom_bar() +ggtitle("Palabras distintas por Banco",subtitle = "Elaborado por: Aníbal Garrido Meneses")+ coord_flip() + theme_classic()                        



### Palabras mÃÂ¡s utilizadas por usuario ###
BP_tidy %>% group_by(Banco, token) %>% count(token) %>% group_by(Banco) %>%
  top_n(9, n) %>% arrange(Banco, desc(n)) %>% print(n=9)

### Filtramos StopWords ###
BP_tidy <- BP_tidy %>% filter(!(token %in% tm::stopwords(kind="es")))

### RepresentaciÃÂ³n grÃÂ¡fica de las frecuencias ###
BP_tidy %>% group_by(Banco, token) %>% count(token) %>% group_by(Banco) %>%
  top_n(5, n) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = Banco)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  ggtitle("Palabras más usada por cada banco",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  coord_flip() +
  facet_wrap(~Banco,scales = "free", ncol = 1, drop = TRUE)+
  theme(text = element_text(size = 12))

### Word Clouds ### Nube de palabras ### 

#install.packages("ggplot2")
library(ggplot2)

df_grouped <- BP_tidy %>% group_by(Banco, token) %>% count(token) %>%
  group_by(Banco) %>% mutate(frecuencia = n / n()) %>%
  arrange(Banco, desc(frecuencia))

## 1 forma ##-----------------------------------------------------
library(ggwordcloud)

df_grouped %>%
  slice(1:100) %>%
  mutate(sentiment = factor(Banco, levels = c("BanEcuador", "CFN","BDE","BIESS","CONAFIPS"))) %>%
  ggplot(aes(label = token, color = Banco, size = n)) +
  geom_text_wordcloud_area() + 
  facet_wrap(~ Banco, nrow = 3)

## 2 forma ##
df_frame <- as.data.frame(df_grouped)
df_frame %>% select(token,n) %>%
  wordcloud2(minSize = 5,shape = "circle",size = .7)



### CorrelaciÃ³n entre usuarios por palabras utilizadas ###--------------------------------
library(gridExtra)
library(scales)

BP_spread <- BP_tidy %>% group_by(Banco, token) %>% count(token) %>%
  spread(key = Banco, value = n, fill = NA, drop = TRUE)

cor.test(~ CONAFIPS + BanEcuador, method = "pearson", data = BP_spread)


#Correlación todas las organizaciones------

library(corrplot)

BP_spread <- BP_tidy %>% group_by(Banco, token) %>% count(token) %>%
  spread(key = Banco, value = n, fill = NA, drop = TRUE)
BP_spread[is.na(BP_spread)] <- 0

names(BP_spread) <- c("token", "BanEcuador", "CFN", 
                          "BDE", "BIESS", "CONAFIPS")

method <- "pearson"
m_cor <- matrix(nrow = 5, ncol = 5)
for (i in 1:dim(m_cor)[1]) {
  for (j in 1:dim(m_cor)[2]) {
    form <- as.formula(paste("~", names(BP_spread)[i+1], 
                             "+", names(BP_spread)[j+1]))
    if(i!=j){
      m_cor[i,j] <- cor.test(form, method = method, 
                             data = BP_spread)$estimate
    }
    if(i==j){m_cor[i,j] <- 1}
  }
}
colnames(m_cor) <- names(BP_spread)[2:6]
rownames(m_cor) <- names(BP_spread)[2:6]
corrplot(m_cor, method="color", type="upper", order="hclust", 
         addCoef.col = "black", tl.col="black", tl.srt=45,
         sig.level = 0.01, insig = "blank", diag=FALSE)

corrplot(m_cor, method = 'color', order = 'alphabet') 

## Correlaciones por Banco

p1 <- ggplot(BP_spread, aes(CONAFIPS,BanEcuador)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5,size=8) +
  scale_x_log10(labels = percent_format(1.0)) +
  scale_y_log10(labels = percent_format(1.0)) +
  geom_abline(color = "red") +
  ggtitle("Correlación BanEcuador-CONAFIPS",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())+theme(text = element_text(size = 20))


p2 <- ggplot(BP_spread, aes(CONAFIPS,CFN)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5,size=8) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Correlación CFN-CONAFIPS",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())+theme(text = element_text(size = 20))


p3 <- ggplot(BP_spread, aes(CONAFIPS, BIESS)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5,size=8) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Correlación BIESS-CONAFIPS",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())+theme(text = element_text(size = 20))

p4 <- ggplot(BP_spread, aes(CONAFIPS,BDE)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5,size=8) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  ggtitle("Correlación BDE-CONAFIPS",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())+theme(text = element_text(size = 20))

grid.arrange(p1,p2, nrow = )

grid.arrange(p3, p4, nrow = )


## Para poder valorar adecuadamente el nivel de correlaciÃ³n es interesante 
## conocer el nÃºmero de palabras comunes entre cada par de autores.

palabras_comunes <- dplyr::intersect(BP_tidy %>% filter(Banco=="BanEcuador") %>%
                                       select(token), BP_tidy %>% filter(Banco=="CONAFIPS") %>%
                                       select(token)) %>% nrow()

paste("Numero de palabras comunes entre CONAFIPS y BanEcuador",subtitle = "Elaborado por: Aníbal Garrido Meneses", palabras_comunes)

r1 <- dplyr::intersect(BP_tidy %>% filter(Banco=="BanEcuador") %>%
                         select(token), BP_tidy %>% filter(Banco=="CONAFIPS") %>%
                         select(token)) %>% nrow()

r2<- dplyr::intersect(BP_tidy %>% filter(Banco=="CFN") %>%
                        select(token), BP_tidy %>% filter(Banco=="CONAFIPS") %>%
                        select(token)) %>% nrow()

r3<- dplyr::intersect(BP_tidy %>% filter(Banco=="BIESS") %>%
                        select(token), BP_tidy %>% filter(Banco=="CONAFIPS") %>%
                        select(token)) %>% nrow()

r4<- dplyr::intersect(BP_tidy %>% filter(Banco=="BDE") %>%
                        select(token), BP_tidy %>% filter(Banco=="CONAFIPS") %>%
                        select(token)) %>% nrow()


Palabras_comunes=c(r1,r2,r3,r4)

Nombres <- c("BanEcuador","CFN","BIESS","BDE")

comunes=data.frame(Nombres,Palabras_comunes)

ggplot(data=comunes, aes(x=Nombres, y=Palabras_comunes))+
  geom_bar(stat="identity",width = 0.5)+
  ggtitle("Palabras comunes con CONAFIPS",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  theme_classic()+theme(text = element_text(size = 30))

# ODDS RADIO

k <- 1
users <- unique(BP_tidy$Banco)
list_logOdds <- list()
p2 <- list()
# Pivotaje y despivotaje
BP_unpivot <- BP_tidy %>% group_by(Banco, token) %>%
  count(token) %>%
  spread(key = Banco, value = n, fill = 0, drop = TRUE) %>% 
  gather(key = "Banco", value = "n", -token)
for (i in 1:length(users)) {
  for (j in 1:length(users)) {
    if(j>i){
      
      # SelecciÃ³n de los autores
      BP_unpivot2 <- BP_unpivot %>% 
        filter(Banco %in% c(users[i], users[j]))
      # Se aÃ±ade el total de palabras de cada autor
      BP_unpivot2 <- BP_unpivot2 %>%
        left_join(BP_tidy %>% group_by(Banco) %>%
                    summarise(N = n()), by = "Banco")
      # CÃ¡lculo de odds y log of odds de cada palabra
      BP_logOdds <- BP_unpivot2 %>% 
        mutate(odds = (n + 1) / (N + 1)) %>%
        select(Banco, token, odds) %>% 
        spread(key = Banco, value = odds)
      BP_logOdds[,4] <- log(BP_logOdds[,2]/BP_logOdds[,3])
      names(BP_logOdds)[4] <- "log_odds"
      BP_logOdds[,5] <- abs(BP_logOdds$log_odds)
      names(BP_logOdds)[5] <- "abs_log_odds"
      BP_logOdds <- BP_logOdds %>%
        mutate(Banco_frecuente = if_else(log_odds > 0,
                                         names(BP_logOdds)[2],
                                         names(BP_logOdds)[3]))
      list_logOdds[[k]] <- BP_logOdds
      p2[[k]] <- BP_logOdds %>% group_by(Banco_frecuente) %>% 
        top_n(10, abs_log_odds) %>%
        ggplot(aes(x = reorder(token, log_odds),
                   y = log_odds, fill = Banco_frecuente)) +
        geom_col() +
        labs(x = "palabra",
             y = paste0("log odds ratio (", users[i], " / ",
                        users[j],")")) + coord_flip() + 
        theme()+theme(text = element_text(size = 25))
      
      
      k <- k + 1
    }
  }
}
head(list_logOdds[[1]])

p2

### RelaciÃÂÃÂ³n entre palabras ####
library(tidytext)

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minÃÂÃÂºsculas
  nuevo_texto <- tolower(texto)
  # EliminaciÃÂÃÂ³n de pÃÂÃÂ¡ginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # EliminaciÃÂÃÂ³n de signos de puntuaciÃÂÃÂ³n
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # EliminaciÃÂÃÂ³n de nÃÂÃÂºmeros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # EliminaciÃÂÃÂ³n de espacios en blanco mÃÂÃÂºltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

bigramas <- BP %>% mutate(Noticia = limpiar(Noticia)) %>%
  select(Noticia) %>%
  unnest_tokens(input = Noticia, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas  %>% count(bigrama, sort = TRUE)

# SeparaciÃÂÃÂ³n de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% tm::stopwords(kind="es")) %>%
  filter(!palabra2 %in% tm::stopwords(kind="es"))

# UniÃÂÃÂ³n de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes

Bigra=data.frame(table(bigramas_BP$Banco,bigramas_BP$bigrama))
head(Bigra)

bigramas_BP  %>% count(bigrama, sort =TRUE) %>% print(n = 5)


bigramas_BP %>% group_by(bigrama) %>% summarise(total=n()) %>% 
  arrange(desc(total)) %>%  top_n(15) %>% 
  ggplot(aes(x = reorder(bigrama,total), y = total)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Conteo de Bigramas", x = "Bigramas",
       y = "Cantidad")

### Hacer Bigramas por USUARIO ###
### Ejercicio Intentar hacer 3gramas ###

### NETWORKS ####
# Una forma mÃÂÃÂ¡s visual e informativa de analizar las relaciones entre palabras es
# mediante el uso de networks. 
# El paquete igraph permite crear networks a partir de dataframes 
# que tenga una estructura de columnas de tipo: elemento_A, elemento_B, conexiÃÂÃÂ³n.

library(igraph)
library(ggraph)

graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 10) %>% graph_from_data_frame(directed = FALSE)

set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

ggraph(graph) +
  geom_edge_link() +
  geom_node_point(size = 4, colour = 'steelblue') +
  theme_graph()

ggraph(graph = graph) +
  geom_edge_link(colour = "gray70")  +
  geom_node_text(aes(label = name), size = 4) +
  theme_bw()

ggraph(graph = graph) +
  geom_edge_link(edge_width = 0.25, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

## https://rpubs.com/ben_bellman/rtweet_tidygraph ##
## https://perrystephenson.me/2018/09/29/the-r-twitter-network/ ##

#### Term Frequency e Inverse Document Frequency ####

# NÃºmero de veces que aparece cada tÃÂÃÂ©rmino por tweet
BP_tf <- BP_tidy %>% group_by(Banco, token) %>% summarise(n = n())

# Se aÃÂÃÂ±ade una columna con el total de tÃÂÃÂ©rminos por tweet
BP_tf <- BP_tf %>% mutate(total_n = sum(n))

# Se calcula el tf
BP_tf <- BP_tf %>% mutate(tf = n / total_n )
head(BP_tf)

### Inverse Document Frequency ###
total_documentos = BP_tidy$Banco %>% unique() %>% length()
total_documentos

# NÃÂÃÂºmero de documentos en los que aparece cada tÃÂÃÂ©rmino
BP_idf <- BP_tidy %>% distinct(token, Banco) %>% group_by(token) %>%
  summarise(n_documentos = n())

# CÃÂÃÂ¡lculo del idf
BP_idf <- BP_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))
head(BP_idf)

### Term Frequency - Inverse Document Frequency ###
BP_tf_idf <- left_join(x = BP_tf, y = BP_idf, by = "token") %>% ungroup()
BP_tf_idf <- BP_tf_idf %>% mutate(BP_idf = tf * idf)
BP_tf_idf %>% head(20) %>% view()
#### FIN PARTE 5 ####


#### SeparaciÃÂÃÂ³n de los datos en entrenamiento y test ####
tweets_CONAFIPS_BanEcuador<- tweets %>% filter(autor %in% c("CONAFIPS", "BanEcuadorBP"))

set.seed(123)
train <- sample(x = 1:nrow(tweets_CONAFIPS_BanEcuador), size = 0.8 * nrow(tweets_CONAFIPS_BanEcuador))
tweets_train <- tweets_CONAFIPS_BanEcuador[train, ]
tweets_test  <- tweets_CONAFIPS_BanEcuador[-train, ]

#Es importante verificar que la proporciÃÂÃÂ³n de cada grupo es similar en el set de 
#entrenamiento y en el de test.
table(tweets_train$autor) / length(tweets_train$autor)
table(tweets_test$autor) / length(tweets_test$autor)


#### VectorizaciÃÂÃÂ³n tf-idf #####
#Empleando los tweets de entrenamiento se crea un matriz tf-idf en la que cada 
#columna es un tÃÂÃÂ©rmino, cada fila un documento y el valor de intersecciÃÂÃÂ³n el tf-idf 
#correspondiente. Esta matriz representa el espacio n-dimensional en el que se proyecta 
#cada tweet. Las funciones dfm() y tfidf() del paquete quanteda automatizan la creaciÃÂÃÂ³n 
#una matriz df-idf a partir de un corpus de documentos. AdemÃÂÃÂ¡s, incorpora un tokenizador
#con mÃÂÃÂºltiples opciones de limpieza

library(quanteda)
texto <- paste0("Esto es 1 ejemplo de l'limpieza de6 TEXTO",
                "https://medium.com @restevesd #textmining")

matriz_tfidf <- dfm(x = texto, what = "word", remove_numbers = TRUE,
                    remove_punct = TRUE, remove_symbols = TRUE,
                    remove_separators = TRUE, remove_twitter = FALSE,
                    split_hyphens = TRUE, remove_url = FALSE)

colnames(matriz_tfidf) #versiÃÂÃÂ³n 1

limpiar_tokenizar(texto = texto) #versiÃÂÃÂ³n 2 de limpieza

#La funciÃÂÃÂ³n dfm() interpreta cada elemento de un vector character como un documento 
#distinto. Como el resultado de la funciÃÂÃÂ³n limpiar_tokenizar() trasforma cada tweet en
#un vector de palabras, es necesario concatenar el resultado para que formen de nuevo 
#un ÃÂÃÂºnico elemento.

paste(limpiar_tokenizar(texto = texto), collapse = " ") # limpieza y pegado como un solo vector

# Limpieza y tokenizaciÃÂÃÂ³n de los documentos de entrenamiento
tweets_train$texto <- tweets_train$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()

# CreaciÃÂÃÂ³n de la matriz documento-tÃÂÃÂ©rmino
matriz_tfidf_train <- dfm(x = tweets_train$texto, remove = tm::stopwords(kind="es"))

# Se reduce la dimensiÃÂÃÂ³n de la matriz eliminando aquellos tÃÂÃÂ©rminos que 
# aparecen en menos de 5 documentos. Con esto se consigue eliminar ruido.
matriz_tfidf_train <- dfm_trim(x = matriz_tfidf_train, min_docfreq = 5)

# ConversiÃÂÃÂ³n de los valores de la matriz a tf-idf
matriz_tfidf_train <- dfm_tfidf(matriz_tfidf_train, scheme_tf = "prop",
                                scheme_df = "inverse")

matriz_tfidf_train %>% View() ## para ver la matriz tf_idf

# Limpieza y tokenizaciÃÂÃÂ³n de los documentos de test
tweets_test$texto <- tweets_test$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()

# IdentificaciÃÂÃÂ³n de las dimensiones de la matriz de entrenamiento
# Los objetos dm() son de clase S4, se accede a sus elementos mediante @
dimensiones_matriz_train <- matriz_tfidf_train@Dimnames$features

# ConversiÃÂÃÂ³n de vector a diccionario pasando por lista
dimensiones_matriz_train <- as.list(dimensiones_matriz_train)
names(dimensiones_matriz_train) <- unlist(dimensiones_matriz_train)
dimensiones_matriz_train <- dictionary(dimensiones_matriz_train)

# ProyecciÃÂÃÂ³n de los documentos de test
matriz_tfidf_test <- dfm(x = tweets_test$texto,
                         dictionary = dimensiones_matriz_train)
matriz_tfidf_test <- dfm_tfidf(matriz_tfidf_test, scheme_tf = "prop",
                               scheme_df = "inverse")

matriz_tfidf_test

### Se comprueba que las dimensiones de ambas matrices son iguales.###
all(colnames(matriz_tfidf_test) == colnames(matriz_tfidf_train))

#### Modelo SVM lineal ####

library(e1071)
#instalar librerÃÂÃ¯Â¿Â½a SparseM

modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                  kernel = "linear", cost = 1, scale = TRUE,
                  type = "C-classification")
modelo_svm

#### Predicciones ####
#Empleando el modelo entrenado en el paso anterior se predice la autorÃÂÃ¯Â¿Â½a de los 
#tweets de test.

predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)

#### Matriz de confusiÃÂÃÂ³n ####
table(observado = tweets_test$autor, predicho = predicciones)

# Error de clasificacion
clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
error <- 100 * mean(tweets_test$autor != predicciones)
paste("Numero de clasificaciones incorrectas =", clasificaciones_erroneas)
paste("Porcentaje de error =", round(error,2), "%")

#### OptimizaciÃÂÃÂ³n de hiperparÃÂÃÂ¡metros ####
set.seed(369)
svm_cv <- tune("svm", train.x =  matriz_tfidf_train,
               train.y = as.factor(tweets_train$autor),
               kernel = "linear", 
               ranges = list(cost = c(0.1, 0.5, 1, 2.5, 5)))
summary(svm_cv)

#### Mejor parÃÂÃÂ¡metro ####
svm_cv$best.parameters

modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                  kernel = "linear", cost = 1, scale = TRUE)

predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)
table(observado = tweets_test$autor, predicho = predicciones)

clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
error <- 100 * mean(tweets_test$autor != predicciones)
paste("NÃÂÃÂºmero de clasificaciones incorrectas =", clasificaciones_erroneas)
paste("Porcentaje de error =", round(error,2.5), "%")
#### FIN PARTE 6 ####


### Sentimiento ####
library(tidytext)
library(tidyverse)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

#### Sentimiento promedio de cada tweet ####

BP_sent <- inner_join(x = BP_tidy, y = sentimientos,
                          by = c("token" = "word"))

### Se suman los sentimientos de las palabras que forman cada tweet. ###
BP_sent %>% group_by(Banco) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  head()

#Porcentajes de sentimiento y lineal temporal

BP_sent$Fecha <- as.Date(BP_sent$Fecha)

# positivo - negativo
# tweets_sent[tweets_sent$tipo!="",] %>%
library(scales)

BP_sent[BP_sent$sentiment!=0|BP_sent$sentiment!=0,] %>%
  count(Banco, sentiment) %>%
  group_by(Banco) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Banco, Proporcion, fill = sentiment) +
  geom_col() +
  ggtitle("Porcentaje de sentimientos por Banco",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position = "top")+
  theme(text = element_text(size = 30))

# tweets_sent[tweets_sent$tipo!="",] %>%
BP_sent[BP_sent$sentiment!=0|BP_sent$sentiment!=0,] %>%
  group_by(Banco, Fecha) %>%
  count(sentiment) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = sentiment) +
  geom_col(width = 1) +
  facet_grid(Banco~.) +
  ggtitle("Sentimientos en el tiempo por Banco",subtitle = "Elaborado por: Aníbal Garrido Meneses")+
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  theme(legend.position = "top")+
  theme_classic()+
  theme(text = element_text(size = 15))
  


table(BP_sent$sentiment,BP$Banco)

#### Porcentaje de tweets positivos, negativos y neutros por autor ####

porcentaje_sentimiento <- BP_sent %>% group_by(Banco) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(Banco) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())




porcentaje_sentimiento %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -Banco) %>%
  ggplot(aes(x = Banco, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_classic()

textostop <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol
textostop <- as_tibble(textostop) # lo convierto a tibble

## cambio de nombre a words la columna ##
colnames(textostop)
colnames(textostop)[1] <- "token"

dic_nrc <- get_sentiments("nrc")
colnames(dic_nrc)
colnames(dic_nrc)[1] <- "token"

sentiment_nrc_tbl <- BP_tidy %>%
  inner_join(dic_nrc, by = "token")


library(ggjoy)


## analizando todo el tiempo disponible ##

#sentiment_nrc_tbl =sentiment_nrc_tbl [ grep("coop", sentiment_nrc_tbl$token, invert = TRUE) , ]

sentiment_nrc_tbl%>% 
  ggplot() +
  geom_joy(aes(
    x = Fecha,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Sentimiento en los posteos en el tiempo de los bancos",subtitle = "Elaborado por: Aníbal Garrido Meneses",
       x = "Fecha",
       y = "Sentimento") + 
  scale_fill_discrete(guide=F)+
  facet_wrap( ~ Banco, ncol=2)+
  theme(text = element_text(size = 17))

table(sentiment_nrc_tbl$Banco, sentiment_nrc_tbl$sentiment)

ggplot(sentiment_nrc_tbl, aes(y = Banco)) +
  geom_bar(aes(fill = sentiment), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")

#Crear la funciÃÂÃÂ³n coord_radar()
#CÃÂÃÂ³digo cortesÃÂÃ¯Â¿Â½a de Erwan Le Pennec
#http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

sentiment_nrc_tbl %>%
  group_by(Banco, sentiment) %>%
  tally %>%
  ggplot(aes(x=sentiment, y=n, group=Banco)) +
  geom_polygon(aes(color = Banco),        #geom_polygon para que cierre las lÃÂÃ¯Â¿Â½neas.
               fill = NA, size = 1.1) +  #fill=NA para relleno del polÃÂÃ¯Â¿Â½gono sea transparente.
  theme(axis.ticks.y = element_blank(),  #Elimino marcas de ejes
        axis.text.y = element_blank()) + #Elimino nombres de ejes.
  labs(title="Analizando sentimientos acerca de los bancos",
       subtitle="Elaborado por: Aníbal Garrido Meneses",
       caption="Análisis de sentimientos",
       x="",
       y="") +                           #Elimino etiquetas de ejes
  coord_radar()+theme(text = element_text(size = 14))

