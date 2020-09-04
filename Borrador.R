library(sp)
library(geojsonio)
library(rgdal)
library(rio)

#Unidad 1. Colección y preprocesamiento de datos
#Leyendo data
linkprueba = "https://github.com/JairAlva/Estadistica-II/raw/master/prueba.xlsx"
prueba = import(linkprueba)

fromGit = "https://raw.githubusercontent.com/JairAlva/Estadistica-II/master/seattle.json"
wazipMap <- rgdal::readOGR(fromGit,stringsAsFactors = FALSE)
plot(wazipMap)

#Datos abiertos de San Isidro
millave = "VPV6jDIBmg5PpXBoaAz5SOulsMiIQdPYIDgisV4d"

GUID = "http://api.datosabiertos.msi.gob.pe/api/v2/datastreams/INTER-DE-SEREN-2018/"

FORMATO = "data.json/"

request=paste0(GUID,FORMATO,'?auth_key=',millave)
request

library(jsonlite) 
serenosSI = fromJSON(request)
serenosSI

FORMATO="data.pjson/"
request2=paste0(GUID,FORMATO,'?auth_key=',millave)

serenosSI = fromJSON(request2)$result
str(serenosSI)

head(serenosSI$result)

dim(serenosSI$result)

Parametros='&from=01/01/2010'
request3=paste0(GUID,FORMATO,'?auth_key=',millave,Parametros)
serenosSI_all = fromJSON(request3)$result

dim(serenosSI_all)

#scraping
linkPage = "https://www.nationsonline.org/oneworld/corruption.htm"
linkPath = "/html/body/table[3]"

library(htmltab)

corrupcion = htmltab(doc = linkPage, which =linkPath) 

#Preprocesamiento de datos
library(htmltab) 

linkCIA_urb = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/349.html"
linkPath_urb='//*[@id="fieldListing"]'

urban = htmltab(doc = linkCIA_urb, 
                which =linkPath_urb)

urban[1,2]

library(stringr)
# me trae cada numero
str_extract_all('25.3%,0% y 23.5% 13 34 hola',"\\d")

# me trae numeros adyacentes:
str_extract_all('25.3%,0% y 23.5% 13 34 hola',"\\d+") # + es al menos 1 / * es 0 o más

# numero entero, seguido opcionalmente de punto y mas numero de una o mas cifras.
str_extract_all('25.3%,0% y 23.5% 13 34 hola',"\\d+\\.*\\d*")

# numero entero, seguido opcionalmente de punto y mas numero de una o mas cifras y %.
str_extract_all('25.3%,0% y 23.5% 13 34 hola', "\\d+\\.*\\d*\\%")

# porcentaje sin el simbolo

#  \\-

str_extract_all('25.3%,0% y 23.5% 13 34 hola', "(\\d+\\.*\\d*)(?=\\%)")

# porcentaje sin el simbolo

str_extract_all('25.3%,0% y -23.5% 13 34 hola', "(\\d+\\.*\\d*)(?=\\%)")

# porcentaje sin el simbolo hasta negativos

str_extract_all('25.3%,0% y -23.5% 13 34 hola', "(\\-*\\d+\\.*\\d*)(?=\\%)")

# con [[1]] recien accedemos al elemento:
str_extract_all('25.3%, 0%y 23%', "(\\-*\\d+\\.*\\d*)(?=\\%)")[[1]]

# primer valor es
str_extract_all('25%, 0% y 23.5%', "(\\-*\\d+\\.*\\d*)(?=\\%)")[[1]][1]

# tercer valor es
str_extract_all('25%, 0% y 23.5%', "(\\-*\\d+\\.*\\d*)(?=\\%)")[[1]][3]

#Aplicado a la base de datos exportada
str_extract_all(urban$Urbanization,pattern="(\\-*\\d+\\.*\\d*)(?=\\%)")

str_extract_all(urban$Urbanization,pattern="(\\-*\\d+\\.*\\d*)(?=\\%)",simplify = T)

PATRON="(\\-*\\d+\\.*\\d*)(?=\\%)"
COLSUCIA=urban$Urbanization

# UNA COLUMNA
urban$pop_urb=str_extract_all(string = COLSUCIA,pattern=PATRON,simplify = T)[,1]

# OTRA COLUMNA
urban$rate_urb=str_extract_all(string = COLSUCIA,pattern=PATRON,simplify = T)[,2]

head(urban[,-2])

#PARTICIONES
# recuerda:
test=urban[1,2]
test

str_split(test,pattern = 'rate of urbanization:')

urban$pop_urb2=str_split(urban$Urbanization,
                         pattern = 'rate of urbanization:',
                         simplify = T)[,1]

urban$rate_urb2=str_split(urban$Urbanization,
                          pattern = 'rate of urbanization:',
                          simplify = T)[,2]

urban$pop_urb2=str_split(urban$pop_urb2,
                         pattern = '% of total',
                         simplify = T)[,1]

urban$pop_urb2=str_split(urban$pop_urb2,pattern = ':',simplify = T)[,2]

urban$rate_urb2=str_split(urban$rate_urb2,pattern = '%',simplify = T)[,1]

head(urban[,-2])


urban$pop_urb2 = as.numeric(urban$pop_urb2)
urban$rate_urb2 = as.numeric(urban$rate_urb2)
#Parsers
library(readr)
parse_number(urban$Urbanization)

library(magrittr) # para %>%

urban$popr3 = str_split(urban$Urbanization,pattern = 'rate of urbanization:',
          simplify = T)[,1]%>%parse_number()

urban$rate3 = str_split(urban$Urbanization,pattern = 'rate of urbanization:',
          simplify = T)[,2]%>%parse_number()


#Preparación de tabla de datos
Lk="https://github.com/PoliticayGobiernoPUCP/estadistica_anapol2/raw/master/DATA/AGUA.xlsx"

agua=import(Lk)

aggregate(cbind(todaSemana, NoTodaSemana) # dependientes
          ~ Provincia, # nivel
          data = agua,    # data
          sum)            # operacion
#Ejericcio de limpieza
#mi tesis
library(rio)
rev = import("C:/Users/Jair Alva/Desktop/PUCP/Tesis/Base de datos/COPIA MADRE - Determinantes CPRA (2012-2013  2017).xlsx",
             which = 1)
library(tidyverse)
library(magrittr)

revocatorias = rev %>% rename(departamento=...1, provincia=...4, distritos=...5, rev2017=...18,
               rev2013=...22, rev2012=...26, rev2009=...30, rev2008=...34, 
               rev2005=...38, rev2004=...42, rev2001=...46, rev1997=...50) %>%
  select(departamento, provincia, distritos, rev1997, rev2001, rev2004, rev2005,
         rev2008, rev2009, rev2012, rev2013, rev2017) %>%
  drop_na(distritos) %>%
  filter(!distritos=="DISTRITO") %>%
  mutate(distubigeo = str_split(distritos, " ", n = 2, simplify = T)[,1]) %>%
  filter(!distubigeo=="01") %>%
  select(!distubigeo) %>%
  mutate(rev1997=as.numeric(rev1997), rev2001=as.numeric(rev2001), 
         rev2004=as.numeric(rev2004), rev2005=as.numeric(rev2005), 
         rev2008=as.numeric(rev2008), rev2009=as.numeric(rev2009), 
         rev2012=as.numeric(rev2012), rev2013=as.numeric(rev2013),
         rev2017=as.numeric(rev2017)) %>% 
  mutate(rev1997 = replace(rev1997, which(is.na(rev1997)), 0), 
         rev2001 = replace(rev2001, which(is.na(rev2001)), 0),
         rev2004 = replace(rev2004, which(is.na(rev2004)), 0),
         rev2005 = replace(rev2005, which(is.na(rev2005)), 0),
         rev2008 = replace(rev2008, which(is.na(rev2008)), 0),
         rev2009 = replace(rev2009, which(is.na(rev2009)), 0),
         rev2012 = replace(rev2012, which(is.na(rev2012)), 0),
         rev2013 = replace(rev2013, which(is.na(rev2013)), 0),
         rev2017 = replace(rev2017, which(is.na(rev2017)), 0)) %>%
  mutate(rev1997=if_else(rev1997>=1, 1, 0), rev2001=if_else(rev2001>=1, 1, 0),
         rev2004=if_else(rev2004>=1, 1, 0), rev2005=if_else(rev2005>=1, 1, 0),
         rev2008=if_else(rev2008>=1, 1, 0), rev2009=if_else(rev2009>=1, 1, 0),
         rev2012=if_else(rev2012>=1, 1, 0), rev2013=if_else(rev2013>=1, 1, 0),
         rev2017=if_else(rev2017>=1, 1, 0))


t = revocatorias %>%
  group_by(departamento) %>%
  tally()

R1997 = revocatorias %>%
  group_by(departamento, rev1997) %>%
  tally() %>%
  filter(rev1997==1) %>%
  select(!rev1997) %>%
  rename(R1997 = n)

R2001 = revocatorias %>%
  group_by(departamento, rev2001) %>%
  tally() %>%
  filter(rev2001==1) %>%
  select(!rev2001) %>%
  rename(R2001 = n)

R2004 = revocatorias %>%
  group_by(departamento, rev2004) %>%
  tally() %>%
  filter(rev2004==1) %>%
  select(!rev2004) %>%
  rename(R2004 = n)

R2005 = revocatorias %>%
  group_by(departamento, rev2005) %>%
  tally() %>%
  filter(rev2005==1) %>%
  select(!rev2005) %>%
  rename(R2005 = n)

R2008 = revocatorias %>%
  group_by(departamento, rev2008) %>%
  tally() %>%
  filter(rev2008==1) %>%
  select(!rev2008) %>%
  rename(R2008 = n)

R2009 = revocatorias %>%
  group_by(departamento, rev2009) %>%
  tally() %>%
  filter(rev2009==1) %>%
  select(!rev2009) %>%
  rename(R2009 = n)

R2012 = revocatorias %>%
  group_by(departamento, rev2012) %>%
  tally() %>%
  filter(rev2012==1) %>%
  select(!rev2012) %>%
  rename(R2012 = n)

R2013 = revocatorias %>%
  group_by(departamento, rev2013) %>%
  tally() %>%
  filter(rev2013==1) %>%
  select(!rev2013) %>%
  rename(R2013 = n)



R2017 = revocatorias %>%
  group_by(departamento, rev2017) %>%
  tally() %>%
  filter(rev2017==1) %>%
  select(!rev2017) %>%
  rename(R2017 = n)

A = merge(t, R1997, by.x = "departamento", by.y = "departamento", all.x = T)
B = merge(A, R2001, by.x = "departamento", by.y = "departamento", all.x = T)
C = merge(B, R2004, by.x = "departamento", by.y = "departamento", all.x = T)
D = merge(C, R2005, by.x = "departamento", by.y = "departamento", all.x = T)
E = merge(D, R2008, by.x = "departamento", by.y = "departamento", all.x = T)
F1 = merge(E, R2009, by.x = "departamento", by.y = "departamento", all.x = T)
G = merge(F1, R2012, by.x = "departamento", by.y = "departamento", all.x = T)
H = merge(G, R2013, by.x = "departamento", by.y = "departamento", all.x = T)
regiones = merge(H, R2017, by.x = "departamento", by.y = "departamento", all.x = T)

regiones[,c(2:11)]=lapply(regiones[,c(2:11)],as.numeric)


sum (regiones[ , 11], na.rm = T)

porcentajeR =regiones %>%
  mutate(R1997 = replace(R1997, which(is.na(R1997)), 0), 
                  R2001 = replace(R2001, which(is.na(R2001)), 0),
                  R2004 = replace(R2004, which(is.na(R2004)), 0),
                  R2005 = replace(R2005, which(is.na(R2005)), 0),
                  R2008 = replace(R2008, which(is.na(R2008)), 0),
                  R2009 = replace(R2009, which(is.na(R2009)), 0),
                  R2012 = replace(R2012, which(is.na(R2012)), 0),
                  R2013 = replace(R2013, which(is.na(R2013)), 0),
                  R2017 = replace(R2017, which(is.na(R2017)), 0)) %>%
  mutate(P1997 = round(R1997/n*100, digits = 2), P2001 = round(R2001/n*100, digits = 2),
         P2004 = round(R2004/n*100, digits = 2), P2005 = round(R2005/n*100, digits = 2),
         P2008 = round(R2008/n*100, digits = 2), P2009 = round(R2009/n*100, digits = 2),
         P2012 = round(R2012/n*100, digits = 2), P2013 = round(R2013/n*100, digits = 2),
         P2017 = round(R2017/n*100, digits = 2)) %>%
  select(departamento, P1997, P2001, P2004, P2005, P2008, P2009, P2012, P2013, P2017)

write.csv(porcentajeR,"PorcentajeRevocatorias.csv")

library(tableHTML)
#create an html table 
tableHTML(porcentajeR)

#and to export in a file
write_tableHTML(tableHTML(porcentajeR), file = 'PorcentajeRevocatorias.html')

## CLUSTERS
# Cluster particionante
# coleccion
library(htmltab)
demolink = "https://en.wikipedia.org/wiki/Democracy_Index"
demopath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]"
demo<- htmltab(doc = demolink, which =demopath)

# limpieza
library(stringr)
library(magrittr)

names(demo)= str_split(names(demo),">>",simplify = T)[,1]%>%gsub('\\s','',.)

table(demo$Country)

demo[,-c(1,8,9)]=lapply(demo[,-c(1,8,9)], trimws,whitespace = "[\\h\\v]") # no blanks

# preparación
demo=demo[,-c(1)] #sin Rank
demo[,-c(1,8,9,10)]=lapply(demo[,-c(1,8,9,10)], as.numeric) # a numerico

demo$Country= str_split(demo$Country," ",n=2, simplify = T)[,1]
# veamos que tenemos:
str(demo)


row.names(demo)=demo$Country

# alternativa a complete.cases:
demo=na.omit(demo)
