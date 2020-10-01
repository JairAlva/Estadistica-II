library(htmltab)
library(stringr)
library(magrittr)

#Primera Base

url1 = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/223.html" 

gini = htmltab(doc = url1, 
              which = '//*[@id="fieldListing"]', #herramientas de desarrollador
              encoding = "UTF-8") 

names(gini)
names(gini)=c("Pais","Gini")

new_gini=separate(gini,Gini,into=c("Gini",'xtra1'), "[(]")

new_gini$xtra1=NULL

str(new_gini)

new_gini$Gini=as.numeric(new_gini$Gini)

summary(new_gini)

new_gini[!complete.cases(new_gini),]

#Segunda Base

url2 = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/225.html"

taxes = htmltab(doc = url2, 
               which = '//*[@id="fieldListing"]', #herramientas de desarrollador
               encoding = "UTF-8") 

names(taxes)
names(taxes)=c("Pais","Taxes")

taxes$Taxes = parse_number(taxes$Taxes)

taxes[!complete.cases(taxes),]
#Merging

final=merge(new_gini,taxes,by.x='Pais', by.y='Pais') 

final[!complete.cases(final),]
head(final)

attach(final)
plot(final$Gini,final$Taxes)
