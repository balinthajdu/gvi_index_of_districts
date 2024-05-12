# Working Directory be�ll�t�sa:
setwd("C:/Users/B�lint/OneDrive/Dokumentumok/BCE 2023-24/�konometria/Beadand�")

# A dolgozat ISO-8859-2 k�dol�sban �r�dott! A kommentek csak �gy olvashat�ak rendesen!

# A dolgozatban haszn�lt �sszes Package �s K�nyvt�r:

# Az �sszes csomag let�lt�se (ha m�g nem lenne meg):
# install.packages("readxl")
# install.packages("magrittr") 
# install.packages("dplyr")
# install.packages("naniar")
# install.packages("corrplot")
# install.packages("car")
# install.packages("skedastic")
# install.packages("lmtest")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("psych")
# install.packages("ggplot2")

# K�nyvt�rak:
library(readxl)
library(dplyr)
library(naniar)
library(magrittr)
library(corrplot)
library(car)
library(skedastic)
library(lmtest)
library(sf)
library(leaflet)
library(psych)
library(ggplot2)

# Adatok beimport�l�sa:
jarasok <- read_excel("GVI_17_komplex-mutat�.xlsx", 
                      sheet = "alapadatok_2019", col_types = c("text", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric"), skip = 0)

View(jarasok)
str(jarasok) 

# Beimport�lt adatb�zis m�dos�t�sa
jarasok <- jarasok[2:nrow(jarasok),]#els� sorban az oszlop sz�ma volt, arra nincs sz�ks�g
jarasok <- as.data.frame(jarasok)
str(jarasok)# az adatt�pusok rendben vannak
summary(jarasok)
gg_miss_var(jarasok)
which(is.na(jarasok))
keresett <- jarasok %>% filter(is.na(JFM_19))#Polg�rdi j�r�sr�l nincs adat, ennek oka, hogy id�k�zben megsz�nt: mivel m�r nem l�tezik �s csak egy megfigyel�sr�l van sz�, imput�l�s helyett t�tlni �rdemes
jarasok <- jarasok[jarasok$N�V != "Polg�rdi",]
gg_miss_var(jarasok)#most m�r minden rendben hi�nyz� adatokat tekintve

# Adatb�zis m�dos�t�sa: A kezdeti adatb�zisb�l 12 v�ltoz� kiv�laszt�sa, amellyel dolgozni szeretn�nk (10 magyar�z�v�ltoz� + 1 eredm�nyv�ltoz� + J�r�snevek)
summary(szelekcios_modell <- lm(JFM_19~.-N�V, data = jarasok))#p-�rt�kek �s logikus gondolkod�s alapj�n tartsunk meg 10 magyar�z�v�ltoz�t + n�v + fejletts�gi mutat�
jarasok <- jarasok[,c(1, 5, 6, 7, 10, 11, 12, 15, 17, 18, 21, 24 )]

# �kezetes elnevez�sek elimin�l�sa + R�videbb v�ltoz�nevek a k�nyebb kezelhet�s�g �rdek�ben:
jarasok <- jarasok %>%
  rename(nev = N�V)

jarasok <- jarasok %>%
  rename(jfm = JFM_19)

jarasok <- jarasok %>%
  rename(kisker_boltok = "1000 �lland� lakosra jut� kiskereskedelmi boltok sz�ma, 2017")

jarasok <- jarasok %>%
  rename(epitett_lakasok = "�p�tett lak�sok sz�ma (db) a lak�sok ar�ny�ban, 2017"  )

jarasok <- jarasok %>%
  rename(szennyviz_csatlakozas = "A k�z�zemi szennyv�zgy�jt�-h�l�zatba (k�zcsatornah�l�zatba) bekapcsolt lak�sok ar�nya, 2017" )

jarasok <- jarasok %>%
  rename(legkozelebbi_megyeszkh = "A legk�zelebbi megyesz�khely el�rhet�s�ge, 2012" )

jarasok <- jarasok %>%
  rename(hulladek_mennyiseg = "A lakoss�gt�l elk�l�n�tett gy�jt�ssel elsz�ll�tott telep�l�si hullad�k (tonna) 1000 �lland� lakosra vet�tve" )

jarasok <- jarasok %>%
  rename(allaskeresok_aranya = "Nyilv�ntartott �ll�skeres�k ar�nya, 2017")

jarasok <- jarasok %>%
  rename(villamosenergia = "1000 �lland� lakosra jut� h�ztart�sok r�sz�re szolg�ltatott villamosenergia mennyis�ge, 2017"  )

jarasok <- jarasok %>%
  rename(telepulesek_120felett = "120 f�/km2 n�ps�r�s�g feletti telep�l�seken lak�k ar�nya, 2017"  )

jarasok <- jarasok %>%
  rename(halalozas = "1000 lakosra jut� hal�loz�sok sz�ma, 2017" )

jarasok <- jarasok %>%
  rename(fiatalodasi_index = "Fiatalod�si index (0-18/60-X �ves n�pess�g ar�nya, 2017"  )

# Ellen�rz�s:
str(jarasok)
View(jarasok)
summary(szelekcios_modell <- lm(jfm~.-nev, data = jarasok)) 
# A r�videbb v�ltoz�nevekkel, jobban �tl�that�. Szuper.

# Outliersz�r�s:
175*0.01 # 1,75, sz�val 1-2 megfigyel�s sz�rhet� ki, ha 1% a t�r�shat�r az outliersz�r�sn�l.
175*0.03 # 5,25, sz�val 5-6 megfigyel�s sz�rhet� ki, ha 3% a maxim�lis t�r�shat�r az outliersz�r�sn�l.
summary(jarasok)
par(mar = c(2, 2, 2, 2))
par(mfrow=c(5,1))
boxplot(jarasok$epitett_lakasok)
boxplot(jarasok$kisker_boltok)
boxplot(jarasok$legkozelebbi_megyeszkh)
boxplot(jarasok$szennyviz_csatlakozas)
boxplot(jarasok$hulladek_mennyiseg)

par(mfrow=c(5,1))
boxplot(jarasok$villamosenergia)
boxplot(jarasok$halalozas)
boxplot(jarasok$telepulesek_120felett)
boxplot(jarasok$fiatalodasi_index)
boxplot(jarasok$allaskeresok_aranya)
dev.off()
# Felfedezhet� 1-2 kil�g� �rt�k, azonban l�tni kell, hogy Magyarorsz�gra er�s orsz�gon bel�li fejletts�gi egyenl�tlens�g jellemz�
# Azaz az adatok k�nnyen lehetnek helyesek, gondoljunk csak a Budapest-fej�s�gre
# Nem besz�lve arr�l, hogy k�zvetetten a KSH-t�l sz�rmaznak az adatok, aminek ugyan a m�dszertan�ba id�nk�nt bele lehet k�tni,de egy megb�zhat� int�zm�nynek sz�m�t
# Ezek k�vetkezm�ny�ben nem sz�r�nk ki egyetlen adatot sem �s megtartjuk a 175 megfigyel�s�nket.

# Le�r� statisztika:
summary(jarasok)
describe(jarasok[,2:12]) # Minden mennyis�gi v�ltoz�ra (kiv�ve a j�r�snevekre.)
describe(jarasok[,12])

# Alapmodell: 10 magyar�z�v�ltoz� + 1 eredm�nyv�ltoz�: Regresszi� + Multikollinearit�s + Heteroszkedaszticit�s + RESET-teszt
# Line�ris regresszi�:
summary(szelekcios_modell <- lm(jfm~.-nev, data = jarasok))
# R-n�gyzet: 93,61% - Nagyon magas magyar�z�er�.
# Korrig�lt R-n�gyzet: 92,92%
# Glob�lis F-pr�ba: < 2.2e-16 - A modell szignifik�ns a sokas�gban, azaz a mint�n k�v�li vil�gban.

# Multikollinearit�s vizsg�lata:
korrel0 <- cor(jarasok[,2:(ncol(jarasok)-2)])
corrplot(korrel0, method="number") # Nincs egzakt multikollinearit�s
corrplot(korrel0, method="color") # �gy tal�n jobban �tl�that�.

vif(szelekcios_modell) # VIF mutat�k alapj�n nem �rdemes f�komponenselemz�ssel kezdeni, a modellszelekci� ut�n �rdemes �jra megn�zni.
# Mindegyik VIF mutat� �rt�ke 3 alatt van.

# Heteroszkedaszticit�s:
jarasok$hibanegyzet <- szelekcios_modell$residuals^2
ggplot(jarasok, aes(x=jfm, y=hibanegyzet)) + geom_point()
jarasok <- subset(jarasok, select = -hibanegyzet) # Hiban�gyzet kiv�tele, csak az egyszer� �br�zol�s miatt kellett.
str(jarasok) # Ellen�rz�s. Kiszedt�k. Szuper.

ks.test(szelekcios_modell$residuals, "pnorm") # Ez alapj�n �rdemes a Koenker korrekci�zott Breusch-Pagan tesztet alkalmazni.
bptest(szelekcios_modell, studentize = TRUE) # 5%-on m�r egy�rtelm�en heteroszkedasztikusnak mondhat� a szelekcios__modell.

coeftest(szelekcios_modell)
coeftest(szelekcios_modell, vcov = hccm(szelekcios_modell))
# Ezek alapj�n �rdemes lesz kihagyni a fiatalodasi_index v�ltoz�t, hacsak egy interakci�s v�ltozat�t nem �rdemes beletenni

# RESET-teszt:
resettest(szelekcios_modell) # p-�rt�k: 2.733e-08 - A modell nem j�l specifik�lt.

# Transzform�ci�s lehet�s�gek detekt�l�sa: Hisztogram + Pontdiagram + Interakci�k

# R�gi� min�s�gi v�ltoz�val az adatb�zis b�v�t�se:
regio_jaras_abc <- read_excel("regio_jaras_abc.xlsx") # A j�r�sok abc sorrendben vannak.
# Az adatb�zisunkban is abc sorrendben vannak a j�r�sok, sz�val el�g csak hozz�adni a r�gi� oszlopot az adatb�zishoz.
regio_jaras_abc <- as.data.frame(regio_jaras_abc)
jarasok$regio <- as.factor(regio_jaras_abc[,2])
str(jarasok) # Faktor 7 kateg�ri�val, pont amennyi r�gi� van.
levels(jarasok$regio) # D�l-Alf�ld r�gi� a viszony�t�si alap, ez legyen K�z�p-Magyarorsz�g, mert ez a k�zpont, itt van Budapest is �s elt�r�sek alapj�n is j� v�laszt�snak t�nik a t�bbi r�gi�hoz k�pest
boxplot(jarasok$jfm ~ jarasok$regio)
jarasok$regio <- relevel(jarasok$regio, ref ='K�z�p-Magyarorsz�g') 

# Mennyis�gi v�ltoz�k eloszl�s�nak vizsg�lata hisztogrammon:
str(jarasok)
hist(jarasok$jfm) # Eredm�nyv�ltoz�nk, norm�lis eloszl�st k�vet. Nem kell logaritmiz�lni.
hist(jarasok$kisker_boltok) # Enyh�n balra ferde, ha a kilog� �rt�k nem lenne, akkor norm�lis eloszl�st k�vetne. Lehet logaritmiz�lni.
hist(log(jarasok$kisker_boltok)) # El�g sz�pen norm�lis eloszl�st k�vet.
hist(jarasok$epitett_lakasok) # Er�sen balra, ferde jobbra elny�l�. Lehet logaritmiz�lni.
hist(log(jarasok$epitett_lakasok)) # �gy m�r k�zel�t a norm�lis eloszl�shoz.
hist(log(jarasok$epitett_lakasok+0.001)) # A modellben majd konstanssal kell korrig�lni a 0 �rt�kek miatt
hist(jarasok$legkozelebbi_megyeszkh) # Nagyon enyh�n balra ferde, jobbra elnyul�. Nem kell logaritmiz�lni
hist(jarasok$szennyviz_csatlakozas) # Nagyon enyh�n jobbra ferde, balra elnyul�. Nem kell logaritmiz�lni, az nem seg�tene.
hist(jarasok$hulladek_mennyiseg) # K�zepesen balar ferde, jobbra elnyul�. Lehet logaritmiz�lni.
hist(log(jarasok$hulladek_mennyiseg)) # �gy kicsivel jobban k�vet norm�lis eloszl�st.
hist(log(jarasok$hulladek_mennyiseg+0.001)) # A modellben majd konstanssal kell korrig�lni a 0 �rt�kek miatt.
hist(jarasok$villamosenergia) # Szinte norm�lis eloszl�st k�vet. Nem kell logaritmiz�lni.
hist(jarasok$halalozas) # Szinte norm�lsi eloszl�st k�vet. Nem kell logaritmiz�lnom.
hist(jarasok$telepulesek_120felett) # Az els� oszt�lyk�zt lesz�m�tva egyenletes �rt�kek. Nem kell logaritmiz�lni, az nem seg�tene
hist(jarasok$fiatalodasi_index) # Kicsit balra ferde, jobbra elnyul�. Lehet logaritmiz�lni.
hist(log(jarasok$fiatalodasi_index)) # �gy val�ban jobban k�veti a norm�lis eloszl�st.
hist(jarasok$allaskeresok_aranya) # Kicsit balra ferde, jobbra elnyul�. Lehet logaritmiz�lni.
hist(log(jarasok$allaskeresok_aranya)) # Egy kicsit tal�n jobban k�veti a norm�lis eloszl�st. Mindkett� j� lehet, preferenci�n m�lik.
# �sszegz�s: 5 v�ltoz�t lehetne logaritmiz�lni, ezek a k�vetkez�ek:
# A v�ltoz�k: kisker_boltok, epitett_lakasok, hulladek_mennyiseg, allaskeresok_aranya, fiatalosod�si_index
# Ezeket ellen�rizz�k le pontdiagramokon is.

# Mennyis�gi v�ltoz�p�rok tov�bbi vizsg�lata pontdiagramon:
ggplot(data = jarasok, aes(x = kisker_boltok, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A pontok nem illeszkednek a legjobban a line�rishoz, lehet t�nyleg jav�tana a logaritmiz�l�s.

ggplot(data = jarasok, aes(x = log(kisker_boltok), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Igen, a logaritmiz�l�s val�ban seg�t. Bele lehet rakni, de nem musz�j.

ggplot(data = jarasok, aes(x = epitett_lakasok , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") 

ggplot(data = jarasok, aes(x = log(epitett_lakasok) , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Logaritmiz�l�ssal jobban illeszkedik a line�ris egyeneshez.

ggplot(data = jarasok, aes(x = log(epitett_lakasok+0.001) , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A modellben majd konstanssal kell korrig�lni a 0 �rt�kek miatt.

ggplot(data = jarasok, aes(x = legkozelebbi_megyeszkh, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Itt nem tudunk jobb illeszked�st el�rni.

ggplot(data = jarasok, aes(x = szennyviz_csatlakozas, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ez egy el�g sz�p illeszked�s.

ggplot(data = jarasok, aes(x = hulladek_mennyiseg, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Az illeszked�sen lehetne jav�tani egy logaritmiz�l�ssal.

ggplot(data = jarasok, aes(x = log(hulladek_mennyiseg), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # L�that�an jobb lett a kapcsolat. 

ggplot(data = jarasok, aes(x = log(hulladek_mennyiseg+0.001), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A modellben majd konstanssal kell korrig�lni a 0 �rt�kek miatt.

ggplot(data = jarasok, aes(x = villamosenergia, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ez egy el�g j� kapcsolat. Nem kell ezt tov�bb jav�tani.

ggplot(data = jarasok, aes(x = halalozas, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ez is el�g sz�p kapcsolat, itt is maradhat a line�ris tag.

ggplot(data = jarasok, aes(x = telepulesek_120felett, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Tal�n egy parabola l�that� jobbra elny�jtva. Lehet a n�gyzetes tag jobban illeszkedik

ggplot(data = jarasok, aes(x = I(telepulesek_120felett^2), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Egy minim�lisan tal�n siker�lt jav�tani a kapcsolaton.

ggplot(data = jarasok, aes(x = fiatalodasi_index , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A hisztogram alapj�n ezt lehetne logaritmiz�lni.

ggplot(data = jarasok, aes(x = log(fiatalodasi_index) , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # H�t annyit tal�n nem javult a helyzet. Tal�n egy minim�lisan.

ggplot(data = jarasok, aes(x = allaskeresok_aranya, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # R�n�z�sre nincsen baj az illeszked�ssel.

ggplot(data = jarasok, aes(x = log(allaskeresok_aranya), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # R�n�z�sre egy minim�lisan tal�n javul.

# �sszegz�s: 5 logaritmiz�lt v�ltoz�, 1 kvadratikus tag bev�tele indokolhat� lenne.
# Log: kisker_boltok, epitett_lakasok, hulladek_mennyiseg, allaskeresok_aranya, fiatalosod�si_index
# Kvadratikus: telepulesek_120felett

# Interakci�k vizsg�lata
# 1 min�s�gi v�ltoz�nk van, sz�val minden kombin�ci� a magyar�z�v�ltoz�kkal:
str(jarasok)
ggplot(data =jarasok, aes (x = kisker_boltok , y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Nincsenek drasztikus meredeks�gk�l�nbs�gek. Nem musz�j interakci�.

ggplot(data =jarasok, aes (x = epitett_lakasok, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Itt a Nyugat-Dun�nt�l r�gi� meredeks�ge jelent�sen elt�r a t�bbi r�gi�t�l. Lehet interakci�.

ggplot(data =jarasok, aes (x = legkozelebbi_megyeszkh, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Az egy helyen l�v� r�gi�csoportok meredeks�gei t�rnek el egym�st�l. Lehet interakci�, de nem jelent�s a meredeks�gk�l�nbs�g.

ggplot(data =jarasok, aes (x = szennyviz_csatlakozas, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # M�s tengelymetszetek. Nagyon minim�lis meredeks�gk�l�nbs�gek. Nem kell interakci�. 

ggplot(data =jarasok, aes (x = hulladek_mennyiseg, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # A 2 K�z�p r�gi� meredekebb, mint a t�bbi. Lehet interakci�.

ggplot(data =jarasok, aes (x = villamosenergia , y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # M�s tengelymetszetek. Nincsen jelent�s meredeks�gk�l�nbs�g. Nem kell interakci�.

ggplot(data =jarasok, aes (x = halalozas, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # M�s tengelymetszetek. Minim�lis meredeks�gk�l�nbs�gek. Nem kell interakci�.

ggplot(data =jarasok, aes (x = telepulesek_120felett, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # M�s tengelymetszetek. Minim�lis meredeks�gk�l�nbs�gek. Nem kell interakci�.

ggplot(data =jarasok, aes (x = fiatalodasi_index, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # M�s tengelymetszetek. Eddigi legjelent�sebb meredeks�gk�l�nbs�gek. Kell az interakci�.

ggplot(data =jarasok, aes (x = allaskeresok_aranya, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Szinte azonos tengelymetszetek. Nagyon minim�lis meredeks�gk�l�nbs�g. Nem kell interakci�.

# �sszegz�s: 4 interakci�t lehet belevenni
# Ezek a k�vetkez�ek: fiatalosodasi_index*regio, hulladek_mennyiseg*regio, epitett_lakasok*regio, legkozelebbi_megyeszkh*regio

# Modell�p�t�s:

# Modell_1: szelekci�s_modell-t a regi� v�ltoz�val b�v�tett�k
modell_1 <- lm(jfm ~ . - nev, data = jarasok)
summary(modell_1)
resettest(modell_1) # A modell nem tekinthet� j�l specifik�ltnak, mert a p-�rt�k = 9.292e-08.

# Modell_2: Transzform�ci�k bev�tele: 5 logaritmiz�lt tag + 1 Kvadratikus tag
modell_2 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
               + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
               +I(telepulesek_120felett^2) + log(fiatalodasi_index) + log(allaskeresok_aranya) + regio, data = jarasok)
summary(modell_2)
resettest(modell_2) # A modell nem j�l specifik�lt, mert a p-�rt�k = 5.595e-08.

# modell_1 �s modell_1 �sszehasonl�t�sa:
# 9.292e-08 > 5.595e-08, azaz az modell_1 jobban specifik�ltnak tekinthet�, mint a transzform�lt tagokat tartalmaz�
AIC(modell_1, modell_2)
BIC(modell_1, modell_2)
#AIC �s BIC szerint is jobb a modell_1

# Modell_3: A modell_2-b�l a fiatalosod�si_index kiv�tele, mert az nem szignifik�ns v�ltoz�.
modell_3 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
               + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
               +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio, data = jarasok)
summary(modell_3)
resettest(modell_3) # A modell nem j�l speficik�lt, mert a p-�rt�k = 5.167e-08

# modell_2 �s modell_3 �sszehasonl�t�sa:
AIC(modell_3, modell_2)
BIC(modell_3, modell_2) # B�r valamivel rosszabbul specifik�lt, az inform�ci�s krit�riumok szerint jobb a modell_3, �rdemes volt elhagyni a fiatalod�si indexet.
anova(modell_2, modell_3) # anova szerint is �rdemes volt kihagyni

# Interakci�k: itt m�r vissza lehet venni a fiatalosod�si indexet, ha szignifik�ns az interakci�ja
# K�vetkez� interakci�kat fogjuk vizsg�lni: fiatalodasi_index*regio, hulladek_mennyiseg*regio, epitett_lakasok*regio, legkozelebbi_megyeszkh*regio

# modell_int1: Modell_3-hoz mind a 4 interakci� hozz�ad�sa (Ez�ltal a fiatalosod�si index is visszaker�l.)
modell_int1 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + regio*fiatalodasi_index + hulladek_mennyiseg*regio + epitett_lakasok*regio + legkozelebbi_megyeszkh*regio, data = jarasok)
summary(modell_int1)
resettest(modell_int1)# A modell nem j�l specifik�lt, mert a p-�rt�k = 1.51e-05.

# modell_3 �s modell_int1 �sszehasonl�t�sa:
AIC(modell_3, modell_int1)
BIC(modell_3, modell_int1) 
anova(modell_3, modell_int1)
# Az �sszes interakci�t belerakva biztosan rosszabb lett a modell, mint n�lk�l�k

# Modell_int2: a modell_int1-b�l a fiatalosod�si index*r�gi� interakci� kiv�tele, �gy a fiatalosod�si index v�ltoz�t v�gleg kiszedj�k.
modell_int2 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + hulladek_mennyiseg*regio + epitett_lakasok*regio + legkozelebbi_megyeszkh*regio, data = jarasok)
summary(modell_int2)
resettest(modell_int2) # A modell nem j�l specifik�lt, mert p-�rt�k= 1.642e-05. De p-�rt�k magasabb lett valamivel.

# modell_3 �s modell_int2 �sszehasonl�t�sa:
AIC(modell_3, modell_int2)
BIC(modell_3, modell_int2) 
anova(modell_3, modell_int2) # M�g mindig nem �rte meg az interakci�kat bevonni.

# modell_int3: modell_int2-b�l a legkozelebbi_megyeszkh*regio interakci� elhagy�sa
modell_int3 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + hulladek_mennyiseg*regio + epitett_lakasok*regio, data = jarasok)
summary(modell_int3)
resettest(modell_int3) # A modell nem j�l specifik�lt, mert a p-�rt�k = 1.339e-06. p-�rt�k cs�kkent valamivel

# modell_3 �s modell_int3 �sszehasonl�t�sa:
AIC(modell_3, modell_int3)
BIC(modell_3, modell_int3) 
anova(modell_3, modell_int3) # valamivel k�zelebb vagyunk, de m�g mindig modell_3-at prefer�ljuk.

# modell_int4: modell_int3-b�l a hulladek_mennyiseg*regio interakci� elhagy�sa
modell_int4 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + epitett_lakasok*regio, data = jarasok)
summary(modell_int4)
resettest(modell_int4) # A modell nem j�l specifik�lt, mert a p-�rt�k = 3.962e-08. p-�rt�k cs�kkent sajnos, rosszabbul specifik�ltnak tekinthet� �gy.

# modell_3 �s modell_int4 �sszehasonl�t�sa:
AIC(modell_3, modell_int4)
BIC(modell_3, modell_int4) 
anova(modell_3, modell_int4) # BIC �s a Wald-teszt szerint is �rdemes bevonnia. mdoellbe az epitett_lakasok*regio interakci�j�t.

# �sszegz�s:
# A modell_int4 a vegso modell. Mi ezt a modellt prefer�ljuk.
# K�sz�ts�k el a v�gs� modell alapj�n a v�gs� adatb�zist is: 
vegso_jarasok <- jarasok
vegso_jarasok$kisker_boltok <- log(jarasok$kisker_boltok)
vegso_jarasok$epitett_lakasok <- log(jarasok$epitett_lakasok+0.001)
vegso_jarasok$hulladek_mennyiseg <- log(jarasok$hulladek_mennyiseg+0.001)
vegso_jarasok$allaskeresok_aranya <- log(jarasok$allaskeresok_aranya)
vegso_jarasok$telepulesek_120felett <- jarasok$telepulesek_120felett^2

# V�gs� modell: Multikollinearit�s + Heteroszkedaszticit�s

# Heteroszkedaszticit�s:
vegso_jarasok$hibanegyzet <- modell_int4$residuals^2
ggplot(vegso_jarasok, aes(x=jfm, y=hibanegyzet)) + geom_point()
vegso_jarasok <- subset(vegso_jarasok, select = -hibanegyzet) # Hiban�gyzet kiv�tele, csak az egyszer� �br�zol�s miatt kellett.
str(vegso_jarasok)
ks.test(modell_int4$residuals, "pnorm") # Ez alapj�n �rdemes a Koenker korrekci�zott Breusch-Pagan tesztet alkalmazni.
bptest(modell_int4, studentize = TRUE)# A p-�rt�k = 0.1469, sz�val homoszkedasztikus a modell. Szuper.

white_alap <- white(modell_int4, interactions = TRUE)
white_alap$p.value

# Multikollonearit�s viszg�lata �s kezel�se:
str(vegso_jarasok[,2:(ncol(vegso_jarasok)-2)]) # Nincs benne a JFM, a r�gi� �s a j�r�snevek sem. Ez kell nek�nk.
korrel0 <- cor(jarasok[,2:(ncol(vegso_jarasok)-2)])
corrplot(korrel0, method="number")
corrplot(korrel0, method="color") # Nem l�that� er�s kapcsolat.

vif(modell_int4)
# Minden VIF mutat� �rt�ke 5 alatt van, �gy sz� sincs sem zavar�, sem k�ros multikollinearit�sr�l.
# Ezek al�l csak a r�gi� �s az �p�tett lak�sok (�s term�szetesen ezek interakci�ja) a kiv�tel, de ezt az interakci� okozta, sz�val ezzel nincsen probl�ma.
# Ugyan VIF mutat�k alapj�n nem �rdemes f�komponenselemz�st v�gezni, n�zz�k, meg, milyen f�komponensek lenn�nek kialak�that�k.
# Interakci� el�tti VIF mutat�k:
vif(modell_3) # Itt minden VIF mutat� a zavar� 5-�s �rt�k alatt van, sz�val az interakci� miatt lesznek csak zavar� �rt�kek. Ez nem baj.
# Ennek k�vetkezm�ny�ben nem kell kezelni ezt a fajta mesters�ges multikollinearit�st, amit az interakci�val okoztunk.
# M�sik szempont pedig az, hogy a Gy�k�s VIF mutat� �rt�kei nagyon k�zel vannak az 1-hez, ami szint�n arra utal, hogy nincsen multikollinearit�s.

str(vegso_jarasok[,2:(ncol(vegso_jarasok)-2)]) # Nincs benne a JFM, a r�gi� �s a j�r�snevek sem. Ez kell nek�nk.
pca <- prcomp(vegso_jarasok[,2:(ncol(vegso_jarasok)-2)], center = TRUE, scale=TRUE)
summary(pca)# Az els� h�rom f�komponens felel meg a sz�r�s/variancia > 1 szelekci�s felt�telnek

vegso_jarasok_pca <- cbind(vegso_jarasok, pca$x[,1:3])
str(vegso_jarasok_pca)

korrel <- cor(vegso_jarasok_pca[,c(2:11, 14:16)])
corrplot(korrel, method="number")
corrplot(korrel, method="color")

# El�rejelz�s:
predict_data <- data.frame(
  kisker_boltok = median(jarasok$kisker_boltok),  
  epitett_lakasok = median(jarasok$epitett_lakasok),
  legkozelebbi_megyeszkh = median(jarasok$legkozelebbi_megyeszkh),
  szennyviz_csatlakozas = median(jarasok$szennyviz_csatlakozas),
  hulladek_mennyiseg = median(jarasok$hulladek_mennyiseg),
  villamosenergia = median(jarasok$villamosenergia),
  halalozas = median(jarasok$halalozas),
  telepulesek_120felett = median(jarasok$telepulesek_120felett),
  allaskeresok_aranya = median(jarasok$allaskeresok_aranya),
  regio = "K�z�p-Magyarorsz�g"
)

elorejelzes <- predict(modell_int4, newdata = predict_data)
print(elorejelzes)

# Interakci�s t�rk�p a j�r�sokra:
# A t�rk�pet HTML f�jlk�nt csatoljuk �s ott interakt�van lehet a j�r�sok fejletts�gi mutat�j�t megn�zni.
# Innent�l csak ennek a lek�d�l�sa l�that�.
# A k�dok lefuttat�s�hoz sz�ks�g van a ZIP-file-ra.

#--------T�rk�p cucc----------------

# K�l�n adatb�zis l�trehoz�sa:
jarasok_terkep <- jarasok

# a S�sdi j�r�s az�ta Hegyh�ti lett, a shape f�jlban is �gy van, sz�val itt is �tnevezz�k:
jarasok_terkep[129,1] <- "Hegyh�ti"
jarasok_terkep <- jarasok_terkep[order(jarasok_terkep$nev),]

#Beolvas�s �s megjav�t�s:
shape <- st_read("Magyar SHP/hun_admbnda_adm2_osm_20220720.shp")
shape <- st_make_valid(shape)

koord <- st_centroid(shape)
koord <- as.data.frame(st_coordinates(koord))
koord$Jaras <- shape$ADM2_HU 

# Ker�letek �tkonvert�l�sa egys�gesen Budapestre
shape$Jaras_wBP <- shape$ADM2_HU
unique(shape$Jaras_wBP)
shape$Jaras_wBP[grepl("district", shape$Jaras_wBP)] <- "Budapest"

# Bet�rend:
shape <- shape[order(shape$Jaras_wBP),]

nev_BPk <- c(rep("Budapesti ker�letek",22), jarasok_terkep$nev)
nev_BPk <- nev_BPk[order(nev_BPk)]

# Betessz�k a sok Budapestet
shape$nev_BPk <- nev_BPk
shape <- merge(x=shape, y=jarasok_terkep, by.x="nev_BPk", by.y="nev", all.x=TRUE)

# Kvintilisek alapj�n bontjuk meg a fejletts�gi mutat�t
kvintilisek <- quantile(shape$jfm, probs = c(0,0.2,0.4,0.6,0.8,1))
Pali <- colorBin("YlOrRd", domain = shape$jfm, bins = kvintilisek)

leaflet(shape) %>% 
  addTiles() %>%
  addPolygons(color="white",
              opacity=1,
              dashArray = "3",
              weight = 2,
              highlightOptions = highlightOptions(weight=5,
                                                  color = "grey",
                                                  dashArray = "",
                                                  opacity = 0.7,
                                                  fillOpacity = 0.4),
              fillColor = ~Pali(jfm),
              fillOpacity = 0.7,
              popup = paste0(shape$nev_BPk,"<br>",
                             round(shape$jfm,3))) %>%
  addLegend(pal = Pali, values = ~shape$jfm, opacity = 0.7,
            position = "bottomright",
            title = "Fejletts�gi mutat�")

# K�sz a t�rk�p!



#l�bjegyzet, hagyd figyelmen k�v�l
ncol(jarasok)#23 magyar�z�v�ltoz�nk van, ez �ltal�ban szerencs� eset, azonban a dolgozat terjedelm�re val� tekintettel �rdemes lenne csak 10-12 megtartani
library(mRMRe)
szurt<-jarasok[,2:ncol(jarasok)] # v�lasszuk le a nem relev�ns v�ltoz�kat: n�v
szurt_mrmr<-mRMR.data(as.data.frame(szurt)) # alak�tsuk mrmr objektumm�r az adatb�zist
x<-solutions(mRMR.classic(data = szurt_mrmr, target_indices = c(23), feature_count = 10))
x[[1]] #list�t kapunk eredm�ny�l
# melyek ezek a v�ltoz�k?
colnames(szurt)[x[[1]]]
jarasok <- jarasok[,c(1,6, 9, 10, 13, 14, 16, 17, 19:21, 24)]
