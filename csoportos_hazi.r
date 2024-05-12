# Working Directory beállítása:
setwd("C:/Users/Bálint/OneDrive/Dokumentumok/BCE 2023-24/Ökonometria/Beadandó")

# A dolgozat ISO-8859-2 kódolásban íródott! A kommentek csak így olvashatóak rendesen!

# A dolgozatban használt összes Package és Könyvtár:

# Az összes csomag letöltése (ha még nem lenne meg):
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

# Könyvtárak:
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

# Adatok beimportálása:
jarasok <- read_excel("GVI_17_komplex-mutató.xlsx", 
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

# Beimportált adatbázis módosítása
jarasok <- jarasok[2:nrow(jarasok),]#elsõ sorban az oszlop száma volt, arra nincs szükség
jarasok <- as.data.frame(jarasok)
str(jarasok)# az adattípusok rendben vannak
summary(jarasok)
gg_miss_var(jarasok)
which(is.na(jarasok))
keresett <- jarasok %>% filter(is.na(JFM_19))#Polgárdi járásról nincs adat, ennek oka, hogy idõközben megszûnt: mivel már nem létezik és csak egy megfigyelésrõl van szó, imputálás helyett tötlni érdemes
jarasok <- jarasok[jarasok$NÉV != "Polgárdi",]
gg_miss_var(jarasok)#most már minden rendben hiányzó adatokat tekintve

# Adatbázis módosítása: A kezdeti adatbázisból 12 változó kiválasztása, amellyel dolgozni szeretnénk (10 magyarázóváltozó + 1 eredményváltozó + Járásnevek)
summary(szelekcios_modell <- lm(JFM_19~.-NÉV, data = jarasok))#p-értékek és logikus gondolkodás alapján tartsunk meg 10 magyarázóváltozót + név + fejlettségi mutató
jarasok <- jarasok[,c(1, 5, 6, 7, 10, 11, 12, 15, 17, 18, 21, 24 )]

# Ékezetes elnevezések eliminálása + Rövidebb változónevek a könyebb kezelhetõség érdekében:
jarasok <- jarasok %>%
  rename(nev = NÉV)

jarasok <- jarasok %>%
  rename(jfm = JFM_19)

jarasok <- jarasok %>%
  rename(kisker_boltok = "1000 állandó lakosra jutó kiskereskedelmi boltok száma, 2017")

jarasok <- jarasok %>%
  rename(epitett_lakasok = "Épített lakások száma (db) a lakások arányában, 2017"  )

jarasok <- jarasok %>%
  rename(szennyviz_csatlakozas = "A közüzemi szennyvízgyûjtõ-hálózatba (közcsatornahálózatba) bekapcsolt lakások aránya, 2017" )

jarasok <- jarasok %>%
  rename(legkozelebbi_megyeszkh = "A legközelebbi megyeszékhely elérhetõsége, 2012" )

jarasok <- jarasok %>%
  rename(hulladek_mennyiseg = "A lakosságtól elkülönített gyûjtéssel elszállított települési hulladék (tonna) 1000 állandó lakosra vetítve" )

jarasok <- jarasok %>%
  rename(allaskeresok_aranya = "Nyilvántartott álláskeresõk aránya, 2017")

jarasok <- jarasok %>%
  rename(villamosenergia = "1000 állandó lakosra jutó háztartások részére szolgáltatott villamosenergia mennyisége, 2017"  )

jarasok <- jarasok %>%
  rename(telepulesek_120felett = "120 fõ/km2 népsûrûség feletti településeken lakók aránya, 2017"  )

jarasok <- jarasok %>%
  rename(halalozas = "1000 lakosra jutó halálozások száma, 2017" )

jarasok <- jarasok %>%
  rename(fiatalodasi_index = "Fiatalodási index (0-18/60-X éves népesség aránya, 2017"  )

# Ellenõrzés:
str(jarasok)
View(jarasok)
summary(szelekcios_modell <- lm(jfm~.-nev, data = jarasok)) 
# A rövidebb változónevekkel, jobban átlátható. Szuper.

# Outlierszûrés:
175*0.01 # 1,75, szóval 1-2 megfigyelés szûrhetõ ki, ha 1% a tûréshatár az outlierszûrésnél.
175*0.03 # 5,25, szóval 5-6 megfigyelés szûrhetõ ki, ha 3% a maximális tûréshatár az outlierszûrésnél.
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
# Felfedezhetõ 1-2 kilógó érték, azonban látni kell, hogy Magyarországra erõs országon belüli fejlettségi egyenlõtlenség jellemzõ
# Azaz az adatok könnyen lehetnek helyesek, gondoljunk csak a Budapest-fejûségre
# Nem beszélve arról, hogy közvetetten a KSH-tól származnak az adatok, aminek ugyan a módszertanába idõnként bele lehet kötni,de egy megbízható intézménynek számít
# Ezek következményében nem szûrünk ki egyetlen adatot sem és megtartjuk a 175 megfigyelésünket.

# Leíró statisztika:
summary(jarasok)
describe(jarasok[,2:12]) # Minden mennyiségi változóra (kivéve a járásnevekre.)
describe(jarasok[,12])

# Alapmodell: 10 magyarázóváltozó + 1 eredményváltozó: Regresszió + Multikollinearitás + Heteroszkedaszticitás + RESET-teszt
# Lineáris regresszió:
summary(szelekcios_modell <- lm(jfm~.-nev, data = jarasok))
# R-négyzet: 93,61% - Nagyon magas magyarázóerõ.
# Korrigált R-négyzet: 92,92%
# Globális F-próba: < 2.2e-16 - A modell szignifikáns a sokaságban, azaz a mintán kívüli világban.

# Multikollinearitás vizsgálata:
korrel0 <- cor(jarasok[,2:(ncol(jarasok)-2)])
corrplot(korrel0, method="number") # Nincs egzakt multikollinearitás
corrplot(korrel0, method="color") # Így talán jobban átlátható.

vif(szelekcios_modell) # VIF mutatók alapján nem érdemes fõkomponenselemzéssel kezdeni, a modellszelekció után érdemes újra megnézni.
# Mindegyik VIF mutató értéke 3 alatt van.

# Heteroszkedaszticitás:
jarasok$hibanegyzet <- szelekcios_modell$residuals^2
ggplot(jarasok, aes(x=jfm, y=hibanegyzet)) + geom_point()
jarasok <- subset(jarasok, select = -hibanegyzet) # Hibanégyzet kivétele, csak az egyszerû ábrázolás miatt kellett.
str(jarasok) # Ellenõrzés. Kiszedtük. Szuper.

ks.test(szelekcios_modell$residuals, "pnorm") # Ez alapján érdemes a Koenker korrekciózott Breusch-Pagan tesztet alkalmazni.
bptest(szelekcios_modell, studentize = TRUE) # 5%-on már egyértelmûen heteroszkedasztikusnak mondható a szelekcios__modell.

coeftest(szelekcios_modell)
coeftest(szelekcios_modell, vcov = hccm(szelekcios_modell))
# Ezek alapján érdemes lesz kihagyni a fiatalodasi_index változót, hacsak egy interakciós változatát nem érdemes beletenni

# RESET-teszt:
resettest(szelekcios_modell) # p-érték: 2.733e-08 - A modell nem jól specifikált.

# Transzformációs lehetõségek detektálása: Hisztogram + Pontdiagram + Interakciók

# Régió minõségi változóval az adatbázis bõvítése:
regio_jaras_abc <- read_excel("regio_jaras_abc.xlsx") # A járások abc sorrendben vannak.
# Az adatbázisunkban is abc sorrendben vannak a járások, szóval elég csak hozzáadni a régió oszlopot az adatbázishoz.
regio_jaras_abc <- as.data.frame(regio_jaras_abc)
jarasok$regio <- as.factor(regio_jaras_abc[,2])
str(jarasok) # Faktor 7 kategóriával, pont amennyi régió van.
levels(jarasok$regio) # Dél-Alföld régió a viszonyítási alap, ez legyen Közép-Magyarország, mert ez a központ, itt van Budapest is és eltérések alapján is jó választásnak tûnik a többi régióhoz képest
boxplot(jarasok$jfm ~ jarasok$regio)
jarasok$regio <- relevel(jarasok$regio, ref ='Közép-Magyarország') 

# Mennyiségi változók eloszlásának vizsgálata hisztogrammon:
str(jarasok)
hist(jarasok$jfm) # Eredményváltozónk, normális eloszlást követ. Nem kell logaritmizálni.
hist(jarasok$kisker_boltok) # Enyhén balra ferde, ha a kilogó érték nem lenne, akkor normális eloszlást követne. Lehet logaritmizálni.
hist(log(jarasok$kisker_boltok)) # Elég szépen normális eloszlást követ.
hist(jarasok$epitett_lakasok) # Erõsen balra, ferde jobbra elnyúló. Lehet logaritmizálni.
hist(log(jarasok$epitett_lakasok)) # Így már közelít a normális eloszláshoz.
hist(log(jarasok$epitett_lakasok+0.001)) # A modellben majd konstanssal kell korrigálni a 0 értékek miatt
hist(jarasok$legkozelebbi_megyeszkh) # Nagyon enyhén balra ferde, jobbra elnyuló. Nem kell logaritmizálni
hist(jarasok$szennyviz_csatlakozas) # Nagyon enyhén jobbra ferde, balra elnyuló. Nem kell logaritmizálni, az nem segítene.
hist(jarasok$hulladek_mennyiseg) # Közepesen balar ferde, jobbra elnyuló. Lehet logaritmizálni.
hist(log(jarasok$hulladek_mennyiseg)) # Így kicsivel jobban követ normális eloszlást.
hist(log(jarasok$hulladek_mennyiseg+0.001)) # A modellben majd konstanssal kell korrigálni a 0 értékek miatt.
hist(jarasok$villamosenergia) # Szinte normális eloszlást követ. Nem kell logaritmizálni.
hist(jarasok$halalozas) # Szinte normálsi eloszlást követ. Nem kell logaritmizálnom.
hist(jarasok$telepulesek_120felett) # Az elsõ osztályközt leszámítva egyenletes értékek. Nem kell logaritmizálni, az nem segítene
hist(jarasok$fiatalodasi_index) # Kicsit balra ferde, jobbra elnyuló. Lehet logaritmizálni.
hist(log(jarasok$fiatalodasi_index)) # Így valóban jobban követi a normális eloszlást.
hist(jarasok$allaskeresok_aranya) # Kicsit balra ferde, jobbra elnyuló. Lehet logaritmizálni.
hist(log(jarasok$allaskeresok_aranya)) # Egy kicsit talán jobban követi a normális eloszlást. Mindkettõ jó lehet, preferencián múlik.
# Összegzés: 5 változót lehetne logaritmizálni, ezek a következõek:
# A változók: kisker_boltok, epitett_lakasok, hulladek_mennyiseg, allaskeresok_aranya, fiatalosodási_index
# Ezeket ellenõrizzük le pontdiagramokon is.

# Mennyiségi változópárok további vizsgálata pontdiagramon:
ggplot(data = jarasok, aes(x = kisker_boltok, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A pontok nem illeszkednek a legjobban a lineárishoz, lehet tényleg javítana a logaritmizálás.

ggplot(data = jarasok, aes(x = log(kisker_boltok), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Igen, a logaritmizálás valóban segít. Bele lehet rakni, de nem muszáj.

ggplot(data = jarasok, aes(x = epitett_lakasok , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") 

ggplot(data = jarasok, aes(x = log(epitett_lakasok) , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Logaritmizálással jobban illeszkedik a lineáris egyeneshez.

ggplot(data = jarasok, aes(x = log(epitett_lakasok+0.001) , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A modellben majd konstanssal kell korrigálni a 0 értékek miatt.

ggplot(data = jarasok, aes(x = legkozelebbi_megyeszkh, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Itt nem tudunk jobb illeszkedést elérni.

ggplot(data = jarasok, aes(x = szennyviz_csatlakozas, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ez egy elég szép illeszkedés.

ggplot(data = jarasok, aes(x = hulladek_mennyiseg, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Az illeszkedésen lehetne javítani egy logaritmizálással.

ggplot(data = jarasok, aes(x = log(hulladek_mennyiseg), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Láthatóan jobb lett a kapcsolat. 

ggplot(data = jarasok, aes(x = log(hulladek_mennyiseg+0.001), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A modellben majd konstanssal kell korrigálni a 0 értékek miatt.

ggplot(data = jarasok, aes(x = villamosenergia, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ez egy elég jó kapcsolat. Nem kell ezt tovább javítani.

ggplot(data = jarasok, aes(x = halalozas, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ez is elég szép kapcsolat, itt is maradhat a lineáris tag.

ggplot(data = jarasok, aes(x = telepulesek_120felett, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Talán egy parabola látható jobbra elnyújtva. Lehet a négyzetes tag jobban illeszkedik

ggplot(data = jarasok, aes(x = I(telepulesek_120felett^2), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Egy minimálisan talán sikerült javítani a kapcsolaton.

ggplot(data = jarasok, aes(x = fiatalodasi_index , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A hisztogram alapján ezt lehetne logaritmizálni.

ggplot(data = jarasok, aes(x = log(fiatalodasi_index) , y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Hát annyit talán nem javult a helyzet. Talán egy minimálisan.

ggplot(data = jarasok, aes(x = allaskeresok_aranya, y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ránézésre nincsen baj az illeszkedéssel.

ggplot(data = jarasok, aes(x = log(allaskeresok_aranya), y = jfm)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Ránézésre egy minimálisan talán javul.

# Összegzés: 5 logaritmizált változó, 1 kvadratikus tag bevétele indokolható lenne.
# Log: kisker_boltok, epitett_lakasok, hulladek_mennyiseg, allaskeresok_aranya, fiatalosodási_index
# Kvadratikus: telepulesek_120felett

# Interakciók vizsgálata
# 1 minõségi változónk van, szóval minden kombináció a magyarázóváltozókkal:
str(jarasok)
ggplot(data =jarasok, aes (x = kisker_boltok , y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Nincsenek drasztikus meredekségkülönbségek. Nem muszáj interakció.

ggplot(data =jarasok, aes (x = epitett_lakasok, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Itt a Nyugat-Dunántúl régió meredeksége jelentõsen eltér a többi régiótól. Lehet interakció.

ggplot(data =jarasok, aes (x = legkozelebbi_megyeszkh, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Az egy helyen lévõ régiócsoportok meredekségei térnek el egymástól. Lehet interakció, de nem jelentõs a meredekségkülönbség.

ggplot(data =jarasok, aes (x = szennyviz_csatlakozas, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Más tengelymetszetek. Nagyon minimális meredekségkülönbségek. Nem kell interakció. 

ggplot(data =jarasok, aes (x = hulladek_mennyiseg, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # A 2 Közép régió meredekebb, mint a többi. Lehet interakció.

ggplot(data =jarasok, aes (x = villamosenergia , y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Más tengelymetszetek. Nincsen jelentõs meredekségkülönbség. Nem kell interakció.

ggplot(data =jarasok, aes (x = halalozas, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Más tengelymetszetek. Minimális meredekségkülönbségek. Nem kell interakció.

ggplot(data =jarasok, aes (x = telepulesek_120felett, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Más tengelymetszetek. Minimális meredekségkülönbségek. Nem kell interakció.

ggplot(data =jarasok, aes (x = fiatalodasi_index, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Más tengelymetszetek. Eddigi legjelentõsebb meredekségkülönbségek. Kell az interakció.

ggplot(data =jarasok, aes (x = allaskeresok_aranya, y = jfm, color = regio)) +
  geom_point() +
  stat_smooth(method=lm) # Szinte azonos tengelymetszetek. Nagyon minimális meredekségkülönbség. Nem kell interakció.

# Összegzés: 4 interakciót lehet belevenni
# Ezek a következõek: fiatalosodasi_index*regio, hulladek_mennyiseg*regio, epitett_lakasok*regio, legkozelebbi_megyeszkh*regio

# Modellépítés:

# Modell_1: szelekciós_modell-t a regió változóval bõvítettük
modell_1 <- lm(jfm ~ . - nev, data = jarasok)
summary(modell_1)
resettest(modell_1) # A modell nem tekinthetõ jól specifikáltnak, mert a p-érték = 9.292e-08.

# Modell_2: Transzformációk bevétele: 5 logaritmizált tag + 1 Kvadratikus tag
modell_2 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
               + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
               +I(telepulesek_120felett^2) + log(fiatalodasi_index) + log(allaskeresok_aranya) + regio, data = jarasok)
summary(modell_2)
resettest(modell_2) # A modell nem jól specifikált, mert a p-érték = 5.595e-08.

# modell_1 és modell_1 összehasonlítása:
# 9.292e-08 > 5.595e-08, azaz az modell_1 jobban specifikáltnak tekinthetõ, mint a transzformált tagokat tartalmazó
AIC(modell_1, modell_2)
BIC(modell_1, modell_2)
#AIC és BIC szerint is jobb a modell_1

# Modell_3: A modell_2-bõl a fiatalosodási_index kivétele, mert az nem szignifikáns változó.
modell_3 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
               + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
               +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio, data = jarasok)
summary(modell_3)
resettest(modell_3) # A modell nem jól speficikált, mert a p-érték = 5.167e-08

# modell_2 és modell_3 összehasonlítása:
AIC(modell_3, modell_2)
BIC(modell_3, modell_2) # Bár valamivel rosszabbul specifikált, az információs kritériumok szerint jobb a modell_3, érdemes volt elhagyni a fiatalodási indexet.
anova(modell_2, modell_3) # anova szerint is érdemes volt kihagyni

# Interakciók: itt már vissza lehet venni a fiatalosodási indexet, ha szignifikáns az interakciója
# Következõ interakciókat fogjuk vizsgálni: fiatalodasi_index*regio, hulladek_mennyiseg*regio, epitett_lakasok*regio, legkozelebbi_megyeszkh*regio

# modell_int1: Modell_3-hoz mind a 4 interakció hozzáadása (Ezáltal a fiatalosodási index is visszakerül.)
modell_int1 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + regio*fiatalodasi_index + hulladek_mennyiseg*regio + epitett_lakasok*regio + legkozelebbi_megyeszkh*regio, data = jarasok)
summary(modell_int1)
resettest(modell_int1)# A modell nem jól specifikált, mert a p-érték = 1.51e-05.

# modell_3 és modell_int1 összehasonlítása:
AIC(modell_3, modell_int1)
BIC(modell_3, modell_int1) 
anova(modell_3, modell_int1)
# Az összes interakciót belerakva biztosan rosszabb lett a modell, mint nélkülük

# Modell_int2: a modell_int1-bõl a fiatalosodási index*régió interakció kivétele, így a fiatalosodási index változót végleg kiszedjük.
modell_int2 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + hulladek_mennyiseg*regio + epitett_lakasok*regio + legkozelebbi_megyeszkh*regio, data = jarasok)
summary(modell_int2)
resettest(modell_int2) # A modell nem jól specifikált, mert p-érték= 1.642e-05. De p-érték magasabb lett valamivel.

# modell_3 és modell_int2 összehasonlítása:
AIC(modell_3, modell_int2)
BIC(modell_3, modell_int2) 
anova(modell_3, modell_int2) # Még mindig nem érte meg az interakciókat bevonni.

# modell_int3: modell_int2-bõl a legkozelebbi_megyeszkh*regio interakció elhagyása
modell_int3 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + hulladek_mennyiseg*regio + epitett_lakasok*regio, data = jarasok)
summary(modell_int3)
resettest(modell_int3) # A modell nem jól specifikált, mert a p-érték = 1.339e-06. p-érték csökkent valamivel

# modell_3 és modell_int3 összehasonlítása:
AIC(modell_3, modell_int3)
BIC(modell_3, modell_int3) 
anova(modell_3, modell_int3) # valamivel közelebb vagyunk, de még mindig modell_3-at preferáljuk.

# modell_int4: modell_int3-bõl a hulladek_mennyiseg*regio interakció elhagyása
modell_int4 <- lm(jfm ~ log(kisker_boltok) + log(epitett_lakasok+0.001) + legkozelebbi_megyeszkh 
                  + szennyviz_csatlakozas + log(hulladek_mennyiseg+0.001) + villamosenergia + halalozas 
                  +I(telepulesek_120felett^2) + log(allaskeresok_aranya) + regio + epitett_lakasok*regio, data = jarasok)
summary(modell_int4)
resettest(modell_int4) # A modell nem jól specifikált, mert a p-érték = 3.962e-08. p-érték csökkent sajnos, rosszabbul specifikáltnak tekinthetõ így.

# modell_3 és modell_int4 összehasonlítása:
AIC(modell_3, modell_int4)
BIC(modell_3, modell_int4) 
anova(modell_3, modell_int4) # BIC és a Wald-teszt szerint is érdemes bevonnia. mdoellbe az epitett_lakasok*regio interakcióját.

# Összegzés:
# A modell_int4 a vegso modell. Mi ezt a modellt preferáljuk.
# Készítsük el a végsõ modell alapján a végsõ adatbázist is: 
vegso_jarasok <- jarasok
vegso_jarasok$kisker_boltok <- log(jarasok$kisker_boltok)
vegso_jarasok$epitett_lakasok <- log(jarasok$epitett_lakasok+0.001)
vegso_jarasok$hulladek_mennyiseg <- log(jarasok$hulladek_mennyiseg+0.001)
vegso_jarasok$allaskeresok_aranya <- log(jarasok$allaskeresok_aranya)
vegso_jarasok$telepulesek_120felett <- jarasok$telepulesek_120felett^2

# Végsõ modell: Multikollinearitás + Heteroszkedaszticitás

# Heteroszkedaszticitás:
vegso_jarasok$hibanegyzet <- modell_int4$residuals^2
ggplot(vegso_jarasok, aes(x=jfm, y=hibanegyzet)) + geom_point()
vegso_jarasok <- subset(vegso_jarasok, select = -hibanegyzet) # Hibanégyzet kivétele, csak az egyszerû ábrázolás miatt kellett.
str(vegso_jarasok)
ks.test(modell_int4$residuals, "pnorm") # Ez alapján érdemes a Koenker korrekciózott Breusch-Pagan tesztet alkalmazni.
bptest(modell_int4, studentize = TRUE)# A p-érték = 0.1469, szóval homoszkedasztikus a modell. Szuper.

white_alap <- white(modell_int4, interactions = TRUE)
white_alap$p.value

# Multikollonearitás viszgálata és kezelése:
str(vegso_jarasok[,2:(ncol(vegso_jarasok)-2)]) # Nincs benne a JFM, a régió és a járásnevek sem. Ez kell nekünk.
korrel0 <- cor(jarasok[,2:(ncol(vegso_jarasok)-2)])
corrplot(korrel0, method="number")
corrplot(korrel0, method="color") # Nem látható erõs kapcsolat.

vif(modell_int4)
# Minden VIF mutató értéke 5 alatt van, így szó sincs sem zavaró, sem káros multikollinearitásról.
# Ezek alól csak a régió és az épített lakások (és természetesen ezek interakciója) a kivétel, de ezt az interakció okozta, szóval ezzel nincsen probléma.
# Ugyan VIF mutatók alapján nem érdemes fõkomponenselemzést végezni, nézzük, meg, milyen fõkomponensek lennének kialakíthatók.
# Interakció elõtti VIF mutatók:
vif(modell_3) # Itt minden VIF mutató a zavaró 5-ös érték alatt van, szóval az interakció miatt lesznek csak zavaró értékek. Ez nem baj.
# Ennek következményében nem kell kezelni ezt a fajta mesterséges multikollinearitást, amit az interakcióval okoztunk.
# Másik szempont pedig az, hogy a Gyökös VIF mutató értékei nagyon közel vannak az 1-hez, ami szintén arra utal, hogy nincsen multikollinearitás.

str(vegso_jarasok[,2:(ncol(vegso_jarasok)-2)]) # Nincs benne a JFM, a régió és a járásnevek sem. Ez kell nekünk.
pca <- prcomp(vegso_jarasok[,2:(ncol(vegso_jarasok)-2)], center = TRUE, scale=TRUE)
summary(pca)# Az elsõ három fõkomponens felel meg a szórás/variancia > 1 szelekciós feltételnek

vegso_jarasok_pca <- cbind(vegso_jarasok, pca$x[,1:3])
str(vegso_jarasok_pca)

korrel <- cor(vegso_jarasok_pca[,c(2:11, 14:16)])
corrplot(korrel, method="number")
corrplot(korrel, method="color")

# Elõrejelzés:
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
  regio = "Közép-Magyarország"
)

elorejelzes <- predict(modell_int4, newdata = predict_data)
print(elorejelzes)

# Interakciós térkép a járásokra:
# A térképet HTML fájlként csatoljuk és ott interaktívan lehet a járások fejlettségi mutatóját megnézni.
# Innentól csak ennek a lekódólása látható.
# A kódok lefuttatásához szükség van a ZIP-file-ra.

#--------Térkép cucc----------------

# Külön adatbázis létrehozása:
jarasok_terkep <- jarasok

# a Sásdi járás azóta Hegyháti lett, a shape fájlban is úgy van, szóval itt is átnevezzük:
jarasok_terkep[129,1] <- "Hegyháti"
jarasok_terkep <- jarasok_terkep[order(jarasok_terkep$nev),]

#Beolvasás és megjavítás:
shape <- st_read("Magyar SHP/hun_admbnda_adm2_osm_20220720.shp")
shape <- st_make_valid(shape)

koord <- st_centroid(shape)
koord <- as.data.frame(st_coordinates(koord))
koord$Jaras <- shape$ADM2_HU 

# Kerületek átkonvertálása egységesen Budapestre
shape$Jaras_wBP <- shape$ADM2_HU
unique(shape$Jaras_wBP)
shape$Jaras_wBP[grepl("district", shape$Jaras_wBP)] <- "Budapest"

# Betûrend:
shape <- shape[order(shape$Jaras_wBP),]

nev_BPk <- c(rep("Budapesti kerületek",22), jarasok_terkep$nev)
nev_BPk <- nev_BPk[order(nev_BPk)]

# Betesszük a sok Budapestet
shape$nev_BPk <- nev_BPk
shape <- merge(x=shape, y=jarasok_terkep, by.x="nev_BPk", by.y="nev", all.x=TRUE)

# Kvintilisek alapján bontjuk meg a fejlettségi mutatót
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
            title = "Fejlettségi mutató")

# Kész a térkép!



#lábjegyzet, hagyd figyelmen kívül
ncol(jarasok)#23 magyarázóváltozónk van, ez általában szerencsé eset, azonban a dolgozat terjedelmére való tekintettel érdemes lenne csak 10-12 megtartani
library(mRMRe)
szurt<-jarasok[,2:ncol(jarasok)] # válasszuk le a nem releváns változókat: név
szurt_mrmr<-mRMR.data(as.data.frame(szurt)) # alakítsuk mrmr objektummár az adatbázist
x<-solutions(mRMR.classic(data = szurt_mrmr, target_indices = c(23), feature_count = 10))
x[[1]] #listát kapunk eredményül
# melyek ezek a változók?
colnames(szurt)[x[[1]]]
jarasok <- jarasok[,c(1,6, 9, 10, 13, 14, 16, 17, 19:21, 24)]
