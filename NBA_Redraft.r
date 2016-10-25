library(xml2) 
library(dplyr)
library(ggplot2)

tables = lapply(c(1989:2008) ,function(yr){
      df <- paste0("http://www.basketball-reference.com/draft/NBA_", yr, ".html", sep="") %>%
            as.character() %>%
            read_html() %>%
            xml_find_all(xpath = "//*[@id='stats']/tbody/tr/td") %>%
            xml_text() %>%
            matrix(ncol=21, byrow=T) %>%
            as.data.frame(stringsAsFactors=FALSE)
      df$Year = yr
      names(df) = c('Rk', 'Tm', 'Player', 'College', 'Yrs', 'G','MP_T', 'PTS_T',
                    'TRB_T','AST_T', 'FG','threeP', 'FT', 'MP', 'PTS', 'TRB', 'AST',
                    'WS', 'WS_per', 'BPM', 'VORP','Year')
      df
})

str(tables)


df_full <- Reduce(x = tables, f = rbind)

df_full$Rk <- as.numeric(df_full$Rk)
df_full$Yrs <- as.numeric(df_full$Yrs)
df_full$G <- as.numeric(df_full$G)
df_full$MP_T <- as.numeric(df_full$MP_T)
df_full$PTS_T <- as.numeric(df_full$PTS_T)
df_full$TRB_T <- as.numeric(df_full$TRB_T)
df_full$AST_T <- as.numeric(df_full$AST_T)
df_full$FG <- as.numeric(df_full$FG)
df_full$threeP <- as.numeric(df_full$threeP)
df_full$FT <- as.numeric(df_full$FT)
df_full$MP <- as.numeric(df_full$MP)
df_full$PTS <- as.numeric(df_full$PTS)
df_full$TRB <- as.numeric(df_full$TRB)
df_full$AST <- as.numeric(df_full$AST)
df_full$WS <- as.numeric(df_full$WS)
df_full$WS_per <- as.numeric(df_full$WS_per)
df_full$BPM <- as.numeric(df_full$BPM)
df_full$VORP <- as.numeric(df_full$VORP)
df_full$Year <- as.numeric(df_full$Year)

str(df_full)

df <- select(df_full, Rk, Tm, Player, College, MP_T, VORP, Year)

#Dealing with missing values
#1989
df$College[df$Player == "Vlade Divac"] <- "International"
df$College[df$Player == "Dino Radja"] <- "International"
#1990
df$College[df$Player == "Toni Kukoc"] <- "International"
df$College[df$Player == "Stefano Rusconi"] <- "International"
#1991
df$College[df$Player == "Zan Tabak"] <- "International"
#1992
df$College[df$Player == "Sasha Danilovic"] <- "International"
#1993
df$College[df$Player == "Gheorghe Muresan"] <- "International"
df$College[df$Player == "Marcelo Nicola"] <- "International"
#1994
df$College[df$Player == "Andrei Fetisov"] <- "International"
df$College[df$Player == "William Njoku"] <- "International"
df$College[df$Player == "Zeljko Rebraca"] <- "International"
#1995
df$College[df$Player == "Kevin Garnett"] <- "High School"
df$College[df$Player == "Dragan Tarlac"] <- "International"
df$College[df$Player == "Dejan Bodiroga"] <- "International"
df$College[df$Player == "Aurelijius Zukauskas"] <- "International"
#1996
df$College[df$Player == "Peja Stojakovic"] <- "International"
df$College[df$Player == "Zydrunas Ilgauskas"] <- "International"
df$College[df$Player == "Efthimi Rentzias"] <- "International"
df$College[df$Player == "Martin Muursepp"] <- "International"
df$College[df$Player == "Kobe Bryant"] <- "High School"
df$College[df$Player == "Jermaine O'Neal"] <- "High School"
#1997
df$College[df$Player == "Chris Anstey"] <- "International"
df$College[df$Player == "Marko Milic"] <- "International"
df$College[df$Player == "Predrag Drobnjak"] <- "International"
df$College[df$Player == "Alain Digbeu"] <- "International"
df$College[df$Player == "Ben Pepper"] <- "International"
df$College[df$Player == "Roberto Duenas"] <- "International"
df$College[df$Player == "Tracy McGrady"] <- "High School"
#1998
df$College[df$Player == "Dirk Nowitzki"] <- "International"
df$College[df$Player == "Rasho Nesterovic"] <- "International"
df$College[df$Player == "Mirsad Turkcan"] <- "International"
df$College[df$Player == "Vladimir Stepania"] <- "International"
df$College[df$Player == "Bruno Sundov"] <- "International"
df$College[df$Player == "Al Harrington"] <- "High School"
df$College[df$Player == "Rashard Lewis"] <- "High School"
df$College[df$Player == "Korleone Young"] <- "High School"
#1999
df$College[df$Player == "Frederic Weis"] <- "International"
df$College[df$Player == "Andrei Kirilenko"] <- "International"
df$College[df$Player == "Wang Zhizhi"] <- "International"
df$College[df$Player == "Gordan Giricek"] <- "International"
df$College[df$Player == "Manu Ginobili"] <- "International"
df$College[df$Player == "Jonathan Bender"] <- "High School"
df$College[df$Player == "Leon Smith"] <- "High School"
#2000
df$College[df$Player == "Darius Miles"] <- "High School"
df$College[df$Player == "DeShawn Stevenson"] <- "High School"
df$College[df$Player == "Hedo Turkoglu"] <- "International"
df$College[df$Player == "Dalibor Bagaric"] <- "International"
df$College[df$Player == "Jake Tsakalidis"] <- "International"
df$College[df$Player == "Primoz Brezec"] <- "International"
df$College[df$Player == "Marko Jaric"] <- "International"
df$College[df$Player == "Soumaila Samake"] <- "International"
df$College[df$Player == "Olumide Oyedeji"] <- "International"
df$College[df$Player == "Josip Sesar"] <- "International"
df$College[df$Player == "Igor Rakocevic"] <- "International"
#2001
df$College[df$Player == "Pau Gasol"] <- "International"
df$College[df$Player == "DeSagana Diop"] <- "International"
df$College[df$Player == "Vladimir Radmanovic"] <- "International"
df$College[df$Player == "Raul Lopez"] <- "International"
df$College[df$Player == "Tony Parker"] <- "International"
df$College[df$Player == "Mehmet Okur"] <- "International"
df$College[df$Player == "Ousmane Cisse"] <- "International"
df$College[df$Player == "Antonis Fotsis"] <- "International"
df$College[df$Player == "Robertas Javtokas"] <- "International"
df$College[df$Player == "Kwame Brown"] <- "High School"
df$College[df$Player == "Tyson Chandler"] <- "High School"
df$College[df$Player == "Eddy Curry"] <- "High School"
#2002
df$College[df$Player == "Yao Ming"] <- "International"
df$College[df$Player == "Nikoloz Tskitishvili"] <- "International"
df$College[df$Player == "Nene Hilario"] <- "International"
df$College[df$Player == "Bostjan Nachbar"] <- "International"
df$College[df$Player == "Jiri Welsch"] <- "International"
df$College[df$Player == "Nenad Krstic"] <- "International"
df$College[df$Player == "Milos Vujanic"] <- "International"
df$College[df$Player == "David Andersen"] <- "International"
df$College[df$Player == "Juan Carlos Navarro"] <- "International"
df$College[df$Player == "Mario Kasun"] <- "International"
df$College[df$Player == "Peter Fehse"] <- "International"
df$College[df$Player == "Federico Kammerichs"] <- "International"
df$College[df$Player == "Mladen Sekularac"] <- "International"
df$College[df$Player == "Amar'e Stoudemire"] <- "High School"
#2003
df$College[df$Player == "Darko Milicic"] <- "International"
df$College[df$Player == "Mickael Pietrus"] <- "International"
df$College[df$Player == "Zarko Cabarkapa"] <- "International"
df$College[df$Player == "Sasha Pavlovic"] <- "International"
df$College[df$Player == "Boris Diaw"] <- "International"
df$College[df$Player == "Zoran Planinic"] <- "International"
df$College[df$Player == "Carlos Delfino"] <- "International"
df$College[df$Player == "Ndudi Ebi"] <- "International"
df$College[df$Player == "Leandro Barbosa"] <- "International"
df$College[df$Player == "Maciej Lampe"] <- "International"
df$College[df$Player == "Sofoklis Schortsanitis"] <- "International"
df$College[df$Player == "Szymon Szewczyk"] <- "International"
df$College[df$Player == "Slavko Vranes"] <- "International"
df$College[df$Player == "Zaza Pachulia"] <- "International"
df$College[df$Player == "Malick Badiane"] <- "International"
df$College[df$Player == "Sani Becirovic"] <- "International"
df$College[df$Player == "James Lang"] <- "International"
df$College[df$Player == "Paccelis Morlende"] <- "International"
df$College[df$Player == "Remon Van de Hare"] <- "International"
df$College[df$Player == "Nedzad Sinanovic"] <- "International"
df$College[df$Player == "Xue Yuyang"] <- "International"
df$College[df$Player == "Andreas Glyniadakis"] <- "International"
df$College[df$Player == "LeBron James"] <- "High School"
df$College[df$Player == "Travis Outlaw"] <- "High School"
df$College[df$Player == "Kendrick Perkins"] <- "High School"
#2004
df$College[df$Player == "Andris Biedrins"] <- "International"
df$College[df$Player == "Pavel Podkolzin"] <- "International"
df$College[df$Player == "Viktor Khryapa"] <- "International"
df$College[df$Player == "Sergei Monia"] <- "International"
df$College[df$Player == "Sasha Vujacic"] <- "International"
df$College[df$Player == "Beno Udrih"] <- "International"
df$College[df$Player == "Anderson Varejao"] <- "International"
df$College[df$Player == "Peter John Ramos"] <- "International"
df$College[df$Player == "Albert Miralles"] <- "International"
df$College[df$Player == "Viktor Sanikidze"] <- "International"
df$College[df$Player == "Ha Seung-Jin"] <- "International"
df$College[df$Player == "Sergei Lishouk"] <- "International"
df$College[df$Player == "Vassilis Spanoulis"] <- "International"
df$College[df$Player == "Sergei Karaulov"] <- "International"
df$College[df$Player == "Dwight Howard"] <- "High School"
df$College[df$Player == "Shaun Livingston"] <- "High School"
df$College[df$Player == "Robert Swift"] <- "High School"
df$College[df$Player == "Sebastian Telfair"] <- "High School"
df$College[df$Player == "Al Jefferson"] <- "High School"
df$College[df$Player == "Josh Smith"] <- "High School"
df$College[df$Player == "J.R. Smith"] <- "High School"
df$College[df$Player == "Dorell Wright"] <- "High School"
#2005
df$College[df$Player == "Fran Vazquez"] <- "International"
df$College[df$Player == "Yaroslav Korolev"] <- "International"
df$College[df$Player == "Johan Petro"] <- "International"
df$College[df$Player == "Ian Mahinmi"] <- "International"
df$College[df$Player == "Ricky Sanchez"] <- "International"
df$College[df$Player == "Ersan Ilyasova"] <- "International"
df$College[df$Player == "Roko Ukic"] <- "International"
df$College[df$Player == "Mile Ilic"] <- "International"
df$College[df$Player == "Martynas Andriuskevicius"] <- "International"
df$College[df$Player == "Erazem Lorbek"] <- "International"
df$College[df$Player == "Mickael Gelabale"] <- "International"
df$College[df$Player == "Axel Hervelle"] <- "International"
df$College[df$Player == "Marcin Gortat"] <- "International"
df$College[df$Player == "Uros Slokar"] <- "International"
df$College[df$Player == "Cenk Akyol"] <- "International"
df$College[df$Player == "Martell Webster"] <- "High School"
df$College[df$Player == "Andrew Bynum"] <- "High School"
df$College[df$Player == "Gerald Green"] <- "High School"
df$College[df$Player == "C.J. Miles"] <- "High School"
df$College[df$Player == "Monta Ellis"] <- "High School"
df$College[df$Player == "Louis Williams"] <- "High School"
df$College[df$Player == "Andray Blatche"] <- "High School"
df$College[df$Player == "Amir Johnson"] <- "High School"
#2006
df$College[df$Player == "Andrea Bargnani"] <- "International"
df$College[df$Player == "Mouhamed Sene"] <- "International"
df$College[df$Player == "Thabo Sefolosha"] <- "International"
df$College[df$Player == "Oleksiy Pecherov"] <- "International"
df$College[df$Player == "Sergio Rodriguez"] <- "International"
df$College[df$Player == "Joel Freeland"] <- "International"
df$College[df$Player == "Kosta Perovic"] <- "International"
df$College[df$Player == "Marcus Vinicius"] <- "International"
df$College[df$Player == "Lior Eliyahu"] <- "International"
df$College[df$Player == "Vladimir Veremeenko"] <- "International"
df$College[df$Player == "Cheikh Samb"] <- "International"
df$College[df$Player == "Yotam Halperin"] <- "International"
df$College[df$Player == "Ejike Ugboaja"] <- "International"
df$College[df$Player == "Edin Bavcic"] <- "International"
df$College[df$Player == "Loukas Mavrokefalidis"] <- "International"
df$College[df$Player == "Damir Markota"] <- "International"
#2007
df$College[df$Player == "Yi Jianlian"] <- "International"
df$College[df$Player == "Marco Belinelli"] <- "International"
df$College[df$Player == "Rudy Fernandez"] <- "International"
df$College[df$Player == "Tiago Splitter"] <- "International"
df$College[df$Player == "Petteri Koponen"] <- "International"
df$College[df$Player == "Kyrylo Fesenko"] <- "International"
df$College[df$Player == "Stanko Barac"] <- "International"
df$College[df$Player == "Sun Yue"] <- "International"
df$College[df$Player == "Marc Gasol"] <- "International"
df$College[df$Player == "Renaldas Seibutis"] <- "International"
df$College[df$Player == "Brad Newley"] <- "International"
df$College[df$Player == "Giorgos Printezis"] <- "International"
df$College[df$Player == "Milovan Rakovic"] <- "International"
#2008
df$College[df$Player == "Danilo Gallinari"] <- "International"
df$College[df$Player == "Alexis Ajinca"] <- "International"
df$College[df$Player == "Serge Ibaka"] <- "International"
df$College[df$Player == "Nicolas Batum"] <- "International"
df$College[df$Player == "Nikola Pekovic"] <- "International"
df$College[df$Player == "Omer Asik"] <- "International"
df$College[df$Player == "Nathan Jawai"] <- "International"
df$College[df$Player == "Ante Tomic"] <- "International"
df$College[df$Player == "Goran Dragic"] <- "International"
df$College[df$Player == "Tadija Dragicevic"] <- "International"
df$College[df$Player == "Semih Erden"] <- "International"

#把Redraft不要依照unique重排
tables_test2 = lapply(c(1989:2008) ,function(yr){
              df_2 <- subset(df, Year == yr) 
              df_2 <- df_2[order(-df_2$VORP), ]
              df_Redraft = data.frame(VORP = df_2$VORP, Redraft=c(1:length(df_2$VORP)), Player = df_2$Player)
              df_Redraft = df_Redraft[c("Player", "Redraft")]
              df_2 <- left_join(df_2 , df_Redraft) %>% 
                      arrange(Redraft) %>%
                      as.data.frame(stringsAsFactors=FALSE)
      
              df_2
})

df_redraft2 <- Reduce(x = tables_test2, f = rbind)
#Rank - Redraft
p2 <- ggplot(data = df_redraft2, aes(Redraft, Rk)) +
     scale_y_continuous(name="Rk", limits=c(0,60)) +
     scale_x_continuous(name="Redraft", limits=c(0,60)) +
     geom_abline(data=df_redraft, mapping=aes(slope = 1, intercept = 0)) +
     geom_point()      

p2
#斜線上方是表現比預期好的球員，斜線下方是表現比預期差的球員。

#omit NA
df_redraft3 <- na.omit(df_redraft2)
p3 <- ggplot(data = df_redraft3, aes(Rk, Redraft)) +
     scale_x_continuous(name="Rk", limits=c(0,60)) +
     scale_y_continuous(name="Redraft", limits=c(0,60)) +
     geom_abline(data=df_redraft, mapping=aes(slope = 1, intercept = 0)) +
     geom_point() +     
     coord_flip()

p3


#MP_T > 1000
df_redraft4 <- subset(df_redraft2, MP_T > 1000)
p4 <- ggplot(data = df_redraft4, aes(Redraft, Rk)) +
     scale_y_continuous(name="Rk", limits=c(0,60)) +
     scale_x_continuous(name="Redraft", limits=c(0,60)) +
     geom_abline(data=df_redraft, mapping=aes(slope = 1, intercept = 0)) +
     geom_point()    
  
p4
