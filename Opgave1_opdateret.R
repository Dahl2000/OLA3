library(devtools)
library(dkstat)
library(dst)
library(danstat)
library(tidyverse)
library(combinat)

################################################
# Opgave 1.1 – Kombinationsalgoritme i R
#############################################

meta_data <- dst_meta("FORV1")
str(meta_data)

# Hent dataen
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# Filtrer data
FORV1_filter <- list(
  INDIKATOR = "*",  # Brug "*" for at inkludere alle indikatorer
  Tid = "*"         # Brug "*" for at inkludere alle tidspunkter
)

FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

# bred format
FORV1Data_wide <- FORV1Data %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

# Sørg for, at TID-kolonnen er i kvartalsformat
FORV1Data_wide$TID <- paste0(format(as.Date(FORV1Data_wide$TID), "%Y"), quarters(as.Date(FORV1Data_wide$TID)))

# lav data til kvartal data
FORV1Data_wide <- aggregate(. ~ TID, data = FORV1Data_wide, FUN = mean, na.rm = TRUE)

start_date <- "2000Q1"
end_date <- "2024Q4"

# Subsetter data mellem start- og slutdatoen
FORV1_2000Q1 <- subset(FORV1Data_wide, TID >= start_date & TID <= end_date)
row.names(FORV1_2000Q1) <- NULL

# fjern TID og indikator
# FORV1_2000Q1 <- FORV1_2000Q1[,-(1:2)]

# Gør tid til factor
FORV1_2000Q1$TID <- as.factor(FORV1_2000Q1$TID)

# forkort kolonne navne
colnames(FORV1_2000Q1)[3:ncol(FORV1_2000Q1)] <- c("Familiens økonomi (nu vs. før)",
                                                  "Familiens økonomi (fremtid)",
                                                  "Danmarks økonomi (nu vs. før)",
                                                  "Danmarks økonomi (fremtid)",
                                                  "Større køb (nu)",
                                                  "Priser (nu vs. før)",
                                                  "Priser (fremtid)",
                                                  "Arbejdsløshed (fremtid)",
                                                  "Større køb (fremtid)",
                                                  "Spare op (nu)",
                                                  "Spare op (fremtid)",
                                                  "Økonomisk status (nu)")

# FORV1_2000Q1 til numerisk
FORV1_2000Q1$Forbrugertillidsindikatoren <- as.numeric(FORV1_2000Q1$Forbrugertillidsindikatoren)
FORV1_2000Q1$`Familiens økonomi (nu vs. før)` <- as.numeric(FORV1_2000Q1$`Familiens økonomi (nu vs. før)`)
FORV1_2000Q1$`Familiens økonomi (fremtid)` <- as.numeric(FORV1_2000Q1$`Familiens økonomi (fremtid)`)
FORV1_2000Q1$`Danmarks økonomi (nu vs. før)` <- as.numeric(FORV1_2000Q1$`Danmarks økonomi (nu vs. før)`)
FORV1_2000Q1$`Danmarks økonomi (fremtid)` <- as.numeric(FORV1_2000Q1$`Danmarks økonomi (fremtid)`)
FORV1_2000Q1$`Større køb (nu)` <- as.numeric(FORV1_2000Q1$`Større køb (nu)`)
FORV1_2000Q1$`Priser (nu vs. før)` <- as.numeric(FORV1_2000Q1$`Priser (nu vs. før)`)
FORV1_2000Q1$`Priser (fremtid)` <- as.numeric(FORV1_2000Q1$`Priser (fremtid)`)
FORV1_2000Q1$`Arbejdsløshed (fremtid)` <- as.numeric(FORV1_2000Q1$`Arbejdsløshed (fremtid)`)
FORV1_2000Q1$`Større køb (fremtid)` <- as.numeric(FORV1_2000Q1$`Større køb (fremtid)`)
FORV1_2000Q1$`Spare op (nu)` <- as.numeric(FORV1_2000Q1$`Spare op (nu)`)
FORV1_2000Q1$`Spare op (fremtid)` <- as.numeric(FORV1_2000Q1$`Spare op (fremtid)`)
FORV1_2000Q1$`Økonomisk status (nu)` <- as.numeric(FORV1_2000Q1$`Økonomisk status (nu)`)


#######
# Lav kombinationer ved hjælp af combn funktion
komb1 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 1)
komb2 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 2)
komb3 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 3)
komb4 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 4)
komb5 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 5)
komb6 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 6)
komb7 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 7)
komb8 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 8)
komb9 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 9)
komb10 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 10)
komb11 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 11)
komb12 <- combn(FORV1_2000Q1[1,3:ncol(FORV1_2000Q1)], 12)

class(komb12)

komb2[1,1]+komb2[2,1]

colMeans(komb2)

x <- apply(komb2, 2, mean)

###############################################

# liste til resultater
resultat_liste <- list()

# Loop
for (i in 1:nrow(FORV1_2000Q1)) {
  komb2 <- colMeans(combn(FORV1_2000Q1[i,3:ncol(FORV1_2000Q1)], 2))
  # Tilføj resultatet til listen
  resultat_liste[[i]] <- komb2
}


####################################
# På alle
# lav FORV1_2000Q1 til en matrix for at undgå error: 'x' must be an array of at least two dimensions
# FORV1_2000Q1_matrix <- data.matrix(FORV1_2000Q1)
# row.names(FORV1_2000Q1_matrix) <- NULL

# liste til alle 12 kombinationer
comb <- rep(list(rep(list(rep(list())),nrow(FORV1_2000Q1))),(ncol(FORV1_2000Q1)-2))

# loop til alle 12 kombinationer
for(j in 1:(ncol(FORV1_2000Q1)-2)) {
  for (i in 1:length(comb[[1]])) {
    # tilføj til comb listen
    comb[[j]][[i]] <- colMeans(combn(FORV1_2000Q1[i,3:ncol(FORV1_2000Q1)],j))
  }
}

# den gemmer kombinationer fra 1:11, men ikke 12. D. 12 kombi laver vi om lidt

#########################################

unlist1 <- matrix(unlist(comb[[1]]),ncol = 100)
unlist2 <- matrix(unlist(comb[[2]]),ncol = 100)
unlist3 <- matrix(unlist(comb[[3]]),ncol = 100)
unlist4 <- matrix(unlist(comb[[4]]),ncol = 100)
unlist5 <- matrix(unlist(comb[[5]]),ncol = 100)
unlist6 <- matrix(unlist(comb[[6]]),ncol = 100)
unlist7 <- matrix(unlist(comb[[7]]),ncol = 100)
unlist8 <- matrix(unlist(comb[[8]]),ncol = 100)
unlist9 <- matrix(unlist(comb[[9]]),ncol = 100)
unlist10 <- matrix(unlist(comb[[10]]),ncol = 100)
unlist11 <- matrix(unlist(comb[[11]]),ncol = 100)


# Beregn gennemsnittet fra kolonne 3 til sidste kolonne for hver række for at lave den 12 kombinationer loopet ikke laver
unlist12 <- rowMeans(FORV1_2000Q1[, 3:ncol(FORV1_2000Q1)], na.rm = TRUE)

# Liste af alle matrix-objekter
matrix_list <- list(unlist1, unlist2, unlist3, unlist4, unlist5, unlist6, 
                    unlist7, unlist8, unlist9, unlist10, unlist11, unlist12)

# Samle som rækker
comb_data <- data.frame(do.call(rbind, matrix_list))

#######################

# Lav år og kvartal til en vektor, der skal indsættes som en ny række
# Beregn antal kolonner
n <- ncol(comb_data)

# Beregn år og kvartal baseret på antal kolonner i matrixen
start_år <- 2000
år <- start_år + (0:(n - 1)) %/% 4
kvartaler <- (0:(n - 1)) %% 4 + 1
år_kvartaler <- paste0(år, "-Q", kvartaler)

# Indsæt 'år_kvartaler' som den første række i matrixen
comb_data <- rbind(år_kvartaler, comb_data)

# TID som rækkenavn
rownames(comb_data)[1] <- "TID"

# transpose så vi får 100 rækker, i stedet for 100 kolonner
comb_data_t <- t(comb_data)
comb_data_t <- data.frame(comb_data_t)

comb_data_t$TID <- as.factor(comb_data_t$TID)

###################################
########################
# lav kombinationer på spørgsmålene

# Udtræk spørgsmålene til videre brug - opg. 1.3
kolonnenavne <- colnames(FORV1_2000Q1)[3:14]

# Generer alle kombinationer fra størrelse 1 til 12
alle_kombinationer <- unlist(lapply(1:length(kolonnenavne), 
                                    function(x) combn(kolonnenavne, x, simplify = FALSE)), recursive = FALSE)

# Konverter kombinationerne til en læsbar form (en enkelt streng pr. kombination)
comb_names <- sapply(alle_kombinationer, function(x) paste(x, collapse = "_"))

# Tildel kombinationerne som kolonnenavne
colnames(comb_data_t)[2:length(comb_data_t)] <- comb_names[1:(length(comb_data_t) - 1)]

##################################################
# OPG. 1.2 - hvilken indikator har den bedste R2
#################################################

# indlæs realvækst
realforbrug.meta <- dst_meta(table = "NKHC021", lang = "da") 

# vælger filteret på realforbrug dataframe (vil gerne hente totalforbruget, 2020 priser, der er sæssonkorrigeret)
realforbrug.filter <- list( 
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

# Henter data ud fra filteret der er sat ovenover
realforbrug_data <- dst_get_data(table = "NKHC021", query = realforbrug.filter)  
realforbrug_data <- realforbrug_data[, -c(1:3)]  # Fjern de tre første kolonner med metadata, da der er na værdier i value

# Beregning af realvækst
realforbrug_data$Realvækst <- (realforbrug_data$value / dplyr::lag(realforbrug_data$value, 4) - 1) * 100  # Beregn realvækst år-over-år

# udvælger rækker og kolonner for perioden 2000 Q1 til 2024 Q4ª (række 41-138)
realforbrug_data_filtreret <- realforbrug_data[c(41:nrow(realforbrug_data)), c("TID", "Realvækst")] 

# tilføj 2 rækker mere så det matcher længden på comb_data_t
new_rows <- data.frame(matrix(NA, nrow = 1, ncol = ncol(realforbrug_data_filtreret)))
colnames(new_rows) <- colnames(realforbrug_data_filtreret)
realforbrug_data_filtreret <- rbind(realforbrug_data_filtreret, new_rows)

# kombinere datasættene med cbind
kombineret_data <- cbind(comb_data_t[, 1], Realvækst = realforbrug_data_filtreret$Realvækst, comb_data_t[, -1])
colnames(kombineret_data)[1] <- "TID"

# Konverter kolonnerne fra 3 til slutningen til numeriske
kombineret_data[, 3:ncol(kombineret_data)] <- lapply(kombineret_data[, 3:ncol(kombineret_data)], as.numeric)

# laver en liste til at gemme R²-værdierne
r2_values <- list()

# Loop gennem hver indikator (kolonne) og beregn R²-værdien
for (i in 3:ncol(kombineret_data)) {
  col_names <- names(kombineret_data)[i]
  # Kør lineær regression mellem Realvækst og den aktuelle indikator
  model <- lm(Realvækst ~ kombineret_data[[col_names]], data = kombineret_data)
  # Gem R²-værdien
  r2_values[[col_names]] <- summary(model)$r.squared
}

# Konverter R²-værdierne til en dataframe for nem sammenligning
r2_df <- data.frame(Indicator = names(r2_values), R2 = unlist(r2_values))
r2_df$R2 <- round(r2_df$R2, digits = 3)

# her laves der en rangliste over r2 værdierne fra højest til lavest. 
r2_df <- r2_df[order(-r2_df$R2), ] 

print(r2_df)[1,] # bedste R2 - 0.451


############################################
# OPG. 1.3
###############################################

# Bedste kombination er med 7 spørgsmål. Spørgsmålene er en blanding af mikro og makro tendenser.
# Familiens økonomi (fremtid)_Danmarks økonomi (nu vs. før)_Større køb (nu)
# Priser (nu vs. før)_Større køb (fremtid)_Spare op (fremtid)_Økonomisk status (nu)

# alternativ kombination
print(r2_df)[2,] 

##############################################
# OPG. 1.4
#############################################

# subset bedste indikator 
best_comb <- kombineret_data[["Familiens økonomi (fremtid)_Danmarks økonomi (nu vs. før)_Større køb (nu)_Priser (nu vs. før)_Større køb (fremtid)_Spare op (fremtid)_Økonomisk status (nu)"]]

# indsæt bedste indikator til FORV1_2000Q1
kombineret_data_FINAL <- cbind(kombineret_data[, 1:2], best_comb, kombineret_data[, 3:ncol(kombineret_data)])

# lav linær model for bedste kombination
model <- lm(kombineret_data_FINAL$Realvækst ~ kombineret_data_FINAL$best_comb, data = kombineret_data_FINAL, na.action = na.exclude)

summary(model)

# lav realvækst forudsigelse for Q3 og Q4 2024
predict_data <- data.frame(best_comb = kombineret_data_FINAL$best_comb)
best_comb_Realvækst <- predict(model, newdata = predict_data)
predict_data$Predicted_Realvækst <- best_comb_Realvækst

# indsæt predicted realvækst
kombineret_data_FINAL_FINAL <- cbind(kombineret_data_FINAL[, 1:2], best_comb_Realvækst, kombineret_data_FINAL[, 3:ncol(kombineret_data_FINAL)])

library(ggplot2)
library(dplyr)

ggplot(kombineret_data_FINAL_FINAL, aes(x = TID)) +
  # Linje for best_comb_Realvækst, ignorér NA uden at fjerne rækker
  geom_line(aes(y = best_comb_Realvækst, color = "Forudsigelse - realvækst", group = 1), na.rm = TRUE) +  # group=1 for at forbinde alle punkter i én linje
  # Linje for Realvækst, ignorér NA uden at fjerne rækker
  geom_line(aes(y = Realvækst, color = "DST - Realvækst", group = 1), na.rm = TRUE) +  # Korrigeret label for DST - Realvækst
  labs(title = "Forudsigelse af realvæksten øjner stigning og fald",
       x = "TID", y = "Realvækst") +
  theme_minimal() +
  # Tilføj labels kun for 2024-Q3 og 2024-Q4
  geom_text(data = subset(kombineret_data_FINAL_FINAL, TID %in% c("2024-Q3", "2024-Q4")),
            aes(y = best_comb_Realvækst, label = round(best_comb_Realvækst, 2)),  # Tilføj y-værdi for tekstpositionen
            vjust = -1, color = "red") +  # Juster position og farve på tekst
  # Rotér x-aksens etiketter med 45 grader
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  # Vis hver anden etikette på x-aksen
  scale_x_discrete(breaks = levels(kombineret_data_FINAL_FINAL$TID)[seq(1, length(levels(kombineret_data_FINAL_FINAL$TID)), by = 3)]) +
  # Definer farver og tilføj en legend
  scale_color_manual(values = c("Forudsigelse - realvækst" = "blue", 
                                "DST - Realvækst" = "green")) +
  # Tilføj en title til legend
  guides(color = guide_legend(title = "Variabler"))


#####################################################
# OPG. 1.5
#####################################################

# udvælg mikro spørgsmål kolonner
mikro <- data.frame(FORV1_2000Q1$`Familiens økonomi (nu vs. før)`,
                    FORV1_2000Q1$`Familiens økonomi (fremtid)`,
                    FORV1_2000Q1$`Større køb (nu)`,
                    FORV1_2000Q1$`Større køb (fremtid)`,
                    FORV1_2000Q1$`Spare op (nu)`,
                    FORV1_2000Q1$`Spare op (fremtid)`,
                    FORV1_2000Q1$`Økonomisk status (nu)`)

############################
# kombinations algoritme for mikro spørgsmål

mikro_liste <- rep(list(rep(list(rep(list())),nrow(mikro))),(ncol(mikro)))

# mikro <- matrix(mikro)

for(j in 1:(ncol(mikro))) {
  for (i in 1:length(mikro_liste[[1]])) {
    # tilføj til comb listen
    mikro_liste[[j]][[i]] <- colMeans(combn(as.numeric(mikro[i,1:ncol(mikro)]),j))
  }
}

mikro1 <- matrix(unlist(mikro_liste[[1]]),ncol = 100)
mikro2 <- matrix(unlist(mikro_liste[[2]]),ncol = 100)
mikro3 <- matrix(unlist(mikro_liste[[3]]),ncol = 100)
mikro4 <- matrix(unlist(mikro_liste[[4]]),ncol = 100)
mikro5 <- matrix(unlist(mikro_liste[[5]]),ncol = 100)
mikro6 <- matrix(unlist(mikro_liste[[6]]),ncol = 100)

mikro7 <- rowMeans(mikro[, 1:ncol(mikro)], na.rm = TRUE)

mikro_liste <- list(mikro1, mikro2, mikro3, mikro4, mikro5, mikro6, mikro7)
mikro_data <- data.frame(do.call(rbind, mikro_liste))

##############################
# Lav år og kvartal til en vektor, der skal indsættes som en ny række
# Beregn antal kolonner
n <- ncol(mikro_data)

# Beregn år og kvartal baseret på antal kolonner i matrixen
start_år <- 2000
år <- start_år + (0:(n - 1)) %/% 4
kvartaler <- (0:(n - 1)) %% 4 + 1
år_kvartaler <- paste0(år, "-Q", kvartaler)

# Indsæt 'år_kvartaler' som den første række i matrixen
mikro_data <- rbind(år_kvartaler, mikro_data)

# TID som rækkenavn
rownames(mikro_data)[1] <- "TID"

# transpose så vi får 100 rækker, i stedet for 100 kolonner
mikro_data_t <- t(mikro_data)
mikro_data_t <- data.frame(mikro_data_t)

mikro_data_t$TID <- as.factor(mikro_data_t$TID)

###################
# lav kombinationer på mikro spørgsmålene

# brug mikro_data_frame

mikro_kol <- c("Familiens økonomi (nu vs. før)",
               "Familiens økonomi (fremtid)",
               "Større køb (nu)",
               "Større køb (fremtid)",
               "Spare op (nu)",
               "Spare op (fremtid)",
               "Økonomisk status (nu)")

# Generer alle kombinationer fra størrelse 1 til 7
mikro_kombinationer <- unlist(lapply(1:length(mikro_kol), 
                                    function(x) combn(mikro_kol, x, simplify = FALSE)), recursive = FALSE)

# Konverter kombinationerne til en læsbar form (en enkelt streng pr. kombination)
mikro_names <- sapply(mikro_kombinationer, function(x) paste(x, collapse = "_"))

# Tildel kombinationerne som kolonnenavne
colnames(mikro_data_t)[2:length(mikro_data_t)] <- mikro_names[1:(length(mikro_data_t) - 1)]

#################################

# tilføj realvækst til mikro_data_t
mikro_data_t <- cbind(mikro_data_t[, 1], realforbrug_data_filtreret$Realvækst, mikro_data_t[, 2:ncol(mikro_data_t)])
mikro_data_t[ , 2:ncol(mikro_data_t)] <- sapply(mikro_data_t[ , 2:ncol(mikro_data_t)], as.numeric)

colnames(mikro_data_t)[1] <- "TID"
colnames(mikro_data_t)[2] <- "Realvækst"


# laver en liste til at gemme R²-værdierne
mikro_r2_values <- list()

# Loop gennem hver indikator (kolonne) og beregn R²-værdien
for (i in 3:ncol(mikro_data_t)) {
  mikro_names <- names(mikro_data_t)[i]
  # Kør lineær regression mellem Realvækst og den aktuelle indikator
  model <- lm(Realvækst ~ mikro_data_t[[mikro_names]], data = mikro_data_t)
  # Gem R²-værdien
  mikro_r2_values[[mikro_names]] <- summary(model)$r.squared
}

# Konverter R²-værdierne til en dataframe for nem sammenligning
mikro_r2_df <- data.frame(Indicator = names(mikro_r2_values), R2 = unlist(mikro_r2_values))
mikro_r2_df$R2 <- round(mikro_r2_df$R2, digits = 3)

mikro_r2_df <- mikro_r2_df[order(-mikro_r2_df$R2), ] # her laves der en rangliste over r2 værdierne fra højest til lavest. 

###############
# Bedst mikro R2
print(mikro_r2_df)[1,] # 0.346 - Større køb (fremtid)

