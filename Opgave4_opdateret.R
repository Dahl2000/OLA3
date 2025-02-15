################ OPAGVE 4 ##############
library(dkstat)
library(tidyr)
library(dplyr)
library(lubridate)
#install.packages("pls")
library(pls)


# Dataindhentning fra DST API
alltables <- dst_get_tables(lang = "da")                     # Hent liste af tabeller fra DST API
dst_search("Forbrugerforventninger")                         # Søg efter tabeller relateret til forbrugerforventninger
FORV1.meta <- dst_meta(table = "FORV1", lang = "da")         # Hent metadata for tabellen FORV1

# Definer filterliste for at hente relevant data
FORV1.filter <- list(
  INDIKATOR = "*",                                           # Alle indikatorer
  Tid = "*"                                                  # Alle tidsperioder
)

# Hent data baseret på filteret og omform til bredt format
Forbrugertillid <- dst_get_data(table = "FORV1", query = FORV1.filter, lang = "da")
Forbrugertillidwide <- pivot_wider(Forbrugertillid, names_from = INDIKATOR, values_from = value)

# Datarensning og transformation til kvartalsvis data
Forbrugertillid2024kvartal <- Forbrugertillidwide %>%
  filter(as.Date(TID) >= as.Date("2000-01-01")) %>%          # Filtrer data fra 2000 og frem
  select(-2) %>%                                             # Fjern den samlede FTI (anden kolonne)
  mutate(Quarter = paste0(year(as.Date(TID)), "-Q", quarter(as.Date(TID)))) %>%  # Opret kvartal-kolonne
  group_by(Quarter) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))   # Beregn gennemsnit for hver kvartal

# Konverter tibble til en data frame for at kunne bruge rownames
Forbrugertillid2024kvartal <- as.data.frame(Forbrugertillid2024kvartal)

#retter negativ korrelationsfejl på spørgsmål 6 og 8 

Forbrugertillid2024kvartal$`Priser i dag, sammenlignet med for et år siden` <- Forbrugertillid2024kvartal$`Priser i dag, sammenlignet med for et år siden`*-1
Forbrugertillid2024kvartal$`Arbejdsløsheden om et år, sammenlignet med i dag` <- Forbrugertillid2024kvartal$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1

# Konverter første kolonne ('Quarter') til rækkenavne
rownames(Forbrugertillid2024kvartal) <- Forbrugertillid2024kvartal$Quarter

# Fjern den første kolonne ('Quarter'), da den nu er rækkenavne
Forbrugertillid2024kvartal <- Forbrugertillid2024kvartal[, -1]

###Kombinationsalgoritme

# Udvælg og filtrer kun numeriske kolonner fra 1 til 12
Forbrugertillid2024kvartal_numeric <- Forbrugertillid2024kvartal[, sapply(Forbrugertillid2024kvartal[, 1:12], is.numeric)]

# Opret en tom liste til at gemme resultaterne
kombinationresultat <- list()

# Beregn alle kombinationer og gem gennemsnit
n <- ncol(Forbrugertillid2024kvartal_numeric)
for (k in 1:n) {
  combs <- combn(n, k, simplify = FALSE)
  for (idx in combs) {
    avg_combination <- rowMeans(Forbrugertillid2024kvartal_numeric[, idx, drop = FALSE], na.rm = TRUE)
    comb_name <- paste("Comb", paste(idx, collapse = "-"), sep = "_")
    kombinationresultat[[comb_name]] <- avg_combination
  }
}

# Konverter resultaterne til en dataframe for bedre overskuelighed
results_df <- as.data.frame(kombinationresultat)

######## Opgave 1.2 – R2 og forbrugertillidsindikatorer

# Hent metadata og opsæt filter
Forbrug.meta <- dst_meta(table = "NKHC021", lang = "da")  # Hent metadata for tabellen NKHC021 fra DST

Forbrug.filter <- list(  # Definer et filter for at hente totalforbrug, 2020-priser og sæsonkorrigerede data
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

# Hent data baseret på filteret
Forbrugdata <- dst_get_data(table = "NKHC021", query = Forbrug.filter)  
Forbrugdata <- Forbrugdata[, -c(1:3)]  # Fjern de tre første kolonner med metadata

# Beregning af realvækst
Forbrugdata$Realvækst <- (Forbrugdata$value / dplyr::lag(Forbrugdata$value, 4) - 1) * 100  # Beregn realvækst år-over-år

# Udvælg relevant periode og kolonner (fra 2000Q1 til 2024Q2)
Forbrugdata2024 <- as.data.frame(Forbrugdata[c(41:138), c(1, 3)])  # Vælg rækkerne 41-138 (2000Q1-2024Q2) og kolonnerne TID og Realvækst

# Datoformatering
Forbrugdata2024$TID <- as.Date(Forbrugdata2024$TID)  # Konverter TID til datoformat

# Udtræk år og kvartal fra datoformatet
Forbrugdata2024$TID <- paste0(year(Forbrugdata2024$TID), "-Q", quarter(Forbrugdata2024$TID))  # Opret en 'År-Q'-format kolonne

# Konverter til data frame hvis det er en tibble
Forbrugdata2024 <- as.data.frame(Forbrugdata2024)

# Sæt værdierne i kolonne 1 (TID) som rækkenavne
rownames(Forbrugdata2024) <- Forbrugdata2024$TID

# Fjern den første kolonne (TID) forsigtigt
Forbrugdata2024 <- Forbrugdata2024[, -1, drop = FALSE]  # drop=FALSE sikrer, at vi ikke fjerner hele strukturen

#Lineær regression 

kombinationer2024Q2 <- results_df[1:(nrow(results_df) - 2), ]

combined_data <- cbind(Forbrugdata2024, kombinationer2024Q2)

# Antag, at din dataframe hedder 'combined_data'
realvaekst <- combined_data$Realvækst  # Afhængig variabel

# Fjern 'Realvækst'-kolonnen for at kun have kombinationerne
indicators <- combined_data[, -which(names(combined_data) == "Realvækst")]

# Opret en tom vektor til at gemme R²-værdierne
r2_values <- numeric(ncol(indicators))

# Loop igennem hver kombination og kør regression
for (i in 1:ncol(indicators)) {
  indicator <- indicators[, i]  # Vælg den i'te indikator
  model <- lm(realvaekst ~ indicator)  # Udfør regressionen
  r2_values[i] <- summary(model)$r.squared  # Gem R²-værdien
}

# Lav en tabel med kombinationer og deres tilsvarende R²-værdier
results <- data.frame(Indicator = names(indicators), R2 = r2_values)

# Sorter resultaterne efter R²-værdien
sorted_results <- results[order(-results$R2), ]

# Vis de bedste resultater
print(head(sorted_results))

# Initialiser en liste til at gemme de gennemsnitlige R² værdier og standardafvigelser for hver iteration
combined_data2 <- combined_data
r_squared_summary <- list()
# Gentag processen, indtil der er 63 rækker tilbage
while (nrow(combined_data2) > 63) {
  
  # Initialiser en vektor til at gemme R² værdier for den aktuelle iteration
  opt_r_squared_values <- numeric(ncol(combined_data2)-1)
  combination_names <- names(combined_data2)[-1]  # Navne på kombinationerne
  
  # Loop gennem hver kombination (starter fra kolonne 2, da kolonne 1 er Realvækst)
  for (i in 2:ncol(combined_data2)) {
    # Skab en lineær model med Realvækst som afhængig variabel og hver kolonne som uafhængig variabel
    opt_model <- lm(combined_data2$Realvækst ~ combined_data2[, i])
    
    # Gem R²-værdien i vektoren på index i-1 (for at tilpasse indekseringen)
    opt_r_squared_values[i-1] <- summary(opt_model)$r.squared
  }
  
  r_squared_summary[[length(r_squared_summary) + 1]] <- opt_r_squared_values
  
  # Fjern den sidste række fra combined_data til næste iteration
  combined_data2 <- combined_data2[-nrow(combined_data2), ]
}

# Saml alle iterationers R² statistikker i en enkelt dataframe
r_squared_summary_df <- as.data.frame(r_squared_summary)
row.names(r_squared_summary_df) <- combination_names
numn_columns <- ncol(r_squared_summary_df)
newcolumnnames <- paste0("periode", seq(from=98, by=-1, length.out = numn_columns))
colnames(r_squared_summary_df) <- newcolumnnames

# Udskriv det endelige datasæt
print(r_squared_summary_df)

r_squared_summary_df$Mean_R_squared <- rowMeans(r_squared_summary_df, na.rm = TRUE)
r_squared_summary_df$SD_R_squared <- apply(r_squared_summary_df, 1, sd, na.rm = TRUE)
r_squared_summary_df$Index_Rank <- r_squared_summary_df$Mean_R_squared / r_squared_summary_df$SD_R_squared

