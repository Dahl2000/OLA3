###5,1 OLA 3 #### 

library(httr) 

library(jsonlite) 


##API endpoint# 

url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items" 

###API Nøgle  

api_key <- "b4e88217-3210-45fb-b66d-ad0a3cf9f326" 

# Send GET-anmodning til API'et 

response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key)) 



### Tjek koden 

status_code(response) 



#Parse JSON-indholdet 

data <- fromJSON(content(response, "text")) 



## Vis datastrukturen 

str(data) 



#Ekstraher relevante felter fra dataene 

observations <- data$features 



# Kontroller enheden for sigtbarhed (visibility) 

visibility_data <- observations[observations$properties$parameterId == "visibility", ] 

print(visibility_data) 



## Kontroller data for maksimal vindhastighed (wind_max) og opdateringsfrekvens 

wind_max_data <- observations[observations$properties$parameterId == "wind_gust_always_past1h", ] 

print(wind_max_data)
# 5.4 (1)📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊

library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Definer dine stationId'er
station_ids <- c("06074", "06079")  # Tilføj flere stationer, hvis nødvendigt

# Opret en tom liste til at gemme data fra hver station
data_list <- list()

# API nøgle
api_key <- "9eaa8b44-d20c-4910-aa38-762e9eced7f3"

# Loop igennem hver station og hent data
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_dir&datetime=2022-11-01T00:00:00Z/2022-11-30T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Kombiner alle data frames i én samlet data frame
combined_data <- bind_rows(data_list)

# Opret en ny kolonne med kun datoen fra observed (fjerner klokkeslæt)
combined_data <- combined_data %>%
  mutate(date = as.Date(properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"))

# Beregn gennemsnitlig vindretning per dag og per station
daily_wind_dir <- combined_data %>%
  group_by(date, stationId = properties$stationId) %>%
  summarize(avg_wind_dir = mean(properties$value, na.rm = TRUE))  # Gennemsnit pr. dag

# Konverter stationId til numerisk, hvis det er en faktor eller karakter
daily_wind_dir <- daily_wind_dir %>%
  mutate(stationId = as.numeric(as.factor(stationId)),  # Konverter til numerisk
         angle_rad = avg_wind_dir * pi / 180,           # Konverter til radianer
         x_end = cos(angle_rad) * 0.5,                  # Beregn x-komponenten og juster længde
         y_end = sin(angle_rad) * 0.5)                  # Beregn y-komponenten og juster længde

# Plot pilene med stationId som en faktor på y-aksen
ggplot(daily_wind_dir, aes(x = date, y = stationId)) +
  geom_segment(aes(xend = date + x_end,        # Juster pilens længde på x-aksen
                   yend = as.numeric(stationId) + y_end,  # Juster pilens længde på y-aksen
                   x = date, 
                   y = as.numeric(stationId)),
               arrow = arrow(length = unit(0.15, "inches")), # Tilføj pilehoved
               color = "blue") +
  labs(title = "Vindretning per Dag i November 2022",
       x = "Dato",
       y = "Station ID") +
  theme_minimal()
#5,4 (1)📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊📊
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Definer dine stationId'er Aarhus og Anholt
station_ids <- c("06074", "06079")  # Tilføj flere stationer, hvis nødvendigt

# Opret en tom liste til at gemme data fra hver station
data_list <- list()

# API nøgle
api_key <- "9eaa8b44-d20c-4910-aa38-762e9eced7f3"

# Loop igennem hver station og hent data for vindretning
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_dir_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Kombiner alle data frames i én samlet data frame
combined_data <- bind_rows(data_list)

# Opret en ny kolonne med kun datoen fra observed (fjerner klokkeslæt)
combined_data <- combined_data %>%
  mutate(date = as.Date(properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"))

# Beregn gennemsnitlig vindretning per dag og per station
daily_wind_dir <- combined_data %>%
  group_by(date, stationId = properties$stationId) %>%
  summarize(avg_wind_dir = mean(properties$value, na.rm = TRUE))  # Gennemsnit pr. dag

# Konverter stationId til numerisk, hvis det er en faktor eller karakter
daily_wind_dir <- daily_wind_dir %>%
  mutate(stationId = as.numeric(as.factor(stationId)),  # Konverter til numerisk
         angle_rad = avg_wind_dir * pi / 180,           # Konverter til radianer
         x_end = cos(angle_rad) * 0.5,                  # Beregn x-komponenten og juster længde
         y_end = sin(angle_rad) * 0.5)                  # Beregn y-komponenten og juster længde



# Loop igennem hver station og hent data for vindstyrke
for (station in station_ids) {
  # Definer URL med den aktuelle stationId
  url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
                "parameterId=wind_max_per10min_past1h&datetime=2023-10-01T00:00:00Z/2023-10-31T23:59:59Z&stationId=", station)
  
  # Send GET-anmodning
  response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
  
  # Parse JSON-indhold og gem i listen
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_list[[station]] <- as.data.frame(data$features)
}

# Opret en ny data frame til vindstyrke ved at kombinere data for alle stationer fra data_list
combined_data_strength <- bind_rows(data_list)

# Opret en ny kolonne med kun datoen fra 'observed' (fjerner klokkeslæt)
combined_data_strength <- combined_data_strength %>%
  mutate(date = as.Date(properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"))

# Beregn gennemsnitlig vindstyrke per dag og per station
daily_wind_strength <- combined_data_strength %>%
  group_by(date, stationId = properties$stationId) %>%
  summarize(avg_wind_strength = mean(properties$value, na.rm = TRUE))  # Gennemsnit pr. dag


# Opret en data frame til at matche de numeriske og tekst-baserede stationId'er
station_mapping <- data.frame(
  numeric_id = c(1, 2),
  text_id = c("06074", "06079")
)

# Slå de to data frames sammen baseret på matching af de numeriske og tekst ID'er
daily_wind_dir <- daily_wind_dir %>%
  left_join(station_mapping, by = c("stationId" = "numeric_id")) %>%
  rename(stationId_text = text_id)  # Omdøb til stationId_text for at undgå navnekonflikter

# Brug stationId_text i join med daily_wind_strength
combined_data <- left_join(daily_wind_strength, daily_wind_dir, by = c("date", "stationId" = "stationId_text"))

# Variabel for pilens længde
arrow_length <- 0.5

# Plot af vindstyrke og vindretning
ggplot(combined_data, aes(x = date, color = factor(stationId))) +
  # Linjegraf for gennemsnitlig vindstyrke
  geom_line(aes(y = avg_wind_strength), linewidth = 1) +
  
  # Tilføj pile for vindretningen
  geom_segment(
    aes(
      xend = date + arrow_length * cos(angle_rad),     # Ændring i x-retning
      y = avg_wind_strength,
      yend = avg_wind_strength + arrow_length * sin(angle_rad), # Ændring i y-retning
      color = factor(stationId)
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    size = 0.6,
    lineend = "round"
  ) +
  
  # Titel og akse-labels
  labs(
    title = "Daglig Gennemsnitlig Vindstyrke og Vindretning i Oktober 2023",
    x = "Dato - Oktober 2023",
    y = "Vindstyrke (m/s)",
    color = "Station"
  ) +
  
  # Æstetik
  scale_color_manual(values = c("06074" = "blue", "06079" = "hotpink")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )+
  guides(color = guide_legend(title = "Station ID")) # Tilføj en specificeret legend for stationerne
