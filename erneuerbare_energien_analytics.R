# Zeitreihenanalyse und Prognose der Stromerzeugung aus Erneuerbaren Energien in Deutschland
# Autor: Jaqueline Lange
# Datum: 23.09.2023
# Daten: Open Power System Data (https://data.open-power-system-data.org/

# 1. Datenaufbereitung

# Automatische Installation und Laden der benötigten Pakete
packages <- c(
  "tidyverse",
  "lubridate",
  "forecast",
  "zoo",
  "tseries",
  "prophet",
  "here"
)

# Fehlende Pakete installieren
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Alle Pakete laden
lapply(packages, library, character.only = TRUE)

# install.packages("devtools") #Installiere devtools falls Probleme bei tidyverse auftreten
#devtools::install_github("r-lib/conflicted")

# Erstellung Plot Ordner und Speicher-Helfer
PLOT_DIR <- if (requireNamespace("here", quietly = TRUE)) {
here("plots")
} else {
  file.path(getwd(), "plots")
}
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# Base-R/Forecast/ACF/STL/Prophet etc.
save_png <- function(filename, expr, width = 1600, height = 1000, res = 150) {
  png(file.path(PLOT_DIR, filename), width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  eval.parent(substitute(expr))
  invisible(file.path(PLOT_DIR, filename))
}

# ggplot & autoplot
gg_save <- function(filename, plot, width = 9, height = 5.5, dpi = 150) {
  ggsave(file.path(PLOT_DIR, filename), plot = plot,
         width = width, height = height, dpi = dpi)
  invisible(file.path(PLOT_DIR, filename))
}

# CSV einlesen

csv_path <- here("data", "time_series_60min_singleindex_DE.csv")
 if (!file.exists(csv_path)) {
   stop("CSV wurde hier nicht gefunden: ", csv_path,
        "\nLege die Datei in den Ordner 'data/' im Projekt-Root oder passe den Pfad an.")
 }
 data <- read_csv(csv_path)

# Datenbereinigung
colSums(is.na(data)) # Anzahl NA der jeweiligen Spalten

data_clean <- data %>%
  rename(datetime = cet_cest_timestamp) %>%  # Zeitstempel umbenennen
  select(datetime, # Relevante Spalten auswählen
         solar = DE_solar_generation_actual,
         wind_onshore = DE_wind_onshore_generation_actual,
         wind_offshore = DE_wind_offshore_generation_actual) %>%
  drop_na() # NA-Werte entfernen

# Basis Plot zur ersten Übersicht erstellen

library(ggplot2)

p_uebersicht <- ggplot(data_clean, aes(x = datetime)) +
  geom_line(aes(y = solar, color = "Solar")) +
  geom_line(aes(y = wind_onshore, color = "Wind Onshore")) +
  geom_line(aes(y = wind_offshore, color = "Wind Offshore")) +
  labs(
    title = "Stromerzeugung mit erneuerbaren Energien in Deutschland",# Haupttitel
    subtitle = "Quelle: Open Power System Data",  # Untertitel
    x = "Datum",
    y = "MWh",
    color = "Energiequelle"
  ) +
  theme_minimal()

print(p_übersicht)
gg_save("uebersicht_zeitreihen.png",p_uebersicht)

# Aggregation täglich (Energiesumme pro Tag in MWh)
daily_data <- data_clean %>%
  mutate(date = as.Date(datetime)) %>% # Datum aus Zeitstempel extrahieren
  group_by(date) %>% # Gruppierung aller Zeilen mit demselben Datum
  summarise( # Summe der Energiequellen pro Tag
    solar = sum(solar, na.rm = TRUE), # tägliche Solarproduktion 
    wind_onshore = sum(wind_onshore, na.rm = TRUE), # tägliche Onshore Windproduktion
    wind_offshore = sum(wind_offshore, na.rm = TRUE), # tägliche Offshore Windproduktion
    .groups = "drop" # Gruppierung aufheben um Fehler durch gruppiertes Tibble nachhaltig zu vermeiden
  )

# Daten in das Long-Format für ggplot2 umwandeln
# Long-Format hier sinnvoll bei einer y-Achse mit mehreren Kategorien
daily_long <- daily_data %>%
  pivot_longer(cols = c(solar, wind_onshore, wind_offshore),
               names_to = "source",
               values_to = "MWh")

# Zeitreihe plotten
p_taeglich <- ggplot(daily_long, aes(x = date, y = MWh, color = source)) +
  geom_line() +
  labs(title = "Tägliche Stromerzeugung aus Solar und Wind in Deutschland",
       subtitle = "Quelle: Open Power System Data",
       x = "Datum", y = "MWh",
       color = "Energiequelle"
  ) +
  scale_color_discrete(
    labels = c("solar"= "Solar",
               "wind_onshore" = "Wind Onshore",
               "wind_offshore" = "Wind Offshore")
  ) +
  theme_minimal()

print(p_täglich)
gg_save("taegliche_zeitreihen.png", p_taeglich)

# Monatliche Durchschnittswerte für Saisonalität
# Aggregation monatlich im Durchschnitt
monthly_avg <- daily_data %>%
  # Extrahiere den Monat aus der Spalte date. Statt Zahlen werden Abkürzungen der Monatsnamen verwendet
  mutate(month = month(date, label = TRUE)) %>%
  # Gruppiere nach Monat
  group_by(month) %>%
  # Berechne den Mittelwert pro Monat für die Energiequellen
  summarise(
    across(c(solar, wind_onshore, wind_offshore), \(x) mean(x, na.rm = TRUE)),
  # Gruppierung aufheben um Fehler durch gruppiertes Tibble nachhaltig zu vermeiden
            .groups = "drop")

# Monatliche Daten in das Long-Format umwandeln für ggplot2
monthly_long <- monthly_avg %>%
  # Welche Spalten verlängert werden
  pivot_longer(cols = c(solar, wind_onshore, wind_offshore),
               names_to = "source", # Neue Spalte für die Kategorien
               values_to = "MWh")  # Neue Spalte für die Werte

p_monatlich <- ggplot(monthly_long, aes(x = month, y = MWh, fill = source)) +
  # Balken für den Vergleich verschiedener Kategorien, ungestapelt
  geom_col(position = "dodge") +
  labs(title = "Monatliche Durchschnittserzeugung aus Solar und Wind in Deutschland (MWh)",
       subtitle = "Saisonale Muster erneuerbarer Energien",
       x = "Monat", y = "Durchschnittliche MWh" ,
       fill = "Energiequelle"
  ) + 
  scale_fill_discrete(
    labels = c(
      "solar" = "Solar",
      "wind_onshore" = "Wind Onshore",
      "wind_offshore" = "Wind Offshore"
    )
  )+
  theme_minimal()

print(p_monatlich)
gg_save("monatliche_zeitreihen.png", p_monatlich)

# 2. Zeitreihenanalyse
# Analyse der täglich summierten Stromerzeugungsdaten da diese eine feinere zeitliche Auflösung
# haben als die Rohdaten aber nicht so grob sind wie die monatlichen Daten
# Trends und saisonale Muster bleiben erhalten

# 2.1 Vorbereitung der Zeitreihen
# Extraktion der Zeitreihen für jede Energiequelle
# Erzeugung von ts-Objekten

solar_ts <- ts(daily_data$solar, start = c(2015, 1), frequency = 365)
wind_on_ts <- ts(daily_data$wind_onshore, start = c(2015, 1), frequency = 365)
wind_off_ts <- ts(daily_data$wind_offshore, start = c(2015, 1), frequency = 365)

# 2.2 Dekomposition (STL = Seasonal-Trend Decomposition using Loess)
# Loess = Lokale Glättungsmethode (LOcal regrESSio)

solar_dec <- stl(solar_ts, s.window = 13)
wind_on_dec <- stl(wind_on_ts, s.window = 13)
wind_off_dec <- stl(wind_off_ts, s.window = 13)

save_png("stl_solar.png",        { plot(solar_dec, main = "STL-Dekomposition: Solar") })
save_png("stl_wind_onshore.png", { plot(wind_on_dec, main = "STL-Dekomposition: Wind Onshore") })
save_png("stl_wind_offshore.png",{ plot(wind_off_dec, main = "STL-Dekomposition: Wind Offshore") })

# 2.3 Stationarität prüfen
# Augmented Dickey-Fuller Test
adf.test(solar_ts) 
adf.test(wind_on_ts) 
adf.test(wind_off_ts) 

#Autokorrelation prüfen

acf(solar_ts, main = "ACF Solar")
acf(wind_on_ts, main = "ACF Wind Onshore")
acf(wind_off_ts, main = "ACF Wind Offshore")

save_png("acf_solar.png",        { acf(solar_ts, main = "ACF Solar") })
save_png("acf_wind_onshore.png", { acf(wind_on_ts, main = "ACF Wind Onshore") })
save_png("acf_wind_offshore.png",{ acf(wind_off_ts, main = "ACF Wind Offshore") })

# Differenzierung
# Nicht stationäre Zeitreihen müssen differenziert werden

solar_ts_diff <- diff(solar_ts)
wind_on_diff <- diff(wind_on_ts)
wind_off_diff <- diff(wind_off_ts)

save_png("acf_solar_diff.png",   { acf(solar_ts_diff, main = "ACF Solar (Differenziert)") })
save_png("acf_wind_on_diff.png", { acf(wind_on_diff,  main = "ACF Wind Onshore (Differenziert)") })
save_png("acf_wind_off_diff.png",{ acf(wind_off_diff, main = "ACF Wind Offshore (Differenziert)") })

# 2.4 Auto-ARIMA Modelle

# SARIMA Modelle mit automatischer Parametersuche
model_sarima_solar <- auto.arima(solar_ts, seasonal = TRUE, # Einbezug Saisonalität
                                 stepwise = TRUE, # schnellere Suche
                                 approximation = TRUE, # Näherungen ein
                                 trace = TRUE) # Übersicht getesteter Modelle
model_sarima_wind_on <- auto.arima(wind_on_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE, trace = TRUE)
model_sarima_wind_off <- auto.arima(wind_off_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE, trace = TRUE)

# Modellzusammenfassungen

summary(model_sarima_solar)
summary(model_sarima_wind_on)
summary(model_sarima_wind_off)

# Residuenanalyse zur Modellvalidierung
checkresiduals(model_sarima_solar)
checkresiduals(model_sarima_wind_on)
checkresiduals(model_sarima_wind_off)

save_png("residuals_sarima_solar.png",    { checkresiduals(model_sarima_solar) })
save_png("residuals_arima_wind_on.png",  { checkresiduals(model_sarima_wind_on) })
save_png("residuals_arima_wind_off.png", { checkresiduals(model_sarima_wind_off) })


# Prognose für ein Jahr (365 Tage)

forecast_sarima_solar <- forecast(model_sarima_solar, h = 365)
forecast_sarima_wind_on <- forecast(model_sarima_wind_on, h = 365)
forecast_sarima_wind_off <- forecast(model_sarima_wind_off, h = 365)

accuracy(forecast_sarima_solar)
accuracy(forecast_sarima_wind_on)
accuracy(forecast_sarima_wind_off)

p_fc_solar <- autoplot(forecast_sarima_solar) +
  labs(title = "SARIMA-Prognose Solarenergie", x = "Datum", y = "MWh")
p_fc_wind_on <- autoplot(forecast_sarima_wind_on) +
  labs(title = "SARIMA-Prognose Windenergie Onshore", x = "Datum", y = "MWh")
p_fc_wind_off <- autoplot(forecast_sarima_wind_off) +
  labs(title = "SARIMA-Prognose Windenergie Offshore", x = "Datum", y = "MWh")

print(p_fc_solar);    gg_save("forecast_sarima_solar.png", p_fc_solar)
print(p_fc_wind_on);  gg_save("forecast_sarima_wind_onshore.png", p_fc_wind_on)
print(p_fc_wind_off); gg_save("forecast_sarima_wind_offshore.png", p_fc_wind_off)


# 2.5 Prophet

#Solar
# Konventierung der Zeitreihe in ein dataframe mit den Spalten ds (Datum) und y (Beobachtung)

df_solar <- daily_data %>%
  select(ds = date, y = solar) %>% # Format YYYY-MM-DD, y numeric
  arrange(ds)
head(df_solar)

# Prophet-Modell mit Jahres- und Wochen-Saisonalität erzeugen

m_solar <- prophet(df_solar,
                   yearly.seasonality = TRUE,
                   weekly.seasonality = FALSE,
                   daily.seasonality = FALSE,
                   interval.width = 0.90, # 90% Konfidenzintervall
                   seasonality.mode = "multiplicative")

# Prognose für 1 Jahr
future_solar <- make_future_dataframe(m_solar, periods = 365)
forecast_p_solar <- predict(m_solar, future_solar)

# Prüfen
head(forecast_p_solar[, c("ds","yhat","yhat_lower","yhat_upper")])

# Plot ganzer Zeitraum
p_prophet_solar <- plot(m_solar, forecast_p_solar) + ggtitle("Prophet-Prognose Solar")
gg_save("forecast_prophet_solar.png", p_prophet_solar)

# Modellgüte prüfen (Kreuzvalidierung)

cv <- cross_validation(
  m_solar,
  initial = 1095,
  period = 180,
  horizon = 365,
  units = "days"
)

# Fehlerkennzahlen berechnen
perf <- performance_metrics(cv)
head(perf)
summary(perf)


# Wind Onshore
# Konvertierung ins Prophet-Format
df_wind_on <- daily_data %>%
  select(ds = date, y = wind_onshore) %>%
  arrange(ds)
head(df_wind_on)

# Prophet-Modell
m_wind_on <- prophet(df_wind_on,
                     yearly.seasonality = TRUE,
                     weekly.seasonality = FALSE,
                     daily.seasonality  = FALSE,
                     interval.width     = 0.90,
                     seasonality.mode = "multiplicative")
          

# 1 Jahr in die Zukunft prognostizieren
future_wind_on <- make_future_dataframe(m_wind_on, periods = 365)
forecast_p_wind_on <- predict(m_wind_on, future_wind_on)

# Vorschau
head(forecast_p_wind_on[, c("ds","yhat","yhat_lower","yhat_upper")])

# Plot über den gesamten Zeitraum
p_prophet_wind_on <- plot(m_wind_on, forecast_p_wind_on) +
  ggtitle("Prophet-Prognose Windenergie Onshore")

gg_save("forecast_prophet_wind_onshore.png", p_prophet_wind_on)

# Cross-Validation (Rolling-Origin)
cv_wind_on <- cross_validation(
  m_wind_on,
  initial = 1095,  # ~3 Jahre Training
  period  = 180,   # alle 6 Monate neuer Cutoff
  horizon = 365,   # 1 Jahr Forecast
  units   = "days"
)

# Fehlerkennzahlen
perf_wind_on <- performance_metrics(cv_wind_on)
head(perf_wind_on)
summary(perf_wind_on)

# Prophet-Modell für Wind Offshore

# Konvertierung ins Prophet-Format
df_wind_off <- daily_data %>%
  select(ds = date, y = wind_offshore) %>%
  arrange(ds)
head(df_wind_off)

# Prophet-Modell
m_wind_off <- prophet(df_wind_off,
                      yearly.seasonality = TRUE,
                      weekly.seasonality = FALSE,
                      daily.seasonality  = FALSE,
                      interval.width     = 0.90,
                      seasonality.mode   = "multiplicative")

# 1 Jahr in die Zukunft prognostizieren
future_wind_off <- make_future_dataframe(m_wind_off, periods = 365)
forecast_p_wind_off <- predict(m_wind_off, future_wind_off)

# Vorschau
head(forecast_p_wind_off[, c("ds","yhat","yhat_lower","yhat_upper")])

# Plot über den gesamten Zeitraum

p_prophet_wind_off <- plot(m_wind_off, forecast_p_wind_off) +
  ggtitle("Prophet-Prognose Windenergie Offshore")

gg_save("forecast_prophet_wind_offshore.png", p_prophet_wind_off)
# Cross-Validation (Rolling-Origin)
cv_wind_off <- cross_validation(
  m_wind_off,
  initial = 1095,  # ~3 Jahre Training
  period  = 180,   # alle 6 Monate neuer Cutoff
  horizon = 365,   # 1 Jahr Forecast
  units   = "days"
)

# Fehlerkennzahlen
perf_wind_off <- performance_metrics(cv_wind_off)
head(perf_wind_off)
summary(perf_wind_off)

