write_csv(temp_events, "Thermistor_temperature_output_data.csv")

write_csv(signals, "Thermistor_signals_output_data.csv")

temp_events <- read.csv("Thermistor_temperature_output_data.csv", stringsAsFactors = F)

signals <- read.csv("Thermistor_signals_output_data.csv", stringsAsFactors = F)

theme_set(
  theme_minimal()
)

temp_events %>%
  mutate(events = seq(1, n(), 1)) %>%
  ggplot(aes(events, temp)) +
  geom_line() +
  geomp
