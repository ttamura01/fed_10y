
library(tidyverse)
library(lubridate)
library(ggtext)

fed_rate <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=FEDFUNDS&scale=left&cosd=1954-07-01&coed=2024-07-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-08-30&revision_date=2024-08-30&nd=1954-07-01") %>% 
  rename_all(tolower)

treasury_note <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DGS10&scale=left&cosd=1962-01-02&coed=2024-08-28&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-08-30&revision_date=2024-08-30&nd=1962-01-02") %>% 
  rename_all(tolower) %>% 
  rename(tn_10y = dgs10) 

treasury_note %>% 
  mutate(tn_10y = as.numeric(tn_10y, na.rm = TRUE))

#problematic_rows <- treasury_note %>% 
  filter(is.na(as.numeric(tn_10y)))

#print(problematic_rows)

treasury_note <- treasury_note %>% 
  filter(!is.na(tn_10y)) %>% 
  mutate(tn_10y = suppressWarnings(as.numeric(tn_10y)))


mortgage <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MORTGAGE30US&scale=left&cosd=1971-04-02&coed=2024-08-29&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Thursday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-08-30&revision_date=2024-08-30&nd=1971-04-02") %>% 
  rename_all(tolower)

fedfunds_data <- fed_rate %>% 
  mutate(date = ymd(date)) 

treasury_note_data <- treasury_note %>% 
  mutate(date = ymd(date)) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(tn_10y = mean(tn_10y, na.rm = TRUE))

mortgage_data <- mortgage %>% 
  mutate(date = ymd(date)) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(mortgage30us = mean(mortgage30us, na.rm = TRUE))

merged_data <- mortgage_data %>% 
  inner_join(fedfunds_data, by = c("month" = "date")) %>% 
  inner_join(., treasury_note_data, by = "month") %>% 
  filter(month >= "1971-01-01" & month < "2025-01-01") 

updates <- tribble(~ month, ~mortgage30us, ~fedfunds, ~tn_10y,
                   "2024-08-01", 6.35, 5.33, 3.87)

merged_data <- rbind(merged_data, updates)

tail(merged_data)

long_data <- merged_data %>% 
  pivot_longer(cols = -month, names_to = "rates", values_to = "percentage")

ggplot(long_data, aes(x = month, y = percentage, color = rates)) +
  geom_line() +
  labs(title = "Historical relation between Fedfund Rate, US 10-year Treasury yield, and 30-year mortgage rate",
       x = NULL, y = NULL,
       caption = "Source: FRED(Federal Reserve Economic Data), WSJ"
       ) +
  theme(legend.key = element_blank(),
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.title = element_textbox_simple()
  ) 

ggsave("correlation_fedfund_treasury_mortgage.png")

ggplot(merged_data, aes(x = fedfunds, y = mortgage30us)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue")

lm_model <- lm(mortgage30us ~ fedfunds, merged_data)

summary(lm_model)

r_squared <- summary(lm_model)$r.squared

coefficients <- coef(lm_model)

intercept <- coefficients[1]
slope <- coefficients[2]

ggplot(merged_data, aes(x = fedfunds, y = mortgage30us)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = paste("Correlation Between US 30-Year Mortgage Rate and US Fed Rate (R-squared:", round(r_squared, 2), ")"),
       x = "US Fed Funds Rate",
       y = "US 30-Year Mortgage Rate",
       caption = "Source: FRED(Federal Reserve Economic Data), WSJ") +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple()
 ) 

ggsave("correlation_fedfund_mortgage.png")

ggplot(merged_data, aes(x = tn_10y, y = mortgage30us)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue")

lm_model <- lm(mortgage30us ~ tn_10y, merged_data)

summary(lm_model)

r_squared <- summary(lm_model)$r.squared

coefficients <- coef(lm_model)

intercept <- coefficients[1]
slope <- coefficients[2]

ggplot(merged_data, aes(x = tn_10y, y = mortgage30us)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = paste("Correlation Between US 30-Year Mortgage Rate and US 10-year treasury yield (R-squared:", round(r_squared, 2), ")"),
       x = "US 10-year Treasury Yield",
       y = "US 30-Year Mortgage Rate",
       caption = "Source: FRED(Federal Reserve Economic Data by St.Louis FED), WSJ") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  ) 

ggsave("correlation_10y_mortgage.png")
