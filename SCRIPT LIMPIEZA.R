# Thu May 29 10:57:26 2025 ------------------------------
library(readr)   
Global_YouTube_Statistics <- read_csv("Global YouTube Statistics.csv")
head(Global_YouTube_Statistics) 
table(Global_YouTube_Statistics$category)
table(Global_YouTube_Statistics$Country)
table(Global_YouTube_Statistics$)
table(Global_YouTube_Statistics$Youtuber $Country)
options(scipen = 999)  # evita mostrar en notación científica

españa <-subset(Global_YouTube_Statistics, Country == "Spain", select = c("Youtuber", "Country"))
print(n=23,españa) 
youtube<- na.omit(Global_YouTube_Statistics)


library(dplyr)

european_countries <- c("France", "Sweden", "Ukraine", "Andorra", "Germany", 
                        "Italy", "Latvia", "Switzerland", "Netherlands", 
                        "United Kingdom", "Finland", "Spain")

youtubeEU <- youtube %>%
  mutate(Country = ifelse(Country %in% european_countries, "EU", Country))
table(youtubeEU$Country)
youtubeEU_filtered <- youtubeEU %>%
  filter(Country %in% c("EU", "United States"))
table(youtubeEU_filtered$Country) 

eu_data <- subset(youtubeEU_filtered, Country == "EU")
eu_data <- eu_data %>%
  filter(category != "nan")


analisis_EU <- data.frame(
  Media = mean(eu_data$subscribers, na.rm = TRUE),
  Mediana = median(eu_data$subscribers, na.rm = TRUE),
  Minimo = min(eu_data$subscribers, na.rm = TRUE),
  Maximo = max(eu_data$subscribers, na.rm = TRUE),
  Q1 = quantile(eu_data$subscribers, 0.25, na.rm = TRUE),
  Q3 = quantile(eu_data$subscribers, 0.75, na.rm = TRUE),
  IQR = IQR(eu_data$subscribers, na.rm = TRUE),
  Desviacion = sd(eu_data$subscribers, na.rm = TRUE)
)

print(analisis_EU)
library(ggplot2)

ggplot(eu_data, aes(x = subscribers)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  labs(
    title = "Histograma de Suscriptores (EU)",
    x = "Número de Suscriptores",
    y = "Frecuencia"
  ) +
  theme_minimal()

USA_data <- subset(youtubeEU_filtered, Country == "United States")
ggplot(USA_data, aes(x = subscribers)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  labs(
    title = "Histograma de Suscriptores (EU)",
    x = "Número de Suscriptores",
    y = "Frecuencia"
  ) +
  theme_minimal()


USA_data <- subset(youtubeEU_filtered, Country == "United States")
USA_data <- USA_data %>%
  filter(category != "nan")

ggplot(USA_data, aes(x = `video views`)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 90) +
  labs(
    title = "Histograma de Suscriptores (EU)",
    x = "Número de Suscriptores",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggplot(youtubeEU_filtered, aes(x = Country, y = highest_yearly_earnings, fill = Country)) +
  geom_boxplot() +
  labs(
    title = "Comparación de ingresos anuales máximos entre EU y USA",
    x = "País",
    y = "Highest Yearly Earnings"
  ) +
  theme_minimal()
ggplot(youtubeEU_filtered, aes(x = Country, y = highest_yearly_earnings, color = Country)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(
    title = "Distribución de ingresos anuales máximos por país",
    x = "País",
    y = "Highest Yearly Earnings"
  ) +
  theme_minimal()


library(dplyr)

resumen <- youtubeEU_filtered %>%
  group_by(Country) %>%
  summarise(promedio_ingreso = mean(highest_yearly_earnings, na.rm = TRUE))

ggplot(resumen, aes(x = Country, y = promedio_ingreso, fill = Country)) +
  geom_col() +
  labs(
    title = "Promedio de ingresos anuales máximos por país",
    x = "País",
    y = "Promedio Highest Yearly Earnings"
  ) +
  theme_minimal()

modelo <- lm(highest_yearly_earnings ~ Country, data = youtubeEU_filtered)
summary(modelo)
library(ggplot2)

ggplot(youtubeEU_filtered, aes(x = Country, y = highest_yearly_earnings, fill = Country)) +
  geom_boxplot() +
  labs(
    title = "Comparación de ingresos máximos anuales por país",
    x = "País",
    y = "Ingresos máximos anuales"
  ) +
  theme_minimal()

ggplot(youtubeEU_filtered, aes(x = category, y = subscribers, fill = category)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Distribución de suscriptores por categoría (violín)",
    x = "Categoría",
    y = "Número de Suscriptores"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(USA_data, aes(x = category, y = subscribers, fill = category)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Distribución de suscriptores por categoría (violín)",
    x = "Categoría",
    y = "Número de Suscriptores"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


youtube_visitas <- youtubeEU_filtered %>%
  mutate(views_k = video_views_for_the_last_30_days / 1000)
modelo1 <- lm(subscribers_for_last_30_days ~ video_views_for_the_last_30_days, data =youtubeEU_filtered)
summary(modelo1)
anova_model <- aov(highest_monthly_earnings ~ Country, data = youtubeEU_filtered)
summary(anova_model)

modelo_completo <- lm(highest_yearly_earnings ~ Country + subscribers, data = youtubeEU_filtered)
summary(modelo_completo)


library(ggplot2)

ggplot(youtubeEU_filtered, aes(x = subscribers, y = highest_yearly_earnings, color = Country)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Regresión: Highest Yearly Earnings vs. Subscribers",
    x = "Suscriptores",
    y = "Highest Yearly Earnings"
  ) +
  theme_minimal()

ggplot(youtubeEU_filtered, aes(x = subscribers, y = highest_yearly_earnings, color = Country)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(0, 40000000)) +
  labs(
    title = "Recorte de escala: ingresos hasta 40 millones",
    x = "Suscriptores",
    y = "Highest Yearly Earnings"
  ) +
  theme_minimal()

cate_fil <- youtubeEU_filtered %>%
  filter(category %in% c("News & Politics", "Gaming", "Entertainment"))
catego_sub <- lm(subscribers ~ category + Country, data = cate_fil)
summary(catego_sub)

ggplot(cate_fil %>% filter(category == "Gaming"),
       aes(x = Country, y = subscribers, color = Country)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Suscriptores por país - Categoría: Gaming",
    x = "País",
    y = "Suscriptores"
  ) +
  theme_minimal()

ggplot(cate_fil %>% filter(category == "News & Politics"),
       aes(x = Country, y = subscribers, color = Country)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Suscriptores por país - Categoría: Gaming",
    x = "País",
    y = "Suscriptores"
  ) +
  theme_minimal()

ggplot(cate_fil %>% filter(category == "Entertainment"),
       aes(x = Country, y = subscribers, color = Country)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Suscriptores por país - Categoría: Gaming",
    x = "País",
    y = "Suscriptores"
  ) +
  theme_minimal()