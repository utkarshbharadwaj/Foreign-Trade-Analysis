# Load the required package
library(tidyverse)

# 1. Set Working Directory
setwd("/home/diazonium1109/Downloads/Assignments/Semester - II/DARP/Project - Group 24")

# 2. Read and Clean RBI Data
rbi_data <- read.csv("RBI_Foreign_Trade_data.csv", skip = 2, header = FALSE)
colnames(rbi_data) <- c("Year", "Month", 
                        "Exports_INR", "Exports_USD", "Exports_Oil_INR", "Exports_Oil_USD", 
                        "Exports_Nonoil_INR", "Exports_Nonoil_USD", 
                        "Imports_INR", "Imports_USD", "Imports_Oil_INR", "Imports_Oil_USD", 
                        "Imports_Nonoil_INR", "Imports_Nonoil_USD", 
                        "TradeBalance_INR", "TradeBalance_USD", "TradeBalance_Oil_INR", "TradeBalance_Oil_USD", 
                        "TradeBalance_Nonoil_INR", "TradeBalance_Nonoil_USD")

df_clean <- rbi_data %>%
  fill(Year, .direction = "down") %>%
  mutate(across(-c(Year, Month), ~ as.numeric(gsub(",", "", .))))

# 3. Load CPI and Calculate
cpi_data <- read.csv("CPIAUCSL.csv") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, CPI_Value = CPIAUCSL)

base_cpi <- 326.031 # Dec 2025 Base

df_idea3 <- df_clean %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  # Convert to Constant USD
  mutate(
    Exports_Constant_USD = Exports_USD * (base_cpi / CPI_Value),
    Imports_Constant_USD = Imports_USD * (base_cpi / CPI_Value)
  ) %>%
  select(Date, Exports_Constant_USD, Imports_Constant_USD) %>%
  drop_na() %>%
  arrange(Date)

# 4. Visualization 3: Crisis Zones
# Pivot for multi-line plotting
df_idea3_long <- df_idea3 %>%
  pivot_longer(cols = c(Exports_Constant_USD, Imports_Constant_USD), 
               names_to = "Trade_Flow", values_to = "Volume")

# Define the boundaries of the three crises
crisis_zones <- data.frame(
  xmin = as.Date(c("1991-01-01", "2008-08-01", "2020-02-01")),
  xmax = as.Date(c("1992-12-31", "2010-06-30", "2021-06-30")),
  ymin = c(-Inf, -Inf, -Inf),
  ymax = c(Inf, Inf, Inf)
)

plot_3 <- ggplot() +
  # Draw the shaded crisis regions first so they sit in the background
  geom_rect(data = crisis_zones, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#A78BFA", alpha = 0.2) +
  # Draw the actual trade lines
  geom_line(data = df_idea3_long, aes(x = Date, y = Volume, color = Trade_Flow), linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Trade Velocity & Exogenous Shocks (1990-2025)",
    subtitle = "Assessing physical contraction and recovery rates during global macroeconomic crises",
    x = "Year",
    y = "Trade Volume (Dec 2025 USD Millions)"
  ) +
  scale_color_manual(name = "Trade Flow:", 
                     labels = c("Exports", "Imports"),
                     values = c("Exports_Constant_USD" = "#06D6A0", 
                                "Imports_Constant_USD" = "#EF476F")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))

print(plot_3)