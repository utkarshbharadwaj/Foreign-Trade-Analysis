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

# 4. Prepare Data
df_idea5 <- df_clean %>%
  filter(Year >= 2011 & Year <= 2019) %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  mutate(Exports_Nonoil_Constant = Exports_Nonoil_USD * (base_cpi / CPI_Value)) %>%
  select(Date, Year, Exports_Nonoil_Constant) %>%
  drop_na() %>%
  mutate(Era = ifelse(Year <= 2014, "Era 1: Pre-Policy (2011-2014)", "Era 2: Make in India (2015-2019)")) %>%
  arrange(Date)

# 5. Calculate CAGR Instead of T-Test
annual_exports <- df_idea5 %>%
  group_by(Year, Era) %>%
  summarise(Total_Export = sum(Exports_Nonoil_Constant, na.rm = TRUE), .groups = 'drop')

# CAGR = (End Value / Start Value)^(1/Years) - 1
val_2011 <- annual_exports$Total_Export[annual_exports$Year == 2011]
val_2014 <- annual_exports$Total_Export[annual_exports$Year == 2014]
cagr_era1 <- ((val_2014 / val_2011)^(1/3) - 1) * 100

val_2015 <- annual_exports$Total_Export[annual_exports$Year == 2015]
val_2019 <- annual_exports$Total_Export[annual_exports$Year == 2019]
cagr_era2 <- ((val_2019 / val_2015)^(1/4) - 1) * 100

cat("\n--- CAGR ANALYSIS (CONSTANT USD) ---\n")
cat(sprintf("Era 1 (2011-2014) CAGR: %.2f%%\n", cagr_era1))
cat(sprintf("Era 2 (2015-2019) CAGR: %.2f%%\n", cagr_era2))

# 6. Visualization 5: Segmented Trend Analysis
plot_5 <- ggplot(df_idea5, aes(x = Date, y = Exports_Nonoil_Constant, color = Era)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "Assessing Structural Manufacturing Initiatives",
    subtitle = "Comparing Non-Oil Exports before and after 'Make in India' (Excluding COVID-19)",
    x = "Year",
    y = "Non-Oil Exports (Dec 2025 USD Millions)"
  ) +
  scale_color_manual(name = "Policy Era:", 
                     values = c("Era 1: Pre-Policy (2011-2014)" = "#EF476F", 
                                "Era 2: Make in India (2015-2019)" = "#118AB2")) +
  geom_vline(xintercept = as.Date("2014-12-31"), linetype = "dashed", color = "black", linewidth = 1) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot_5)