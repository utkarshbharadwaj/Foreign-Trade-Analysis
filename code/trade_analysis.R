# ==============================================================================
# BEYOND THE DEFICIT: AN EMPIRICAL ANALYSIS OF INDIA'S FOREIGN TRADE (1990-2025)
# Indian Statistical Institute - Group 24
# Master Analysis Script (4-Question Narrative Arc)
# ==============================================================================

# Load required libraries
library(tidyverse)

# --- 1. SET WORKING DIRECTORY ---
# Update this path to where your CSV files are saved
setwd("/home/diazonium1109/Downloads/Assignments/Semester - II/DARP/Project - Group 24/Data")

# --- 2. LOAD AND CLEAN PRIMARY DATA (RBI) ---
rbi_data <- read.csv("RBI_Foreign_Trade_data.csv", skip = 2, header = FALSE)
colnames(rbi_data) <- c("Year", "Month", 
                        "Exports_INR", "Exports_USD", "Exports_Oil_INR", "Exports_Oil_USD", 
                        "Exports_Nonoil_INR", "Exports_Nonoil_USD", 
                        "Imports_INR", "Imports_USD", "Imports_Oil_INR", "Imports_Oil_USD", 
                        "Imports_Nonoil_INR", "Imports_Nonoil_USD", 
                        "TradeBalance_INR", "TradeBalance_USD", "TradeBalance_Oil_INR", "TradeBalance_Oil_USD", 
                        "TradeBalance_Nonoil_INR", "TradeBalance_Nonoil_USD")

# Clean numeric formats and fill years
df_clean <- rbi_data %>%
  fill(Year, .direction = "down") %>%
  mutate(across(-c(Year, Month), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  drop_na(Date)

# --- 3. LOAD AND PREPARE CPI DATA (FRED) ---
cpi_data <- read.csv("CPIAUCSL.csv") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, CPI_Value = CPIAUCSL)

# Base CPI for December 2025 (Constant Dollars)
base_cpi <- 326.031 

# Merge datasets
df_master <- df_clean %>%
  left_join(cpi_data, by = "Date")


# ==============================================================================
# QUESTION 1: THE CURRENCY ILLUSION (Nominal INR vs. Constant USD)
# ==============================================================================
df_q1 <- df_master %>%
  filter(Year >= 1991) %>%
  group_by(Year) %>%
  summarise(
    Annual_Deficit_INR_Nominal = abs(sum(TradeBalance_INR, na.rm = TRUE)),
    Annual_Deficit_USD_Constant = abs(sum(TradeBalance_USD * (base_cpi / CPI_Value), na.rm = TRUE))
  ) %>%
  mutate(
    Index_INR = (Annual_Deficit_INR_Nominal / Annual_Deficit_INR_Nominal[Year == 1991]) * 100,
    Index_USD_Constant = (Annual_Deficit_USD_Constant / Annual_Deficit_USD_Constant[Year == 1991]) * 100
  )

df_q1_long <- df_q1 %>%
  select(Year, Index_INR, Index_USD_Constant) %>%
  pivot_longer(cols = c(Index_INR, Index_USD_Constant), names_to = "Metric", values_to = "Growth_Index")

plot_1 <- ggplot(df_q1_long, aes(x = Year, y = Growth_Index, color = Metric)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Currency Valuation Divergence: The Illusion of the Deficit",
    subtitle = "Comparing Nominal INR Deficit Growth against the Constant USD Deficit (Base 1991 = 100)",
    x = "Year", y = "Deficit Growth Index (1991 = 100)"
  ) +
  scale_color_manual(name = "Denomination:", 
                     labels = c("Nominal INR (Not Adjusted)", "Constant USD (Inflation-Adjusted)"),
                     values = c("Index_INR" = "#EF476F", "Index_USD_Constant" = "#118AB2")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("plot_1.png", plot = plot_1, width = 10, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# QUESTION 2: THE ENERGY DEFICIT (Overall vs. Non-Oil Balance)
# ==============================================================================
df_q2 <- df_master %>%
  filter(Year >= 2011) %>%
  mutate(
    TB_Overall_Constant = TradeBalance_USD * (base_cpi / CPI_Value),
    TB_Nonoil_Constant = TradeBalance_Nonoil_USD * (base_cpi / CPI_Value)
  ) %>%
  select(Date, TB_Overall_Constant, TB_Nonoil_Constant) %>%
  pivot_longer(cols = c(TB_Overall_Constant, TB_Nonoil_Constant), names_to = "Balance_Type", values_to = "Value")

plot_2 <- ggplot(df_q2, aes(x = Date, y = Value, color = Balance_Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8, alpha = 0.6) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "The Energy Gap: Overall vs. Non-Oil Trade Balance (2011-2025)",
    subtitle = "Isolating the physical burden of crude oil imports (Constant Dec 2025 USD)",
    x = "Year", y = "Real Trade Balance (USD Millions)"
  ) +
  scale_color_manual(name = "Trade Component:", 
                     labels = c("Non-Oil Balance", "Overall Balance (Incl. Oil)"),
                     values = c("TB_Nonoil_Constant" = "#06D6A0", "TB_Overall_Constant" = "#EF476F")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("plot_2.png", plot = plot_2, width = 10, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# QUESTION 3: POLICY IMPACT ("Make in India" Segmented Regression)
# ==============================================================================
df_q3 <- df_master %>%
  filter(Year >= 2011 & Year <= 2019) %>%
  mutate(Exports_Nonoil_Constant = Exports_Nonoil_USD * (base_cpi / CPI_Value)) %>%
  mutate(Era = ifelse(Year <= 2014, "Era 1: Pre-Policy (2011-2014)", "Era 2: Make in India (2015-2019)"))

# Calculate CAGR for the Console Output
annual_q3 <- df_q3 %>%
  group_by(Year) %>%
  summarise(Total_Export = sum(Exports_Nonoil_Constant, na.rm = TRUE))

cagr_e1 <- ((annual_q3$Total_Export[annual_q3$Year==2014] / annual_q3$Total_Export[annual_q3$Year==2011])^(1/3) - 1) * 100
cagr_e2 <- ((annual_q3$Total_Export[annual_q3$Year==2019] / annual_q3$Total_Export[annual_q3$Year==2015])^(1/4) - 1) * 100

cat("\n--- Q3: MAKE IN INDIA CAGR (CONSTANT USD) ---\n")
cat(sprintf("Era 1 (2011-2014) Export CAGR: %.2f%%\n", cagr_e1))
cat(sprintf("Era 2 (2015-2019) Export CAGR: %.2f%%\n", cagr_e2))

plot_3 <- ggplot(df_q3, aes(x = Date, y = Exports_Nonoil_Constant, color = Era)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +
  geom_vline(xintercept = as.Date("2014-12-31"), linetype = "dashed", color = "black", linewidth = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Assessing Structural Manufacturing Initiatives",
    subtitle = "Comparing Non-Oil Exports before and after 'Make in India' (Excluding COVID-19)",
    x = "Year", y = "Non-Oil Exports (Dec 2025 USD Millions)"
  ) +
  scale_color_manual(name = "Policy Era:", 
                     values = c("Era 1: Pre-Policy (2011-2014)" = "#EF476F", "Era 2: Make in India (2015-2019)" = "#118AB2")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("plot_3.png", plot = plot_3, width = 10, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# QUESTION 4: THE DEPENDENCY TRAP (Non-Oil Import vs Export Gap)
# ==============================================================================
df_q4 <- df_master %>%
  filter(Year >= 2011 & Year <= 2024) %>%
  mutate(
    Exp_Constant = Exports_Nonoil_USD * (base_cpi / CPI_Value),
    Imp_Constant = Imports_Nonoil_USD * (base_cpi / CPI_Value)
  )

# Calculate Physical Growth and Gap for the Console Output
annual_q4 <- df_q4 %>%
  group_by(Year) %>%
  summarise(Total_Exp = sum(Exp_Constant, na.rm=T), Total_Imp = sum(Imp_Constant, na.rm=T))

c_exp <- ((annual_q4$Total_Exp[annual_q4$Year==2024] / annual_q4$Total_Exp[annual_q4$Year==2011])^(1/13) - 1) * 100
c_imp <- ((annual_q4$Total_Imp[annual_q4$Year==2024] / annual_q4$Total_Imp[annual_q4$Year==2011])^(1/13) - 1) * 100
gap_11 <- annual_q4$Total_Imp[annual_q4$Year==2011] - annual_q4$Total_Exp[annual_q4$Year==2011]
gap_24 <- annual_q4$Total_Imp[annual_q4$Year==2024] - annual_q4$Total_Exp[annual_q4$Year==2024]
gap_exp <- ((gap_24 / gap_11) - 1) * 100

cat("\n--- Q4: SELF-RELIANCE VS DEPENDENCY (CONSTANT USD) ---\n")
cat(sprintf("Non-Oil Exports CAGR (2011-2024): %.2f%%\n", c_exp))
cat(sprintf("Non-Oil Imports CAGR (2011-2024): %.2f%%\n", c_imp))
cat(sprintf("Physical Gap Expansion: %.2f%%\n\n", gap_exp))

plot_4 <- ggplot(df_q4, aes(x = Date)) +
  geom_ribbon(aes(ymin = Exp_Constant, ymax = Imp_Constant), fill = "#EF476F", alpha = 0.25) +
  geom_line(aes(y = Imp_Constant, color = "Non-Oil Imports"), linewidth = 1.2) +
  geom_line(aes(y = Exp_Constant, color = "Non-Oil Exports"), linewidth = 1.2) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Manufacturing Self-Reliance vs. Import Dependency (2011-2024)",
    subtitle = "Analyzing the widening structural gap in core manufacturing (Constant Dec 2025 USD)",
    x = "Year", y = "Trade Volume (USD Millions)"
  ) +
  scale_color_manual(name = "Trade Flow:", 
                     values = c("Non-Oil Imports" = "#EF476F", "Non-Oil Exports" = "#118AB2")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("plot_4.png", plot = plot_4, width = 10, height = 6, dpi = 300, bg = "white")

cat("All 4 plots successfully generated and saved to the working directory!\n")