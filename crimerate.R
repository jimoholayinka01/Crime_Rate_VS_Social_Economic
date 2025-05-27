
install.packages("dplyr")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("plotly")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("RColorBrewer")
install.packages("reshape2")

library(dplyr)
library(Hmisc)
library(corrplot)
library(plotly)
library(ggpubr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(RColorBrewer)

crime_socio<- read.csv("data/crime_vs_socioeconomic_factors.csv")
View(crime_socio)

#Checking for duplicates using the Region
crime_socio%>%
  group_by(Region) %>%
  filter(n() > 1)

#Checking for NA or Null values
any(is.na(crime_socio))

#Checking for outliers
# Select only numeric columns
numeric_cols <- sapply(crime_socio, is.numeric)
crime_socio_numeric <- crime_socio[, numeric_cols]

# Create a boxplot
boxplot(crime_socio_numeric,
        main = "Boxplot of Numeric Columns in Crime vs Socioeconomic Dataset",
        las = 2)

#Data structure and type check
crime_soc_ecn <- crime_socio
str(crime_soc_ecn)

summary(crime_soc_ecn)

describe(crime_soc_ecn)


lowest_edu <- crime_soc_ecn %>%
  arrange(Education_Level) %>%
  slice_head(n = 15)

ggplot(lowest_edu, aes(x = reorder(Region, Education_Level), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Crime rate of 15 Regions with Lowest Education Levels",
       x = "Region",
       y = "Education Level") +
  theme_minimal()

lowest_emp <- crime_soc_ecn %>%
  arrange(Employment_Rate) %>%
  slice_head(n = 15)

ggplot(lowest_emp, aes(x = reorder(Region, Employment_Rate), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Lowest Employment Rate",
       x = "Region",
       y = "Employment Rate") +
  theme_minimal()

lowest_med <- crime_soc_ecn %>%
  arrange(Median_Income) %>%
  slice_head(n = 15)

ggplot(lowest_med, aes(x = reorder(Region, Median_Income), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Lowest Median Income",
       x = "Region",
       y = "Median Income") +
  theme_minimal()

lowest_pov <- crime_soc_ecn %>%
  arrange(Poverty_Rate) %>%
  slice_head(n = 15)

ggplot(lowest_emp, aes(x = reorder(Region, Poverty_Rate), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Lowest Poverty Rate",
       x = "Region",
       y = "Poverty Rate") +
  theme_minimal()


# Sort in descending order and get the top 20
highest_edu <- crime_soc_ecn %>%
  arrange(desc(Education_Level)) %>%
  slice_head(n = 15)

# Plot
ggplot(highest_edu, aes(x = reorder(Region, Education_Level), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Highest Education Levels",
       x = "Region",
       y = "Education Level") +
  theme_minimal()
# Sort in descending order and get the top 20
highest_emp <- crime_soc_ecn %>%
  arrange(desc(Employment_Rate)) %>%
  slice_head(n = 15)

# Plot
ggplot(highest_emp, aes(x = reorder(Region, Employment_Rate), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Highest Employment Rate",
       x = "Region",
       y = "Employment Rate") +
  theme_minimal()

# Sort in descending order and get the top 20
highest_med <- crime_soc_ecn %>%
  arrange(desc(Median_Income)) %>%
  slice_head(n = 15)

# Plot
ggplot(highest_emp, aes(x = reorder(Region, Median_Income), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Highest Median Income",
       x = "Region",
       y = "Median Income") +
  theme_minimal()

# Sort in descending order and get the top 20
highest_pov <- crime_soc_ecn %>%
  arrange(desc(Poverty_Rate)) %>%
  slice_head(n = 15)

# Plot
ggplot(highest_emp, aes(x = reorder(Region, Poverty_Rate), y = Crime_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime rate of Regions with Highest Poverty Rate",
       x = "Region",
       y = "Poverty Rate") +
  theme_minimal()


# Step 1: Create groups
top_edu <- crime_soc_ecn %>%
  arrange(desc(Education_Level)) %>%
  slice_head(n = 20) %>%
  mutate(Edu_Group = "High Education")

bottom_edu <- crime_soc_ecn %>%
  arrange(Education_Level) %>%
  slice_head(n = 20) %>%
  mutate(Edu_Group = "Low Education")

# Step 2: Combine both groups
edu_groups <- bind_rows(top_edu, bottom_edu)

# Step 3: Boxplot to compare Crime Rates
ggplot(edu_groups, aes(x = Edu_Group, y = Crime_Rate, fill = Edu_Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Crime Rate in Regions with High vs Low Education Levels",
       x = "Education Level Group",
       y = "Crime Rate") +
  theme_minimal()


ggplot(edu_groups, aes(x = reorder(Region, Crime_Rate), y = Crime_Rate, fill = Edu_Group)) +
  geom_col() +
  facet_wrap(~ Edu_Group, scales = "free") +
  coord_flip() +
  labs(title = "Crime Rates by Region (High vs Low Education Levels)",
       x = "Region",
       y = "Crime Rate") +
  theme_minimal()

# Step 1: Create groups
top_emply <- crime_soc_ecn %>%
  arrange(desc(Employment_Rate)) %>%
  slice_head(n = 20) %>%
  mutate(Emp_Group = " Highest Employment Rate")

bottom_emply <- crime_soc_ecn %>%
  arrange(Employment_Rate) %>%
  slice_head(n = 20) %>%
  mutate(Emp_Group = "Lowest Employment Rate")

# Step 2: Combine both groups
emply_groups <- bind_rows(top_emply, bottom_emply)

# Step 3: Boxplot to compare Crime Rates
ggplot(emply_groups, aes(x = Emp_Group, y = Crime_Rate, fill = Emp_Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Crime Rate in Regions with High vs Low Employment rate",
       x = "Employement Level Group",
       y = "Crime Rate") +
  theme_minimal()

ggplot(emply_groups, aes(x = reorder(Region, Crime_Rate), y = Crime_Rate, fill = Emp_Group)) +
  geom_col() +
  facet_wrap(~ Emp_Group, scales = "free") +
  coord_flip() +
  labs(title = "Crime Rates by Region (High vs Low Education Levels)",
       x = "Region",
       y = "Crime Rate") +
  theme_minimal()

# Step 1: Create groups
top_poverty <- crime_soc_ecn %>%
  arrange(desc(Poverty_Rate)) %>%
  slice_head(n = 20) %>%
  mutate(Pov_Group = " Highest Poverty Rate")

bottom_poverty <- crime_soc_ecn %>%
  arrange(Poverty_Rate) %>%
  slice_head(n = 20) %>%
  mutate(Pov_Group = "Lowest Poverty Rate")

# Step 2: Combine both groups
poverty_groups <- bind_rows(top_poverty, bottom_poverty)

# Step 3: Boxplot to compare Crime Rates
ggplot(poverty_groups, aes(x = Pov_Group, y = Crime_Rate, fill = Pov_Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Crime Rate in Regions with High vs Low Poverty Rate",
       x = "Poverty Level Group",
       y = "Crime Rate") +
  theme_minimal()

ggplot(poverty_groups, aes(x = reorder(Region, Crime_Rate), y = Crime_Rate, fill = Pov_Group)) +
  geom_col() +
  facet_wrap(~ Pov_Group, scales = "free") +
  coord_flip() +
  labs(title = "Crime Rates by Region (High vs Low Poverty Rate)",
       x = "Region",
       y = "Crime Rate") +
  theme_minimal()




plot_ly(crime_soc_ecn, 
        x = ~Median_Income, 
        y = ~Poverty_Rate, 
        z = ~Crime_Rate, 
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 4, color = ~Crime_Rate, colorscale = 'Viridis')) %>%
  layout(title = "3D Plot: Crime Rate vs Median Income and Poverty Rate")
# Step 1: Create High/Low groups based on median
crime_data_grouped <- crime_soc_ecn %>%
  mutate(
    Poverty_Group = ifelse(Poverty_Rate > median(Poverty_Rate, na.rm = TRUE), "High Poverty", "Low Poverty"),
    Income_Group = ifelse(Median_Income > median(Median_Income, na.rm = TRUE), "High Income", "Low Income")
  )

# Step 2a: Boxplot - Crime Rate vs Poverty Group
ggplot(crime_data_grouped, aes(x = Poverty_Group, y = Crime_Rate, fill = Poverty_Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Crime Rate by Poverty Level",
       x = "Poverty Group",
       y = "Crime Rate") +
  theme_minimal()

# Step 2b: Boxplot - Crime Rate vs Income Group
ggplot(crime_data_grouped, aes(x = Income_Group, y = Crime_Rate, fill = Income_Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Crime Rate by Income Level",
       x = "Income Group",
       y = "Crime Rate") +
  theme_minimal()


plot(crime_soc_ecn$Education_Level, crime_soc_ecn$Crime_Rate,
     main = "Crime Rate vs Education Level", xlab = "Education Level", ylab = "Crime Rate", pch = 19, col = "blue")

plot(crime_soc_ecn$Employment_Rate, crime_soc_ecn$Crime_Rate,
     main = "Crime Rate vs Employment Rate", xlab = "Employment Rate", ylab = "Crime Rate", pch = 19, col = "green")

plot(crime_soc_ecn$Median_Income, crime_soc_ecn$Crime_Rate,
     main = "Crime Rate vs Median Income", xlab = "Median Income", ylab = "Crime Rate", pch = 19, col = "purple")

plot(crime_soc_ecn$Poverty_Rate, crime_soc_ecn$Crime_Rate,
     main = "Crime Rate vs Poverty Rate", xlab = "Poverty Rate", ylab = "Crime Rate", pch = 19, col = "red")

plot(crime_soc_ecn$Population_Density, crime_soc_ecn$Crime_Rate,
     main = "Crime Rate vs Population Density", xlab = "Population Density", ylab = "Crime Rate", pch = 19, col = "orange")

ggplot(crime_soc_ecn, aes(x = Education_Level, y = Crime_Rate)) +
  geom_point(color = "steelblue") +                     # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit (regression line)
  labs(title = "Crime Rate vs Education Level",
       x = "Education Level",
       y = "Crime Rate")

ggplot(crime_soc_ecn, aes(x = Employment_Rate, y = Crime_Rate)) +
  geom_point(color = "green") +                     # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit (regression line)
  labs(title = "Crime Rate vs Employment Rate",
       x = "Employment Rate",
       y = "Crime Rate")

ggplot(crime_soc_ecn, aes(x = Median_Income, y = Crime_Rate)) +
  geom_point(color = "blue") +                     # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit (regression line)
  labs(title = "Crime Rate vs Median Income",
       x = "Median Income",
       y = "Crime Rate")

ggplot(crime_soc_ecn, aes(x = Poverty_Rate, y = Crime_Rate)) +
  geom_point(color = "skyblue") +                     # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit (regression line)
  labs(title = "Crime Rate vs Poverty Rate",
       x = "Poverty Rate",
       y = "Crime Rate")

ggplot(crime_soc_ecn, aes(x = Population_Density, y = Crime_Rate)) +
  geom_point(color = "green") +                     # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit (regression line)
  labs(title = "Crime Rate vs Population Density",
       x = "Population Density",
       y = "Crime Rate")


corr_matrix <- cor(crime_socio_numeric, use = "complete.obs")
corr_matrix["Crime_Rate", ] %>% sort(decreasing = TRUE)
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black")


# Spearman Rank Correlation For nonlinear or non-normally distributed data
cor.test(crime_soc_ecn$Education_Level, crime_soc_ecn$Crime_Rate, method = "spearman")

# Spearman Rank Correlation For nonlinear or non-normally distributed data
cor.test(crime_soc_ecn$Employment_Rate, crime_soc_ecn$Crime_Rate, method = "spearman")

# Spearman Rank Correlation For nonlinear or non-normally distributed data
cor.test(crime_soc_ecn$Median_Income, crime_soc_ecn$Crime_Rate, method = "spearman")

# Spearman Rank Correlation For nonlinear or non-normally distributed data
cor.test(crime_soc_ecn$Poverty_Rate, crime_soc_ecn$Crime_Rate, method = "spearman")

# Spearman Rank Correlation For nonlinear or non-normally distributed data
cor.test(crime_soc_ecn$Population_Density, crime_soc_ecn$Crime_Rate, method = "spearman")

corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black")


hist(crime_soc_ecn$Crime_Rate, main = "Crime Rate Distribution", col = "skyblue", breaks = 20)


hist(crime_soc_ecn$Education_Level, main = "Education level Distribution", col = "skyblue", breaks = 20)
hist(crime_soc_ecn$Employment_Rate, main = "Employment rate Distribution", col = "skyblue", breaks = 20)
hist(crime_soc_ecn$Median_Income, main = "Median income Distribution", col = "skyblue", breaks = 20)
hist(crime_soc_ecn$Poverty_Rate, main = "Poverty rate Distribution", col = "skyblue", breaks = 20)
hist(crime_soc_ecn$Population_Density, main = "Population density Distribution", col = "skyblue", breaks = 20)


#Density plot to check normality visually
ggdensity(crime_soc_ecn$Crime_Rate,
          main = "Normality visuals for Crime Rate",
          xlab = "points")

ggdensity(crime_soc_ecn$Education_Level,
          main = "Normality visuals for Education Level",
          xlab = "points")

ggdensity(crime_soc_ecn$Employment_Rate,
          main = "Normality visuals for Employment Rate",
          xlab = "points")

ggdensity(crime_soc_ecn$Median_Income,
          main = "Normality visuals for Median Income",
          xlab = "points")

ggdensity(crime_soc_ecn$Poverty_Rate,
          main = "Normality visuals for ",
          xlab = "points")

ggdensity(crime_soc_ecn$Population_Density,
          main = "Normality visuals for Education Level",
          xlab = "points")

# Function to group a numeric variable into 3 levels: Low, Medium, High
group_variable <- function(var) {
  cut(var, breaks = quantile(var, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"), include.lowest = TRUE)
}

# Group each variable
crime_soc_ecn$EduGroup <- group_variable(crime_soc_ecn$Education_Level)
crime_soc_ecn$IncomeGroup <- group_variable(crime_soc_ecn$Median_Income)
crime_soc_ecn$PovertyGroup <- group_variable(crime_soc_ecn$Poverty_Rate)
crime_soc_ecn$EmploymentGroup <- group_variable(crime_soc_ecn$Employment_Rate)

# Apply Kruskal-Wallis tests
kw_edu <- kruskal.test(Crime_Rate ~ EduGroup, data = crime_soc_ecn)
kw_income <- kruskal.test(Crime_Rate ~ IncomeGroup, data = crime_soc_ecn)
kw_poverty <- kruskal.test(Crime_Rate ~ PovertyGroup, data = crime_soc_ecn)
kw_employ <- kruskal.test(Crime_Rate ~ EmploymentGroup, data = crime_soc_ecn)

# View results
kw_edu
kw_income
kw_poverty
kw_employ



subset_df <- crime_soc_ecn[, c("Crime_Rate", "Median_Income", "Poverty_Rate")]
melted_df <- melt(subset_df)

ggplot(melted_df, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Distribution of Poverty Rate and Median Income", y = "Value", x = "") +
  theme_minimal()



top_edu <- crime_soc_ecn %>%
  arrange(desc(Education_Level)) %>%
  slice_head(n = 10)

bottom_edu <- crime_soc_ecn %>%
  arrange(Education_Level) %>%
  slice_head(n = 10)

combined <- bind_rows(top_edu, bottom_edu)
combined$Group <- rep(c("Top Education", "Bottom Education"), each = 10)

ggplot(combined, aes(x = reorder(Region, Crime_Rate), y = Crime_Rate, fill = Group)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Group, scales = "free") +
  labs(title = "Crime Rate of Regions with Highest and Lowest Education Levels", x = "Region", y = "Crime Rate")


ggplot(crime_soc_ecn, aes(x = Education_Level, y = Crime_Rate)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Crime Rate vs Education Level", x = "Education Level (%)", y = "Crime Rate")



ggplot(crime_soc_ecn, aes(x = EduGroup, y = Crime_Rate, fill = EduGroup)) +
  geom_boxplot() +
  labs(title = "Crime Rate Across Education Level Groups",
       x = "Education Level Group",
       y = "Crime Rate") +
  theme_minimal()

p1 <- ggplot(crime_soc_ecn, aes(x = EduGroup, y = Crime_Rate, fill = EduGroup)) +
  geom_boxplot() +
  labs(title = "Crime Rate by Education Group", x = "Education Group", y = "Crime Rate") +
  theme_minimal()

p2 <- ggplot(crime_soc_ecn, aes(x = IncomeGroup, y = Crime_Rate, fill = IncomeGroup)) +
  geom_boxplot() +
  labs(title = "Crime Rate by Income Group", x = "Income Group", y = "Crime Rate") +
  theme_minimal()

p3 <- ggplot(crime_soc_ecn, aes(x = PovertyGroup, y = Crime_Rate, fill = PovertyGroup)) +
  geom_boxplot() +
  labs(title = "Crime Rate by Poverty Group", x = "Poverty Group", y = "Crime Rate") +
  theme_minimal()

p4 <- ggplot(crime_soc_ecn, aes(x = EmploymentGroup, y = Crime_Rate, fill = EmploymentGroup)) +
  geom_boxplot() +
  labs(title = "Crime Rate by Employment Group", x = "Employment Group", y = "Crime Rate") +
  theme_minimal()

# Combine all 4
(p1 | p2) / (p3 | p4)

# Optional: Set a consistent theme
custom_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Plot 1: Crime Rate by Education Group
p1 <- ggplot(crime_soc_ecn, aes(x = EduGroup, y = Crime_Rate, fill = EduGroup)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Crime Rate vs. Education Level", x = "Education Group", y = "Crime Rate") +
  custom_theme

# Plot 2: Crime Rate by Income Group
p2 <- ggplot(crime_soc_ecn, aes(x = IncomeGroup, y = Crime_Rate, fill = IncomeGroup)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Crime Rate vs. Income Level", x = "Income Group", y = "Crime Rate") +
  custom_theme

# Plot 3: Crime Rate by Poverty Group
p3 <- ggplot(crime_soc_ecn, aes(x = PovertyGroup, y = Crime_Rate, fill = PovertyGroup)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Crime Rate vs. Poverty Rate", x = "Poverty Group", y = "Crime Rate") +
  custom_theme

# Plot 4: Crime Rate by Employment Group
p4 <- ggplot(crime_soc_ecn, aes(x = EmploymentGroup, y = Crime_Rate, fill = EmploymentGroup)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Crime Rate vs. Employment Rate", x = "Employment Group", y = "Crime Rate") +
  custom_theme

# Combine all 4 plots into a 2x2 grid
combined_plot <- (p1 | p2) / (p3 | p4)

# Show combined plot
print(combined_plot)

ggplot(crime_soc_ecn, aes(x = Poverty_Rate, y = Crime_Rate)) +
  geom_point(color = "#FF7F0E", alpha = 0.5) +
  geom_smooth(method = "loess", color = "black") +
  labs(
    title = "Crime Rate vs. Poverty Rate",
    x = "Poverty Rate (%)",
    y = "Crime Rate"
  ) +
  theme_minimal(base_size = 14)


# Spearman correlation matrix
num_data <- crime_soc_ecn[, sapply(crime_soc_ecn, is.numeric)]
corr_matrix <- cor(num_data, method = "spearman")
melted_corr <- melt(corr_matrix)

ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu", limits = c(-1, 1)) +
  labs(title = "Spearman Rank Correlation Matrix", x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


crime_soc_ecn %>%
  group_by(IncomeGroup) %>%
  summarise(AvgCrime = mean(Crime_Rate)) %>%
  ggplot(aes(x = IncomeGroup, y = AvgCrime, fill = IncomeGroup)) +
  geom_col() +
  labs(
    title = "Average Crime Rate by Income Group",
    x = "Income Group",
    y = "Average Crime Rate"
  ) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal(base_size = 14)

ggplot(crime_soc_ecn, aes(x = EduGroup, y = Crime_Rate, fill = EduGroup)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Crime Rate Across Education Level Groups",
    x = "Education Level Group",
    y = "Crime Rate"
  ) +
  theme_minimal(base_size = 14)

ggplot(crime_soc_ecn, aes(x = IncomeGroup, y = Crime_Rate, fill = IncomeGroup)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  labs(
    title = "Crime Rate Across Income Groups",
    x = "Income Group",
    y = "Crime Rate"
  ) +
  theme_minimal(base_size = 14)

ggplot(crime_soc_ecn, aes(x = PovertyGroup, y = Crime_Rate, fill = PovertyGroup)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Oranges") +
  labs(
    title = "Crime Rate Across Poverty Groups",
    x = "Poverty Group",
    y = "Crime Rate"
  ) +
  theme_minimal(base_size = 14)

ggplot(crime_soc_ecn, aes(x = EmploymentGroup, y = Crime_Rate, fill = EmploymentGroup)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Purples") +
  labs(
    title = "Crime Rate Across Employment Groups",
    x = "Employment Group",
    y = "Crime Rate"
  ) +
  theme_minimal(base_size = 14)

