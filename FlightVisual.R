ATL <- read.csv("ATL.csv")
head(ATL)

# Load required libraries
library(ggplot2)
library(dplyr)

# Assuming your data frame is named ATL
# Sample 10% of the data
sampled_ATL <- ATL[sample(nrow(ATL), nrow(ATL) * 0.01), ]

write.csv(sampled_ATL, "sampled_ATL.csv", row.names = FALSE)

# Convert dates to proper format
sampled_ATL$X2022.04.16 <- as.Date(sampled_ATL$X2022.04.16)

sampled_ATL$X2022.04.17 <- as.Date(sampled_ATL$X2022.04.17)

# Rename columns for clarity
colnames(sampled_ATL) <- c("Search_Date", "Flight_Date", "Departure_City", "Arrival_City", "Duration", "Price", "Seats", "Flight_No", "Airline", "Class")

# Plotting price trend over time from search date to flight date
ggplot(sampled_ATL, aes(x = Search_Date, y = Price, color = Flight_Date)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Flight Price Trend from Search Date to Flight Date (Sampled Data)",
       x = "Search Date",
       y = "Price ($)",
       color = "Flight Date") +
  theme_minimal()

# Plotting price distribution by airline
ggplot(sampled_ATL, aes(x = Airline, y = Price, fill = Airline)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Airline (Sampled Data)",
       x = "Airline",
       y = "Price ($)",
       fill = "Airline") +
  theme_minimal()

# Plotting price distribution by class
ggplot(sampled_ATL, aes(x = Class, y = Price, fill = Class)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Class (Sampled Data)",
       x = "Class",
       y = "Price ($)",
       fill = "Class") +
  theme_minimal()

#################################################################

file_name <- paste0("./", airport, ".csv")

library(ggplot2)
library(dplyr)
library(caret)
library(DALEX)

# Read the data from the CSV file
data <- read.csv("ATL.csv")

# Preprocess and prepare the data
colnames(data) <- c("Search_Date", "Flight_Date", "Departure_City", "Arrival_City", "Duration", "Price", "Seats", "Flight_No", "Airline", "Class")
data$Search_Date <- as.Date(data$Search_Date)
data$Flight_Date <- as.Date(data$Flight_Date)
data$Days_In_Advance <- as.integer(data$Flight_Date - data$Search_Date)
data$Arrival_City <- as.factor(data$Arrival_City)
data$Flight_No <- as.factor(data$Flight_No)
data$Airline <- as.factor(data$Airline)
data$Class <- as.factor(data$Class)
data$Seats <- as.numeric(data$Seats)
data$Flight_Legs <- sapply(strsplit(as.character(data$Airline), "\\|\\|"), length)

library(corrplot)

# Calculate correlation matrix for numerical variables only
numerical_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numerical_data, use = "complete.obs")


# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.cex = 0.8, tl.srt = 45)

# Sample 10% of the data
sampled_data <- data %>% sample_frac(0.05)

# Remove rows with any missing values
sampled_data <- na.omit(sampled_data)

# Prepare data for modeling
x_data <- sampled_data %>% select(-Departure_City, -Duration, -Search_Date, -Flight_Date, -Flight_No, -Seats)

# Fit a GBM model
set.seed(123)  # Ensure reproducibility
train_control <- trainControl(method = "cv", number = 5)
model <- train(Price ~ ., data = x_data, method = "gbm", trControl = train_control, verbose = FALSE)

# Create an explainer using DALEX
explainer <- explain(model = model, data = x_data %>% select(-Price), y = sampled_data$Price)

# Compute the Ceteris Paribus profiles
profile <- model_profile(explainer, variables = "Days_In_Advance")

# Plot CP profiles and save the plot
plot_name <- paste0(airport, "_CP_Profile.png")
cp_plot <- plot(profile, geom = "profiles") + 
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_rect(fill = NA, color = "#0c0c0c", size = 0.6),
    legend.position = "bottom"
  )
ggsave(plot_name, cp_plot, width = 10, height = 8)