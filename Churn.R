
library(dplyr)
library(tidyverse)
library(broom)
library(ggplot2)
#load the data set 
Chn <- read.csv("Customer-Churn-Records.csv")
#Check how dataset looks like 
head(Chn)
str(Chn)
#Explore the Datset 
summary(Chn)
#Count the number of guest 
No_of_customers<- length(Chn$CustomerId)
print(paste("Total Customers:" ,No_of_customers))
#Count the number of distinct customers 
No_of_uniq_customers<- length(unique(Chn$CustomerId))
print(paste("Total Customers:" ,No_of_uniq_customers))
#Number of churned customer 
No_of_churned_customers <- sum(Chn$Exited)
print(paste("Total Churned customers :" ,No_of_churned_customers))

#Determine churn rate 
Churn_rate <- (No_of_churned_customers /No_of_customers) *100
print(paste("Churn rate :" ,Churn_rate,"%"))

#Average Number of Products 
Avg_number_of_products <- mean(Chn$NumOfProducts)
print(paste("Avg_no_of_products :" ,Avg_number_of_products))

#Churn by age 
Churn_rate_by_age <- aggregate (Exited ~ Age, data= Chn, FUN = function(x) 100* sum(x)/length(x))

print(Churn_rate_by_age)

# Create age bins 
Chn2 <- Chn %>%
  mutate(Age_Bin = cut(Age, breaks = c(0, 18, 25, 35, 50, 60,70,80, Inf), labels = c("<18", "18-25", "26-35", "36-50", "50-60","60-70","70-80","80+")))

# Calculate churn rate for each age bin
churn_rate_by_age_bin <- Chn2 %>%
  group_by(Age_Bin) %>%
  summarise (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n())

# Create a bar plot
ggplot(churn_rate_by_age_bin, aes(x = Age_Bin, y = Churn_Rate, fill = Age_Bin)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = Churn_Rate), vjust = -0.5, size = 3) +
  labs(title = "Churn Rate by Age ",
       x = "Age ",
       y = "CHURN Rate (%)") +
  theme_minimal()

#Churn rate by tenure 
Churn_rate_by_Tenure <- Chn %>% group_by(Tenure) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n())

# Create a bar plot
ggplot(Churn_rate_by_Tenure, aes(x = Tenure, y = Churn_Rate, fill = Tenure)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = Customer_count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Churn Rate and Customer Count by Tenure",
       x = "Tenure",
       y = "Churn Rate (%)") +
  theme_minimal()

#Churn rate by country 
 
Churn_rate_by_Country <- Chn %>% group_by(Geography) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n())
#plot
ggplot(Churn_rate_by_Country, aes(x = Geography)) +
  geom_bar(aes(y =Customer_count ), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_line(aes(y = Churn_Rate * 50), color = "red", linewidth = 1, group = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(~./50, name = "Churn Rate(%)")
  ) +
  labs(
    y = "Customer count",
    x = "Geography",
    title = "Churn Rate and Customer Count by Geography"
  ) +
  theme_minimal()

#Churn rate by Active members 
Churn_rate_by_Active_members <- Chn %>% group_by(IsActiveMember,Complain) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n()) 
#Churn rate by card type
 Churn_rate_by_cardtype <- Chn %>% group_by(Card.Type,Tenure) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n()) 
 df<- as.data.frame( Churn_rate_by_cardtype)

 # Scatter plot
 ggplot(Churn_rate_by_cardtype , aes(x = Tenure, y = Churn_Rate, color = Card.Type, )) +
   geom_point(size = 4) +
   labs(title = "Churn Rate vs Total Card Type",
        x = "Tenure",
        y = "Churn Rate",
        color = "Card.Type"
        ) +
   theme_minimal()
 
 #Churn by number of products 
 Churn_rate_by_no_of_customers <- Chn %>% group_by(NumOfProducts) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n())
 #plot
 ggplot(  Churn_rate_by_no_of_customers, aes(x = NumOfProducts)) +
   geom_bar(aes(y = Customer_count), stat = "identity", fill = "green", alpha = 0.7) +
   geom_line(aes(y = Churn_Rate*50, color = "red"), size = 1) +
   scale_y_continuous(
     name = "Customer Count",
     sec.axis = sec_axis(~./50, name = "Churn Rate")
   ) +
   labs(
     title = "Churn Rate, and Customer Count by NumOfProducts ",
     x = "NumOfProducts",
     y = "Customer Count"
   ) +
   theme_minimal() 
 
 #churn rate by satisfaction score 
 Churn_rate_by_satisfaction_score <- Chn %>% group_by(Satisfaction.Score) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n())
 
 # Plot
 ggplot( Churn_rate_by_satisfaction_score, aes(x = Satisfaction.Score)) +
   geom_bar(aes(y = Customer_count), stat = "identity", fill = "blue", alpha = 0.7) +
   geom_line(aes(y = Churn_Rate*10, color = "red"), size = 1) +
   scale_y_continuous(
     name = "Customer Count",
     sec.axis = sec_axis(~./10, name = "Churn Rate")
   ) +
   labs(
     title = "Churn Rate, and Customer Count by Satisfaction score ",
     x = "Satisfaction Score",
     y = "Customer Count"
   ) +
   theme_minimal() 
 
 #churn rate by points earned 
 # Define breaks for bins
 breaks <- seq(100, 1000, by = 50)
 #Create a new variable with bins
 Chn2$points_bins <- cut(Chn2$Point.Earned, breaks = breaks, include.lowest = TRUE)
 #churn rate by points earned
 Churn_rate_by_points_earned  <- Chn2 %>% group_by(points_bins) %>% summarize  (Churn_Rate = round(sum(Exited) / n() * 100,2),Customer_count=n())
 #plot
 ggplot( Churn_rate_by_points_earned, aes(x = points_bins)) +
   geom_bar(aes(y = Customer_count), stat = "identity", fill = "blue", alpha = 0.7) +
   geom_line(aes(y = Churn_Rate* 7, color = "red"), size = 1,group = 1 ) +
   scale_y_continuous(
     name = "Customer Count",
     sec.axis = sec_axis(~./7, name = "Churn Rate")
   ) +
   labs(
     title = "Churn Rate, and Customer Count by points earned ",
     x = "Points earned(bin)",
     y = "Customer Count"
   ) +
   theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   scale_fill_manual(values = "blue", name = "Customer Count") +
   scale_color_manual(values = "red", name = "Churn Rate") 
   
   
