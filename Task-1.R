# Provided data was already imported to: customer_shopping_data

#fix(inv_date)
#plot(customer_shopping_data$quantity ~ inv_date, type="l", col="red")

library(tidyverse)
install.packages("tidyverse")

# idate1 <- customer_shopping_data$invoice_date

# df <- data.frame(date=as.Date(customer_shopping_data$invoice_date, format = "%d/%m/%Y"),
#                 quantity=customer_shopping_data$quantity)


#Filtered data of concern (date and sales quantity) from customer_shopping_data.
df1 <- data.frame(date=as.Date(customer_shopping_data$invoice_date, "%d/%m/%Y"),
                  quantity=df$quantity)



# Grouped data by month and year
result <- df1 %>% count(inv_date = format(date, '%m/%Y'))


# plot(as.numeric(result$inv_date) ~ result$n, type="l", col="red")

library(ggplot2)
# # Time Series Plot
ggplot(result, aes(inv_date, n, fill = inv_date)) + 
  geom_bar(stat = "identity")

 #rm(df)


# # Create a histogram of Distribution of each sales data by sales quantity
hist(customer_shopping_data$age, main="Distribution of each sales data by age",
     xlab="Age", ylab="Frequency", col="skyblue")

# # Create a histogram of Distribution of each sales data by age
hist(customer_shopping_data$age, main="Distribution of each sales data by age",
     xlab="Age", ylab="Frequency", col="skyblue")

# # Create a histogram of Distribution of each sales data by gender

genderType <- c(Female = 0, Male = 1, Other = 2)
customer_shopping_data$gender_num <- genderType[customer_shopping_data$gender]

hist(customer_shopping_data$gender_num, main="Distribution of each sales data by gender",
     xlab="Gender (Female = 0, Male = 1)", ylab="Frequency", col="skyblue")


## Scatter Plot
print(result)
ggplot(result, aes(x = inv_date, y = n)) +
  xlab("Date") +
  ylab("Sales every month")+
  geom_point()





library(ggplot2)
library(scales) # to access breaks/formatting functions
    
ggplot(result, aes(x = inv_date, y = n/1000)) +
  xlab("Date") +
  ylab("Sales (in thousands) every month")+
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
# Plot
ggplot(result, aes(x=as.Date(inv_date), y=n)) +
      geom_point() +
      scale_x_date(date_breaks = "months" , date_labels = "%b-%y")


# Correlation plot

library(dplyr)
result1 <- df1 %>% count(inv_date = format(as.Date(date, "%Y-%m-%d"), "%m/%Y"))


plot(result1$inv_date, result1$n, pch = 19, col = "lightblue")

abline(lm(df1$date ~ df1$quantity), col = "red", lwd = 3)

text(paste("Correlation:", round(cor(df1$date, df1$quantity), 2)), x = 25, y = 95)
