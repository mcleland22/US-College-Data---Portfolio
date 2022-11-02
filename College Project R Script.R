# The graphs generated in this R script can be found in the US-College-Data---Portfolio repository.


install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

# Importing datasets from Excel
College_Data_Overview <- read_excel("C:/Users/Megan/Downloads/College Data Overview.xlsx")
View(College_Data_Overview)

College_Data_Public <- read_excel("C:/Users/Megan/Downloads/College Data Public.xlsx")
View(College_Data_Public)

College_Data_Private <- read_excel("C:/Users/Megan/Downloads/College Data Private.xlsx")
View(College_Data_Private)

# Used t.test to compare statistical average difference

t.test(data = College_Data_Overview, Grad_Rate ~ Private)
# t.test used to evaluate mean grad rate of public and private colleges. 

t.test(data = College_Data_Overview, Expend ~ Private)
# t.test used to compare expenditures between public and private colleges.

t.test(data = College_Data_Overview, S_F_Ratio ~ Private)
# t.test used to assess the student to faculty ratio between public and private colleges.




# Creating pie graphs to visualize the difference in grad rate between college types.
Private <- College_Data_Private
Public <- College_Data_Public

avg_private_grad <- mean(Private$Grad_Rate)
avg_public_grad <- mean(Public$Grad_Rate)

# To compare the ratio of graduated vs. non-graduated private students, I averaged the graduation rates among private colleges. 

x <- c(avg_private_grad, (100-avg_private_grad))
labels <- c("Graduated", "Did Not Graduate")
percent <- round(x/sum(x)*100)
labels <- paste(labels, percent) # add percents to labels
labels <- paste(labels,"%",sep="") # add % to labels
pie(x,labels = labels,
    main="Graduation Rate\n (Private College)")


# To compare the ratio of graduated vs. non-graduated public students, I averaged the graduation rates among public colleges. 

x <- c(avg_public_grad, (100-avg_public_grad))
labels <- c("Graduated", "Did Not Graduate")
percent <- round(x/sum(x)*100)
labels <- paste(labels, percent) # add percents to labels
labels <- paste(labels,"%",sep="") # add % to labels
pie(x,labels = labels,
    main="Graduation Rate\n (Public College)")




# Creating a bar graph to compare the expenditures between college types.
# Imported two refined datasets from BigQuery that were filtered for private and public. 

Private <- College_Data_Private
Public <- College_Data_Public

avg_private_expend <- mean(Private$Expend)
avg_public_expend <- mean(Public$Expend)

avg_df <- data.frame(school=c("Private", "Public"), 
                     value=c(avg_private_expend, avg_public_expend))

ggplot(data = avg_df, aes(x=school, y=value))+
  geom_col(fill = 'light blue', color = 'black', width = 0.68) +
  labs(title = "College Expenditures") +
  labs(subtitle = "Private vs. Public") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))




# Creating a graph to compare the student faculty ratio between college types. 

Private <- College_Data_Private
Public <- College_Data_Public

avg_private_sf_ratio <- mean(Private$S_F_Ratio)
avg_public_sf_ratio <- mean(Public$S_F_Ratio)

avg_df <- data.frame(school=c("Private", "Public"), 
                     value=c(avg_private_sf_ratio, avg_public_sf_ratio))

ggplot(data = avg_df, aes(x=school, y=value))+
  geom_col(fill = 'light blue', color = 'black', width = 0.68) +
  labs(title = "Student Faculty Ratio") +
  labs(subtitle = "Private vs. Public") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
