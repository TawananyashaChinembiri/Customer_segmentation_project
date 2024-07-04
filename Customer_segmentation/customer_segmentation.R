csd = read.csv('customer_segmentation_data.csv')

install.packages('tidyverse')

library(tidyverse)

# The data has no missing values therefore it is clean


# Histogram showing the distribution of age of the customers
age_hist_plot <- ggplot(data<- csd) +
  geom_histogram(mapping = aes(x=age), fill = 'blue', binwidth = 10, color = 'black') +
  labs(
    title = 'Age distribution',
    subtitle = 'Histogram showing of the distribution of age of the customers',
    x= 'Number of customers',
    y = 'Age of the customers'
  )

ggsave('Age Histogram image.png', plot = age_hist_plot, width = 6, height = 4)


# bar chart showing the distribution of gender of the customers
gender_count <- csd %>%
  dplyr::count(gender)

gender_bar <- ggplot(data = gender_count, aes(x = gender, y = n)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'black' ) +
  labs(
    title = 'Gender distribution',
    subtitle = 'Bar chart showing the distribution of gender of the customers',
    x = 'Gender',
    y = 'Number of customers'
  )
ggsave('Gender bar chart.png', plot = gender_bar, width = 6, height = 4)


# Histogram showing the distribution of the salary of the customers
salary_dist <- ggplot(data = csd, aes(x = income)) +
  geom_histogram(fill='green', binwidth = 10000, color= 'black' ) +
  labs(
    title = 'Salary Histogram',
    subtitle = "Histogram showing the distribution of the Customers' salaries",
    y = 'Number of Customers',
    x = 'Customers Salaries'
  )

ggsave('Salary Histogram.png', plot = salary_dist, width = 6, height= 4)

# Histogram showing the distribution of membership years
membership_dist <- ggplot(data = csd, aes(x=membership_years)) +
  geom_histogram(fill = 'blue', color= 'black', binwidth = 2) +
  labs(
    title = 'Membership Years distribution',
    subtitle = 'Histogram showing the distribution of membership years',
    x = 'Membership years',
    y = 'Number of customers'
  )
ggsave('Membership Years distribution.png', plot = membership_dist, width = 6, height = 4)

# distribution showing purchase frequency
purchase_freq <- ggplot(data= csd, aes(x = purchase_frequency)) +
  geom_histogram(fill= 'blue', color = 'black', binwidth = 10)  +
  labs (
    title = 'Purchase Frequency of customers',
    subtitle = 'Histogram showing the purchase frequency of the customers',
    y= 'Number of customers'
  )
ggsave('Purchase Frequency histogram.png', plot = purchase_freq, width = 6, height = 4)


# bar chart distribution of the preferred category

category_count <- csd %>% 
  dplyr::count(preferred_category)

cat_count_dist = ggplot(data = category_count, aes(x= preferred_category, y = n)) +
  geom_bar(stat = 'identity',fill= 'blue', color = 'black') +
  labs(
    title = 'Preferred category distribution',
    subtitle = 'Bar chart showing the distribution of the preferred category',
    x = 'Preferred Category',
    y = 'Number of Customers'
  )
ggsave('Preferred Category.png', plot = cat_count_dist, width = 6, height = 4)

# histogram showing the last purchase amount
last_purchase = ggplot(data = csd, aes(x=last_purchase_amount)) +
  geom_histogram(fill= 'blue', color = 'black', binwidth = 100) +
  labs (
    title = 'Last Purchase Distribution',
    subtitle = 'Histogram showing distribution of last purchase amount',
    x = 'Last Purchase amount',
    y = 'Number of customers'
  )

ggsave('last purchase distribution.png', plot = last_purchase, width = 6, height = 4)


# Histogram showing the spending score of the customers


