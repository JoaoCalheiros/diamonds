install.packages('tidyverse')
install.packages('ggthemes')


library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(gridExtra)

# Histogram of the price of all diamonds in the diamonds data set

ggplot(diamonds, aes(x = price)) +
  geom_histogram(color = 'black', fill = 'SteelBlue', binwidth = 500) +
  scale_x_continuous(labels = dollar, breaks = seq(0, 20000, 1000)) +
  labs(title = 'Distribution of diamonds price',
       x = 'Price', y = 'Count') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle=90))


# Describing shape and center of the price distribution
summary(diamonds$clarity)
# Small amounts of very expensive pieces drive the mean up. The median in this case is a better measurement 
# variable of the center of the distribution

# How many diamonds cost less than $500
diamonds %>% 
  filter(price < 500) %>% 
  summarise(count = n())
# How many diamonds cost less than $2000
diamonds %>% 
  filter(price < 2000) %>% 
  summarise(count = n())
# How many diamonds cost more than $2000
diamonds %>% 
  filter(price > 2000) %>% 
  summarise(count = n())

# Focus on the largest peak of the histogram just created

ggplot(diamonds, aes(x = price)) +
  geom_histogram(color = 'black', fill = 'SteelBlue', binwidth = 50) +
  scale_x_continuous(labels = dollar, breaks = seq(0, 2000, 100)) +
  labs(x = 'Price', y = 'Count') +
  coord_cartesian(c(0, 2000)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle=90))

# Create histograms for diamond prices by cut      

ggplot(diamonds, aes(x=price)) +
  geom_histogram(color='black', fill='SteelBlue', binwidth = 30) +
  scale_x_continuous(labels=dollar, breaks=seq(0, 5000, 200)) +
  labs(title='Price distribution by cut quality',
       x = 'Price', y = 'Count') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle=90)) +
  coord_cartesian(c(0, 5000)) +
  facet_grid(cut~.) 

# Look at distribution for diamonds by cut 

diamonds %>% 
  group_by(cut) %>% 
  summarise(max_price = max(price),
            min_price = min(price),
            median_price = median(price))

# Make plot remember
# y-axis in the histograms is not fixed
qplot(x = price, data=diamonds) +
  facet_wrap(~cut, scales = 'free') +
  labs(x = 'Price', y = 'Count')

# Printing summaries on the prices per quality cut.

cut_price <- data.frame(diamonds$cut, diamonds$price)
fair <- cut_price %>% 
  filter(cut_price$diamonds.cut == 'Fair')
summary(fair$diamonds.price)

good <- cut_price %>% 
  filter(cut_price$diamonds.cut == 'Good')
summary(good$diamonds.price)

v_good <- cut_price %>% 
  filter(cut_price$diamonds.cut == 'Very Good')
summary(v_good$diamonds.price)

premium <- cut_price %>% 
  filter(cut_price$diamonds.cut == 'Premium')
summary(premium$diamonds.price)  

ideal <- cut_price %>% 
  filter(cut_price$diamonds.cut == 'Ideal')
summary(ideal$diamonds.price)  

# Create histogram of price per carat and facet it by cut
# Understanding some functions I might use 
?paste
?expression
?scale_x_log10
?trans_breaks
?trans_format
?math_format

ggplot(diamonds, aes(x=price/carat)) +
  geom_histogram(color='black', fill='SteelBlue', binwidth=0.05) +
  labs(x = 'Price/Carat', y = 'Count') +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +   # (10^.x) the dot is necessary so in the x label it appears 10^3 not 10^x
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text()) +
  facet_grid(cut~., scale = "free")

ggsave("diamonds.pdf", width = 12, height = 9)

#--------------------------
# Investigate the price of diamonds using box plots, numerical summaries and the following categorical variables:
# cut, color and clarity

diamonds %>% 
  group_by(cut) %>% 
  summarise(count = n(),
            avg_price = mean(price))

# Takeaways
# The number of Ideal diamonds(21551) is much larger than the others but its average price($3458) its also the lowest
# The number of Fair diamonds(1610) is the lowest and its average price($4359) its the second highest
# The average price for Premium diamonds($4584) is the highest for all cuts
#--------------------------
# Check which letters represent the best colors for diamonds
levels(diamonds$color)

diamonds %>% 
  group_by(color) %>% 
  summarise(count = n(),
            avg_price = mean(price))

# Takeaways
# Color 'G' is the most represented in the data set - 11292 occurrences 
# Color 'J' is the least represented in the data set - 2808 occurrences - while still having the highest average price: $5324
# The lowest average price is for the color 'E':  $3077
#--------------------------
diamonds %>% 
  group_by(clarity) %>% 
  summarise(couunt = n(),
            avg_price = mean(price))

# Takeaways
# Clarity of SI1 and VS2 are the most represented with 13065 and 12258 occurrences respectively
# The highest average price belongs to clarity SI2 - $5063 - which is greater than all the others by at least $1000
# I1, SI1, VS2 and VS1 average prices range between $3800-$3900
# The lowest average price belongs to VVS1 with a value of $2523
#--------------------------

# Box plots 

# price vs clarity for all colors [D,E,F,G,H,I,J]
ggplot(diamonds, aes(x=clarity, y=price, fill=cut)) +
  geom_boxplot() +
  labs(x = 'Clarity', y = 'Price') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text())



# Matrix with price vs clarity per color
ggplot(diamonds, aes(x=clarity, y=price, fill=cut)) +
  geom_boxplot() +
  labs(x = 'Clarity', y = 'Price',
       title='Distribution of price',
       subtitle='across cut, clarity and color') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text()) +
  facet_grid(color~.)

ggsave("diamonds.pdf", width = 12, height = 9)

# IQR - Interquartile range
# IQR for diamonds with the best color
diamonds %>% 
  group_by(color) %>% 
  filter(color == 'D') %>% 
  summarise(quartile_25 = quantile(price, 0.25),
            quartile_75 = quantile(price, 0.75),
            IQR = quartile_75 - quartile_25)
# IQR for diamonds with the worst color
diamonds %>% 
  group_by(color) %>% 
  filter(color == 'J') %>% 
  summarise(quartile_25 = quantile(price, 0.25),
            quartile_75 = quantile(price, 0.75),
            IQR = quartile_75 - quartile_25)


# Best Color D - IQR: 3302 quartile_25: 911 quartile_75: 4214
# Worst Color J -  IQR: 5834 quartile_25: 1860 quartile_75: 7695 
# These make sense when printing the next command
diamonds %>% 
  group_by(color) %>%
  summarise(count = n(),
            avg_price = mean(price))
#------------------------------------------------------------------
# Investigate the price per carat of diamonds across the different colors of diamonds using box plots.

ggplot(diamonds, aes(x=color, y=price/carat, fill=color)) +
  geom_boxplot() +
  labs(title='Distribution of price',
       subsitle='across colors',
       x='Color',
       y='Price per Carat') +
  coord_cartesian(ylim=c(1000, 6000)) +
  scale_y_continuous(labels=dollar)


#Investigate the weight of the diamonds (carat) using a frequency polygon.
#Use different bin widths to see how the frequency polygon changes.
#What carat size has a count greater than 2000? Check all that apply.

summary(diamonds$carat)
ggplot(diamonds, aes(x=carat)) +
  geom_freqpoly(binwidth=0.1) +
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Frequency of diamonds weight',
       x='Carat',
       y='Count') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text()) 

# Go in more depth
ggplot(diamonds, aes(x=carat)) +
  geom_freqpoly(binwidth=0.05) +
  scale_x_continuous(breaks=c(0.1, 0.5, 0.8, 1.2, 1.5, 2.0, 3.0, 5.0), expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_vline(xintercept=c(0.2, 0.4, 0.7, 0.79, 1.04, 5.01), color='#7daa8e', linetype='longdash') +
  labs(title='Frequency of diamonds weight',
       subtitle='Green vlines - summary(diamonds$carat)',
       x='Carat',
       y='Count') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text()) +
  theme(plot.subtitle = element_text(size=10)) 

ggsave("freq_plot.pdf", width = 12, height = 9)

# Create scatterplots of price vs x,y and z

ggplot(diamonds, aes(x=x, y=price)) +
  geom_point(alpha=.05) +
  coord_cartesian(xlim=c(3.5, 11))+
  labs(title='Price vs X') +
  scale_y_continuous(breaks=seq(0, 20000, 2000), label=dollar) 

# Price vs X - The data starts at x~3.7 and there seems to be an artificial barrier at $19 000 
# Correlation between price and x:
with(diamonds, cor.test(price, x))
# p-value < 2.2e-16 - a typical threshold is 0.05, anything smaller counts as statistically significant
# cor: 0.8844352

# Correlation between price and y:
with(diamonds, cor.test(price, y))
# cor: 0.8654209  

# Correlation between price and z:
with(diamonds, cor.test(price, z))
# cor: 0.8612494 

# Scatter plot price vs depth
ggplot(diamonds, aes(x=depth, y=price)) +
  geom_point(alpha=.05) +
  coord_cartesian(xlim=c(40, 80))+
  labs(title='Price vs Depth') +
  scale_y_continuous(breaks=seq(0, 20000, 2000), label=dollar) 

# Add transparency and x labels every 2 
ggplot(diamonds, aes(x=depth, y=price)) +
  labs(title='Price vs Depth') +
  geom_point(alpha=1/100) +
  scale_x_continuous(breaks=seq(min(diamonds$depth), max(diamonds$depth), 2), labels=seq(min(diamonds$depth), max(diamonds$depth), 2))
with(diamonds,  cor.test(price, depth))
# Depth does not influence the price of diamonds. The cor is not equal to 0 (p-value = 0.0134) and (cor: -0.0106474)

# Price vs Carat with top 1% of both omitted 
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(alpha=.05) +
  labs(title='Price vs Carat',
       subtitle='Omitted top 1%') +
  scale_x_continuous(limits=c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(breaks=seq(0, 18000, 2000),
                     limits=c(0, quantile(diamonds$price, 0.99)),
                     labels=dollar) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text()) +
  theme(plot.subtitle = element_text(size=10))

# Create scatter plot for price vs volume(x * y * z)  
# rough approximation of a diamonds volume
diamonds_v <- diamonds %>% 
  mutate(volume=x*y*z)


ggplot(diamonds_v, aes(x=volume, y=price)) +
  geom_point() +
  labs(title='Price vs Volume') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

# Volumes ranging from 0-500 hold the huge majority of diamonds, whatever the price
# Volumes ranging from 1000-3000 seem to be nonexistent
# 3 outliers  1 of which expands the plot by a very significant size
# Diamonds with volumes near 0 exist

# Correlation of price and volume excluding diamonds with volume of 0 or greater then 800
with(subset(diamonds_v, !(volume == 0 | volume >= 800)), cor.test(price, volume))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0.
diamonds_v2 <- diamonds_v %>% 
  filter(volume != 0, 
         volume <= 800)

ggplot(diamonds_v2, aes(x=volume, y=price)) +
  geom_point(alpha=.05) +
  geom_smooth(method='lm')
# This is not the best model since the relationship does not look normal



diamondsByClarity <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            count = n()) %>% 
  arrange(clarity)


# Create summary dataframes with the mean price by clarity and color
# Next create two bar plots in one output image using package gridExtra

diamonds_mp_cla <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(mean_price = mean(price))

diamonds_mp_col <- diamonds %>% 
  group_by(color) %>% 
  summarise(mean_price = mean(price))

plt1 <- ggplot(diamonds_mp_cla, aes(x=clarity, y=mean_price, fill=clarity)) +
  geom_bar(stat = "identity", color='black') +
  guides(fill=guide_legend(ncol=2))

plt2 <- ggplot(diamonds_mp_col, aes(x=color, y=mean_price, fill=color)) +
  geom_bar(stat = "identity", color='black') +
  guides(fill=guide_legend(ncol=2))

grid.arrange(plt1, plt2)
# Takeaways:
# Upward trend in average price as color goes from D to J
# Downward trend in average price as clarity goes from I1 to IF


# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

ggplot(diamonds, aes(x=table, y=price, color=cut)) +
  geom_point(size=2.5) +
  labs(title='Price vs Table',
       x='table',
       y='price') +
  scale_x_continuous(breaks=seq(50, 80, 2),
                     limits=c(50, 80)) +
  scale_color_brewer(palette = 'RdGy') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text())

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds

# Note: Volume is a very rough approximation of
# a diamond's actual volume.


diamonds <- diamonds %>% 
  mutate(volume = x * y * z) 

ggplot(subset(diamonds, volume <= quantile(volume, 0.99) & volume > 0), aes(x=volume, y=price, color=clarity)) +
  geom_point(size=2.5) +
  scale_y_log10(labels=dollar, 
                breaks=c(0, 1000, 10000)) +
  scale_color_brewer(palette = 'BrBG') +
  theme_minimal()

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.


ggplot(diamonds, aes(x=cut, y=price/carat, color=color)) +
  geom_jitter() +
  facet_wrap(~clarity) +
  scale_color_brewer(palette = 'BrBG') +
  scale_x_discrete(labels=c('F', 'G', 'VG', 'P', 'I'))

