# library
library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# dataset:
master <- read_csv("fifa_19_final_dataset.csv")
df <- master


# basic histogram
p <- df %>%
  #filter( price<300 ) %>%
  ggplot( aes(x=Age)) +
  geom_histogram( binwidth=2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram: Age") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p


# Plot
df %>% 
  group_by(Age) %>% 
  summarise(meanOverall=mean(Overall)) %>% 
  ggplot(aes(x=Age, y=meanOverall)) +
  geom_line( color="#69b3a2", size=1, alpha=0.9, linetype=1) +
  theme_ipsum(axis_title_size = 12,
              axis_title_face = "plain",
              axis_title_just = "ct") +
  ggtitle("Overall rating vs Age") +
  xlab("Age") +
  ylab("Mean Overall rating")


# Age bins vs Value:
df %>%
  # Add a new column called 'bin': cut the initial 'carat' in bins
  mutate( bin=cut_width(Age, width=3, boundary=0)) %>%
  # plot
  ggplot( aes(x=bin, y=Value) ) +
  geom_boxplot(fill="#69b3a2") +
  theme_ipsum(axis_title_size = 12,
              axis_title_face = "plain",
              axis_title_just = "ct",) +
  scale_y_continuous(labels = scales::comma,trans='log10') +
  xlab("Age") +
  ylab("Value")


#Frequency by positions
dft <- df %>% 
  filter(!is.na(Position)) %>% 
  group_by(PositionGroup) %>% 
  tally(sort = T) 

ggplot(dft, aes(x=PositionGroup, y=n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Positions")+
  ylab("Count") 
  
library(tidyverse)
library(ggplot2)

# POS  vs Value:
df %>% 
  ggplot( aes(x=PositionGroup, y=Value) ) +
  geom_boxplot(fill="#69b3a2") +
  theme_ipsum(axis_title_size = 12,
              axis_title_face = "plain",
              axis_title_just = "ct",) +
  scale_y_continuous(labels = scales::comma,trans='log10') +
  xlab("Position") +
  ylab("Value")


# Scatter plot: 
#Rating vs Value
library(ggplot2)
library(ggExtra)

# classic plot :
p <- ggplot(df, aes(x=Overall, y=Value)) +
  geom_point() +
  theme(legend.position="none")+
  scale_y_continuous(labels = scales::comma,trans='log2') +
  #xlim(0,100)+
  xlab("Overall") +
  ylab("Value")

# Set relative size of marginal plots (main plot 10x bigger than marginals)
p1 <- ggMarginal(p, type="histogram", size=10)

# Custom marginal plots:
p2 <- ggMarginal(p, type="histogram", fill = "slateblue", xparams = list(  bins=10))

# Show only marginal plot for x axis
p3 <- ggMarginal(p, margins = 'x', color="purple", size=4)
p
p1    


#########
df %>%
  group_by(Age) %>%
  summarise(Potential = mean(Potential),
            Overall = mean(Overall)) %>%
  ggplot(aes(x= Age)) +
  geom_line(aes(y= Potential), color = "purple", size = 1) +
  geom_line(aes(y= Overall), color = "grey50", size = 1) +
  annotate("text", x= 30, y=71, label = "Potential meets overall\ntalent at 29 years old", color = "purple") +
  ggtitle("Potential And Overall Talent Converges")


#######################

