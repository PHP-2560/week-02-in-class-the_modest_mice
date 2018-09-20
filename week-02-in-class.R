#------------------------------------------------------------------
#Part 1:
#------------------------------------------------------------------
#Install packages
install.packages('dplyr')
library('dplyr')
install.packages('ggplot2')
library('ggplot2')
install.packages('tidyr')
library(tidyr)

#Load data file
rain.df = as.data.frame(read.table('rnf6080.dat'))
nrows = nrow(rain.df)
ncols = ncol(rain.df)
col_names = names(rain.df)
value_row5_col7 = rain.df[5,7]
print_second_row = rain.df[2,]

#output
nrows
ncols
col_names
value_row5_col7
print_second_row

#Labels the first three columns "year" "month" and "day" respectively. 
#The last 24 columns are the hour of the day. 
names(rain.df) <- c("year","month","day",seq(0,23))

#Adds column of sum of 24 rightmost columns
rain.df = 
rain.df %>% 
  mutate(daily = rowSums(rain.df[, 4:27]))

#There is clearly a problem with this there are negative rainfall values.
hist(rain.df$daily, main = "Histogram of Daily Rainfall", xlab = "Daily Rainfall Amount", ylab = "Frequency")

#Changed all values <0 to NA
rain.df[rain.df < 0] = NA
hist(rain.df$daily) #much for reasonable because all the values are positive. 

#------------------------------------------------------------------
#Part II:
#------------------------------------------------------------------
install.packages('nycflights13')
library("nycflights13")

#inspect structure of flights dataset
str(flights)

#Part 1:
flights %>% 
  filter(month == 07 & day == 22)

#Part 2:
flights %>%
  filter(row_number() %in% 567:589)

#Part 3: 
flights %>%
  arrange(year, month, day)

#Part 4:
flights %>% 
  arrange(desc(arr_delay))

#Part 5: 
flights %>% 
  select(1:3)

#Part 6: 
flights %>%
  select(flight, tailnum, carrier)

#Part 7:
flights %>%
  rename('tail_num' = tailnum)

#Part 8:
flights %>%
  mutate(gain = arr_delay + dep_delay)

#Part 9: 
flights %>%
  mutate(speed = distance/(air_time/60))

#Part 10: 
flights %>%
  summarise(mean(arr_delay + dep_delay, na.rm = TRUE))

#Part 11: 
delay = 
  flights %>%
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance, na.rm = TRUE), 
                         delay = mean(arr_delay, na.rm = TRUE)) 

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

#Part 12: 
flights %>% 
  group_by(dest) %>%
  summarise(num_flights = n(), num_planes = n_distinct(tailnum))

#Part 13: 
#The problem with this function is that it serves several purposes. 
#It does multiple things, which hinders the clarity/logic behind it. 
filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

#Rewriting using piping:
#Yes it gives the same results
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(arr = mean(arr_delay, na.rm = TRUE),
            dep = mean(dep_delay, na.rm = TRUE)) %>%
  filter(arr > 30 | dep > 30)

#Part 14:
#This doesn't even run because there is no "date" variable
#It is extremely chaotic and I hate the arrow instead of equal sign. 
#They need to at least write the arguments of filter on the same line...
#It is very hard to follow the logic. 
hourly_delay <- filter(
  summarise(
    group_by(
      filter(
        flights, 
        !is.na(dep_delay)
      ),
      date, hour
    ),
    delay=mean(dep_delay),
    n=n()
  ),
  n>10
)

#Rewriting using piping: 
#This actually gives a result unlike the previous
hourly_delay = 
  flights %>%
    unite("date", 1:3, sep = "-" ) %>%
    filter(!is.na(dep_delay)) %>%
    group_by(date, hour) %>%
    summarise(delay = mean(dep_delay), n = n()) %>%
    filter(n > 10)%>%
    arrange(date, hour)
    
