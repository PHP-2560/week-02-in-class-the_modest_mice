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

