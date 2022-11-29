require(here)
require(data.table)
require(ggplot2)
require(dplyr)

points <- fread(here("data", "kqf_points.csv"))
points <- na.omit(points)

points_lm <- lm(points$final_points ~ points$pre_points)
aov(points_lm)
cor(points$pre_points, points$final_points)
hist(resid(points_lm))


##paired bar prelim vs final by year
##noted differnce between rot% measurements 

points %>%
  ggplot(aes(pre_points, final_points)) +
  geom_point() +
  geom_smooth()

