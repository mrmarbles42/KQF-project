require(here)
require(palmerpenguins)
require(ggplot2)
require(dplyr)
require(gridExtra)

catrate <- read.csv(here("data", "catrate.csv"))
penguin_dat <- subset(penguins, species != "Gentoo")
adelie_dat <- subset(penguin_dat, species == "Adelie")
chinstrap_dat <- subset(penguin_dat, species == "Chinstrap")

hist(catrate$cat.rate,
     xlab = "Catestrophe rate",
     main = "Catastrophe rate frequency")

shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate,
       mu = 0.28)

wilcox.test(catrate$cat.rate, 2/7)

boxplot(catrate$cat.rate)

levels(penguin_dat$species)

shapiro.test(adelie_dat$species)

shapiro.test(adelie_dat$flipper_length_mm)
shapiro.test(chinstrap_dat$flipper_length_mm)


adelie_plot <- adelie_dat %>%
  ggplot(aes(flipper_length_mm)) +
  geom_histogram() +
  ylim(0, 16) +
  xlim(170, 210) +
  labs(x = "Adelie flipper length")

chin_plot <- chinstrap_dat %>%
  ggplot(aes(flipper_length_mm)) +
  geom_histogram() +
  ylim(0, 16) +
  xlim(170, 210) +
  labs(x = "Chinstrap flipper length")

grid.arrange(adelie_plot, chin_plot, ncol = 2)


t.test(adelie_dat$flipper_length_mm, chinstrap_dat$flipper_length_mm)
