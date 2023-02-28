install.packages("palmerpenguins") # You only need to do this once
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("plotly")
library(plotly)
library(tidymodels)
library(ggpubr)
library(palmerpenguins)
library(tidyverse)
data("penguins")
penguins = na.omit(penguins) # Removes missing rows
my.student.number = 220500694 # Replace this with your student number
set.seed(my.student.number)
my.penguins = penguins[sample(nrow(penguins), 100), ]
print(my.penguins)
#Summarizing the Data
head(my.penguins)
penguins %>% 
count(species)
view(my.penguins)
table(my.penguins$year)

my.penguins %>%
  group_by(species, sex)%>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

my.penguins %>% 
  group_by(species) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))
table(my.penguins$species)

mass_flipper <- ggplot(data = my.penguins, 
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
mass_flipper
  



flipper_bill <- ggplot(data = my.penguins,
                       aes(x = flipper_length_mm,
                           y = bill_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Flipper and bill length",
       subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Bill length (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
flipper_bill



#Histogram - Flipper length against Frequency.
flipper_hist <- ggplot(data = my.penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), 
                 alpha = 0.5, 
                 position = "identity", binwidth = 1.5) +  geom_density(aes(y = ..count..), alpha = .2, fill = "#FF6666") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Flipper lengths against frequency",
       plot.title = element_text(size = 30, hjust = 0.5),
       axis.text = element_text(size = 20),
       axis.title = element_text(size = 25))+
  theme(legend.position = c(0.15, 0.85),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
flipper_hist


#Histogram - Bill length against Frequency.
bill_hist <- ggplot(data = my.penguins, aes(x = bill_length_mm)) +
  geom_histogram(aes(fill = species), 
                 alpha = 0.5, 
                 position = "identity", binwidth = 1.5) +  geom_density(aes(y = ..count..), alpha = .2, fill = "#FF6666") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Bill length (mm)",
       y = "Frequency",
       title = "Bill lengths against frequency",
       plot.title = element_text(size = 30, hjust = 0.5),
       axis.text = element_text(size = 20),
       axis.title = element_text(size = 25))+
  theme(legend.position = c(0.15, 0.85),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
bill_hist




ggarrange(flipper_hist, bill_hist, 
          ncol = 2, nrow = 1)





by_species = group_split(my.penguins, species, .keep = TRUE)
by_species
adelie = by_species[[1]]
head(by_species[[1]])
chinstrap = by_species[[2]]
head(by_species[[2]])
gentoo = by_species[[3]]
head(by_species[[3]])
head(my.penguins)

mean(gentoo$flipper_length_mm)
sd(gentoo$flipper_length_mm)
mean(gentoo$bill_length_mm)
sd(gentoo$bill_length_mm)
mean(gentoo$body_mass_g)
sd(gentoo$body_mass_g)

mean(chinstrap$flipper_length_mm)
sd(chinstrap$flipper_length_mm)
mean(chinstrap$bill_length_mm)
sd(chinstrap$bill_length_mm)
mean(chinstrap$body_mass_g)
sd(chinstrap$body_mass_g)

mean(adelie$flipper_length_mm)
sd(adelie$flipper_length_mm)
mean(adelie$bill_length_mm)
sd(adelie$bill_length_mm)
mean(adelie$body_mass_g)
sd(adelie$body_mass_g)

#Histogram - Bill length against Frequency. ADELIE
ad_bill_hist <- ggplot(data = adelie, aes(x = bill_length_mm)) +
  geom_histogram(aes(y = ..density..), bins = 10,color="darkblue", fill="lightblue") + stat_function(fun = dnorm,size = 1, colour = "orange", args = list(mean = mean(adelie$bill_length_mm), sd = sd(adelie$bill_length_mm))) +
  geom_density(color="#4c49b6", lwd=1) +
  labs(x = "Bill length (mm)",
       title = "Adelie Penguins bill lengths")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
ad_bill_hist
pnorm(40, mean(adelie$bill_length_mm), sd(adelie$bill_length_mm)) - pnorm(36,mean(adelie$bill_length_mm) ,sd(adelie$bill_length_mm))
pnorm(50, mean(adelie$bill_length_mm), sd(adelie$bill_length_mm)) - pnorm(45,mean(adelie$bill_length_mm) ,sd(adelie$bill_length_mm))



#Histogram - Bill length against Frequency. CHINSTRAP
chin_bill_hist <- ggplot(data = chinstrap, aes(x = bill_length_mm)) +
  geom_histogram(aes(y = ..density..), bins = 10, color="darkblue", fill="lightblue") +   stat_function(fun = dnorm, size = 1, colour = "orange",
                                                         args = list(mean = mean(chinstrap$bill_length_mm),
                                                                     sd = sd(chinstrap$bill_length_mm))) + geom_density(color="#4c49b6", lwd=1) +
  labs(x = "Bill length (mm)",
       y = element_blank(),
       title = "Chinstrap Penguin Bill lengths")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
chin_bill_hist
pnorm(40, mean(chinstrap$bill_length_mm), sd(chinstrap$bill_length_mm)) - pnorm(36,mean(chinstrap$bill_length_mm) ,sd(chinstrap$bill_length_mm))

?stat_function()


#Histogram - Bill length against Frequency. GENTOO
gent_bill_hist <- ggplot(data = gentoo, aes(x = bill_length_mm)) +
  geom_histogram(aes(y = ..density..),bins = 10, color="darkblue", fill="lightblue") +   stat_function(fun = dnorm,size = 1, colour = "orange",
                                                                                             args = list(mean = mean(gentoo$bill_length_mm),
                                                                                                         sd = sd(gentoo$bill_length_mm))) +
  geom_density(color="#4c49b6", lwd=1) +
  labs(x = "Bill length (mm)",
       y = element_blank(),
       title = "Gentoo Penguin Bill lengths")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
gent_bill_hist
pnorm(50, mean(gentoo$bill_length_mm), sd(gentoo$bill_length_mm)) - pnorm(45,mean(gentoo$bill_length_mm) ,sd(gentoo$bill_length_mm))



ggarrange(ad_bill_hist, chin_bill_hist, gent_bill_hist  + rremove("x.text"), 
          ncol = 3, nrow = 1)





#Histogram - Flipper length against Frequency. ADELIE
ad_flipper_hist <- ggplot(data = adelie, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..density..), bins = 10,color="darkblue", fill="lightblue") + stat_function(fun = dnorm, size = 1, colour = "orange",args = list(mean = mean(adelie$flipper_length_mm), sd = sd(adelie$flipper_length_mm))) +
  geom_density(color="#4c49b6", lwd=1) +
  labs(x = "Flipper length (mm)",
       title = "Adelie Penguins flipper lengths") +
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
ad_flipper_hist
pnorm(40, mean(adelie$flipper_length_mm), sd(adelie$flipper_length_mm)) - pnorm(36,mean(adelie$flipper_length_mm) ,sd(adelie$flipper_length_mm))
pnorm(50, mean(adelie$flipper_length_mm), sd(adelie$flipper_length_mm)) - pnorm(45,mean(adelie$flipper_length_mm) ,sd(adelie$flipper_length_mm))



#Histogram - Flipper length against Frequency. CHINSTRAP
chin_flipper_hist <- ggplot(data = chinstrap, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..density..), bins = 10, color="darkblue", fill="lightblue") + stat_function(fun = dnorm,size = 1, colour = "orange", args = list(mean = mean(chinstrap$flipper_length_mm), sd = sd(chinstrap$flipper_length_mm))) +
  geom_density(color="#4c49b6", lwd=1)+
  labs(x = "Flipper length (mm)",
       y = element_blank(),
       title = "Chinstrap Penguin flipper lengths")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
chin_flipper_hist
pnorm(40, mean(chinstrap$flipper_length_mm), sd(chinstrap$flipper_length_mm)) - pnorm(36,mean(chinstrap$flipper_length_mm) ,sd(chinstrap$flipper_length_mm))




#Histogram - Flipper length against Frequency. GENTOO
gent_flipper_hist <- ggplot(data = gentoo, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..density..),bins = 10, color="darkblue", fill="lightblue") +   stat_function(fun = dnorm,size = 1, colour = "orange",
                                                                                                 args = list(mean = mean(gentoo$flipper_length_mm),
                                                                                                                   sd = sd(gentoo$flipper_length_mm))) +
  geom_density(color="#4c49b6", lwd=1)+
  labs(x = "Flipper length (mm)",
       y = element_blank(),
       title = "Gentoo Penguin flipper lengths")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
gent_flipper_hist
pnorm(50, mean(gentoo$flipper_length_mm), sd(gentoo$flipper_length_mm)) - pnorm(45,mean(gentoo$flipper_length_mm) ,sd(gentoo$flipper_length_mm))


ggarrange(ad_flipper_hist, chin_flipper_hist, gent_flipper_hist  + rremove("x.text"), 
          ncol = 3, nrow = 1)






#Histogram - Body mass against Frequency. ADELIE
ad_bodym_hist <- ggplot(data = adelie, aes(x = body_mass_g)) +
  geom_histogram(aes(y = ..density..), bins = 10,color="darkblue", fill="lightblue") + stat_function(fun = dnorm, size = 1, colour = "orange",args = list(mean = mean(adelie$body_mass_g), sd = sd(adelie$body_mass_g))) +
  geom_density(color="#4c49b6", lwd=1) +
  labs(x = "body mass (g)",
       title = "Adelie Penguins body mass") +
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
ad_bodym_hist
pnorm(40, mean(adelie$flipper_length_mm), sd(adelie$flipper_length_mm)) - pnorm(36,mean(adelie$flipper_length_mm) ,sd(adelie$flipper_length_mm))
pnorm(50, mean(adelie$flipper_length_mm), sd(adelie$flipper_length_mm)) - pnorm(45,mean(adelie$flipper_length_mm) ,sd(adelie$flipper_length_mm))



#Histogram - Body mass against Frequency. CHINSTRAP
chin_bodym_hist <- ggplot(data = chinstrap, aes(x = body_mass_g)) +
  geom_histogram(aes(y = ..density..), bins = 10, color="darkblue", fill="lightblue") + stat_function(fun = dnorm,size = 1, colour = "orange", args = list(mean = mean(chinstrap$body_mass_g), sd = sd(chinstrap$body_mass_g))) +
  geom_density(color="#4c49b6", lwd=1)+
  labs(x = "body mass (g)",
       y = element_blank(),
       title = "Chinstrap Penguin body mass")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
chin_bodym_hist
pnorm(40, mean(chinstrap$flipper_length_mm), sd(chinstrap$flipper_length_mm)) - pnorm(36,mean(chinstrap$flipper_length_mm) ,sd(chinstrap$flipper_length_mm))




#Histogram - Body mass against Frequency. GENTOO
gent_bodym_hist <- ggplot(data = gentoo, aes(x = body_mass_g)) +
  geom_histogram(aes(y = ..density..),bins = 10, color="darkblue", fill="lightblue") +   stat_function(fun = dnorm,size = 1, colour = "orange",
                                                                                                       args = list(mean = mean(gentoo$body_mass_g),
                                                                                                                   sd = sd(gentoo$body_mass_g))) +
  geom_density(color="#4c49b6", lwd=1)+
  labs(x = "body mass (g)",
       y = element_blank(),
       title = "Gentoo Penguin body mass")+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
gent_bodym_hist
pnorm(50, mean(gentoo$flipper_length_mm), sd(gentoo$flipper_length_mm)) - pnorm(45,mean(gentoo$flipper_length_mm) ,sd(gentoo$flipper_length_mm))


ggarrange(ad_bodym_hist, chin_bodym_hist, gent_bodym_hist, 
          ncol = 3, nrow = 1)


ggarrange(ad_bill_hist, chin_bill_hist, gent_bill_hist,ad_flipper_hist, chin_flipper_hist, gent_flipper_hist, ad_bodym_hist, chin_bodym_hist, gent_bodym_hist, 
          ncol = 3, nrow = 3)






bill_a<-ggqqplot(adelie$bill_length_mm, ylab = "Bill length (mm)",ggtheme=theme_classic(), conf.int = TRUE,
                conf.int.level = 0.95) +  labs(title = "QQ-Plot, Adelie bill length (mm)")
bill_c<-ggqqplot(chinstrap$bill_length_mm, ylab = "Bill length (mm)",ggtheme=theme_classic(), conf.int = TRUE,
               conf.int.level = 0.95) +  labs(title = "QQ-Plot, Chinstrap bill length (mm)")
bill_g<-ggqqplot(gentoo$bill_length_mm, ylab = "Bill length (mm)",ggtheme=theme_classic(), conf.int = TRUE,
               conf.int.level = 0.95) +  labs(title = "QQ-Plot, Gentoo bill length (mm)")


flipp_a<-ggqqplot(adelie$flipper_length_mm, ylab = "Flipper length (mm)",ggtheme=theme_classic(), conf.int = TRUE,
         conf.int.level = 0.95) +  labs(title = "QQ-Plot, Adelie flipper length (mm)")
flipp_c<-ggqqplot(chinstrap$flipper_length_mm, ylab = "Flipper length (mm)",ggtheme=theme_classic(), conf.int = TRUE,
                conf.int.level = 0.95) +  labs(title = "QQ-Plot, Chinstrap flipper length (mm)")
flipp_g<-ggqqplot(gentoo$flipper_length_mm, ylab = "Flipper length (mm)",ggtheme=theme_classic(), conf.int = TRUE,
                conf.int.level = 0.95) +  labs(title = "QQ-Plot, Gentoo flipper length (mm)")
                


mass_a<-ggqqplot(adelie$body_mass_g, ylab = "Body mass (g)",ggtheme=theme_classic(), conf.int = TRUE,
         conf.int.level = 0.95) +  labs(title = "QQ-Plot, Adelie body mass (g)")
mass_c<-ggqqplot(chinstrap$body_mass_g, ylab = "Body mass (g)",ggtheme=theme_classic(), conf.int = TRUE,
               conf.int.level = 0.95) +  labs(title = "QQ-Plot, Chinstrap body mass (g)")
mass_g<-ggqqplot(gentoo$body_mass_g, ylab = "Body mass (g)",ggtheme=theme_classic(), conf.int = TRUE,
               conf.int.level = 0.95) +  labs(title = "QQ-Plot, Gentoo body mass (g)")


ggarrange(bill_a + rremove("x.text"), bill_c + rremove("x.text"), bill_g + rremove("x.text"),flipp_a + rremove("x.text"), flipp_c + rremove("x.text"), flipp_g + rremove("x.text"),
          mass_a + rremove("x.text"), mass_c + rremove("x.text"), mass_g + rremove("x.text"),
          ncol = 3, nrow = 3)

ggarrange(bill_a + rremove("x.text"), bill_c + rremove("x.text"), bill_g + rremove("x.text"),
          ncol = 3, nrow = 1)
ggarrange(flipp_a + rremove("x.text"), flipp_c + rremove("x.text"), flipp_g + rremove("x.text"),
          ncol = 3, nrow = 1)
ggarrange(mass_a + rremove("x.text"), mass_c + rremove("x.text"), mass_g + rremove("x.text"),
          ncol = 3, nrow = 1)



par(mfrow=c(1,3))
qqnorm(adelie$bill_length_mm, pch = 1, frame = TRUE,size=5)
qqline(adelie$bill_length_mm, col = "steelblue", lwd = 2)
pnorm(max(adelie$bill_length_mm), mean(adelie$bill_length_mm), sd(adelie$bill_length_mm)) - pnorm(min(adelie$bill_length_mm),mean(adelie$bill_length_mm) ,sd(adelie$bill_length_mm))

qq_chinstrap = qqnorm(chinstrap$bill_length_mm, pch = 1, frame = TRUE)
qq_chinstrap = qqline(chinstrap$bill_length_mm, col = "steelblue", lwd = 2)
pnorm(max(chinstrap$bill_length_mm), mean(chinstrap$bill_length_mm), sd(chinstrap$bill_length_mm)) - pnorm(min(chinstrap$bill_length_mm),mean(chinstrap$bill_length_mm) ,sd(chinstrap$bill_length_mm))

qqnorm(gentoo$bill_length_mm, pch = 1, frame = TRUE)
qqline(gentoo$bill_length_mm, col = "steelblue", lwd = 2)
pnorm(max(gentoo$bill_length_mm), mean(gentoo$bill_length_mm), sd(gentoo$bill_length_mm)) - pnorm(min(gentoo$bill_length_mm),mean(gentoo$bill_length_mm) ,sd(gentoo$bill_length_mm))





par(mfrow=c(1,3))
qqnorm(adelie$flipper_length_mm, pch = 1, frame = TRUE)
qqline(adelie$flipper_length_mm, col = "steelblue", lwd = 2)
pnorm(max(adelie$flipper_length_mm), mean(adelie$flipper_length_mm), sd(adelie$flipper_length_mm)) - pnorm(min(adelie$flipper_length_mm),mean(adelie$flipper_length_mm) ,sd(adelie$flipper_length_mm))

qq_chinstrap = qqnorm(chinstrap$flipper_length_mm, pch = 1, frame = TRUE)
qq_chinstrap = qqline(chinstrap$flipper_length_mm, col = "steelblue", lwd = 2)
pnorm(max(chinstrap$flipper_length_mm), mean(chinstrap$flipper_length_mm), sd(chinstrap$flipper_length_mm)) - pnorm(min(chinstrap$flipper_length_mm),mean(chinstrap$flipper_length_mm) ,sd(chinstrap$flipper_length_mm))

qqnorm(gentoo$flipper_length_mm, pch = 1, frame = TRUE)
qqline(gentoo$flipper_length_mm, col = "steelblue", lwd = 2)
pnorm(max(gentoo$flipper_length_mm), mean(gentoo$flipper_length_mm), sd(gentoo$bflipper_length_mm)) - pnorm(min(gentoo$flipper_length_mm),mean(gentoo$flipper_length_mm) ,sd(gentoo$flipper_length_mm))





table(between(gentoo$bill_length_mm,40,50))

stat_qq_band()
qqnorm(gentoo$bill_length_mm, pch = 1)
qqline(gentoo$bill_length_mm, col = "steelblue", lwd = 3)




fl_bm_pt<-ggplot(my.penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex), size=5) +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  labs(title = "Penguin flipper against Body mass",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin sex") +
  theme(legend.position = c(0.15, 0.75),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 35, hjust = 0.5),
        plot.subtitle = element_text(size = 30),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 30)) +
  facet_wrap(~species)




fl_bl_pt<-ggplot(my.penguins, aes(x = flipper_length_mm,
                        y = bill_length_mm)) +
  geom_point(aes(color = sex), size=5) +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  labs(title = "Penguin flipper against Bill length",
       x = "Flipper length (mm)",
       y = "bill length (mm)",
       color = "Penguin sex") +
  theme(legend.position = c(0.15, 0.75),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 35, hjust = 0.5),
        plot.subtitle = element_text(size = 30),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 30)) +
  facet_wrap(~species)
fl_bl_pt




bd_bl_pt<-ggplot(my.penguins, aes(x = bill_depth_mm,
                                  y = bill_length_mm)) +
  geom_point(aes(color = sex), size=5) +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  labs(title = "Penguin bill depth against Bill length",
       x = "bill depth (mm)",
       y = "bill length (mm)",
       color = "Penguin sex") +
  theme(legend.position = c(0.15, 0.75),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 35, hjust = 0.5),
        plot.subtitle = element_text(size = 30),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 30)) +
  facet_wrap(~species)
bd_bl_pt


bd_bm_pt<-ggplot(my.penguins, aes(x = bill_depth_mm,
                                  y = body_mass_g)) +
  geom_point(aes(color = sex), size=5) +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  labs(title = "Penguin bill depth against Body mass",
       x = "bill depth (mm)",
       y = "Body mass (g)",
       color = "Penguin sex") +
  theme(legend.position = c(0.15, 0.75),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 35, hjust = 0.5),
        plot.subtitle = element_text(size = 30),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 30)) +
  facet_wrap(~species)
bd_bm_pt








ggarrange(fl_bl_pt+ rremove("x.text"), fl_bm_pt + rremove("x.text"),bd_bl_pt,bd_bm_pt,
          ncol = 2, nrow = 2)


?theme



#ISLAND DIFFERNCE
bf_species <- ggplot(data = my.penguins,
       aes(x = flipper_length_mm,
           y = bill_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),size=5) + facet_grid(vars(island), vars(species)) + 
  scale_fill_manual(values = c("Adelie" = "#449dbb",
                               "Chinstrap" = "#4c49b6",
                               "Gentoo" = "#b65449")) + 
  labs(x = "Flipper Length", y = "Bill Length") + guides(fill=guide_legend(title="Species"))+
theme(legend.position = c(0.85, 0.15),
      plot.title.position = "plot",
      plot.caption = element_text(hjust = 0, face= "italic"),
      plot.caption.position = "plot",
      plot.title = element_text(size = 35, hjust = 0.5),
      plot.subtitle = element_text(size = 30),
      axis.text = element_text(size = 25),
      axis.title = element_text(size = 30),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 30),
      strip.text = element_text(size = 30))
bf_species

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()



blbm_species <-ggplot(data = my.penguins,
       aes(x = body_mass_g,
           y = bill_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),size=5) + facet_grid(vars(island), vars(species)) + 
  scale_fill_manual(values = c("Adelie" = "#449dbb",
                               "Chinstrap" = "#4c49b6",
                               "Gentoo" = "#b65449")) + 
  labs(x = "Body mass (g)", y = "Bill Length") + guides(fill=guide_legend(title="Species"))+
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(size = 35, hjust = 0.5),
        plot.subtitle = element_text(size = 30),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        strip.text = element_text(size = 30))



ggarrange(blbm_species+ rremove("x.text"),bf_species+ rremove("x.text"), 
          ncol = 2, nrow = 1)






#Point plot - Bill length against Bill Depth
bill_no_species <- ggplot(data = my.penguins,
                          aes(x = bill_length_mm,
                              y = bill_depth_mm)) +
  geom_point(aes(fill = species,
              color = species,
              shape = species),
              size = 2,
              alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin bill dimensions (omit species)",
       subtitle = "Palmer Station LTER",
       x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  theme(plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  geom_smooth(method = "lm", se = FALSE, color = "gray50")

bill_no_species





flipper_box <- ggplot(data = my.penguins, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Species",
       y = "Flipper length (mm)")+   theme(legend.position = c(0.15, 0.85),
                                           plot.title.position = "plot",
                                           plot.caption = element_text(hjust = 0, face= "italic"),
                                           plot.caption.position = "plot",
                                           plot.title = element_text(size = 35, hjust = 0.5),
                                           plot.subtitle = element_text(size = 30),
                                           axis.title = element_text(size = 30),
                                           legend.title = element_text(size = 30),
                                           legend.text = element_text(size = 30),
                                           axis.text.x = element_text(size = 20))
flipper_box



bill_box <- ggplot(data = my.penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Species",
       y = "Bill length (mm)")+   theme(legend.position = c(0.15, 0.85),
                                        plot.title.position = "plot",
                                        plot.caption = element_text(hjust = 0, face= "italic"),
                                        plot.caption.position = "plot",
                                        plot.title = element_text(size = 35, hjust = 0.5),
                                        plot.subtitle = element_text(size = 30),
                                        axis.title = element_text(size = 30),
                                        legend.title = element_text(size = 30),
                                        legend.text = element_text(size = 30),
                                        axis.text.x = element_text(size = 20))
bill_box


mass_box <- ggplot(data = my.penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Species",
       y = "Body mass (g)")+   theme(legend.position = c(0.15, 0.85),
                                     plot.title.position = "plot",
                                     plot.caption = element_text(hjust = 0, face= "italic"),
                                     plot.caption.position = "plot",
                                     plot.title = element_text(size = 35, hjust = 0.5),
                                     plot.subtitle = element_text(size = 30),
                                     axis.title = element_text(size = 30),
                                     legend.title = element_text(size = 30),
                                     legend.text = element_text(size = 30),
                                     axis.text.x = element_text(size = 20))
mass_box


billd_box <- ggplot(data = my.penguins, aes(x = species, y = bill_depth_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Species",
       y = "Bill depth (mm)")+   theme(legend.position = c(0.15, 0.85),
                                       plot.title.position = "plot",
                                       plot.caption = element_text(hjust = 0, face= "italic"),
                                       plot.caption.position = "plot",
                                       plot.title = element_text(size = 35, hjust = 0.5),
                                       plot.subtitle = element_text(size = 30),
                                       axis.title = element_text(size = 30),
                                       legend.title = element_text(size = 30),
                                       legend.text = element_text(size = 30),
                                       axis.text.x = element_text(size = 20))
billd_box

ggarrange(flipper_box, bill_box, mass_box,billd_box, 
          labels = c("Flipper length (mm), categorised by species", "Bill length (mm), categorised by species", "Body mass (g), categorised by species","Bill depth (mm), categorised by species"),
          ncol = 2, nrow = 2)

table(my.penguins$species)

boxplot(adelie$bill_length_mm~adelie$sex)
boxplot(chinstrap$bill_length_mm~chinstrap$sex)
boxplot(gentoo$bill_length_mm~gentoo$sex)

shapiro.test(my.penguins$body_mass_g)
shapiro.test(my.penguins$bill_length_mm)
shapiro.test(my.penguins$flipper_length_mm)


shapiro.test(adelie$flipper_length_mm)
shapiro.test(gentoo$flipper_length_mm)
shapiro.test(chinstrap$flipper_length_mm)

shapiro.test(adelie$bill_length_mm)
shapiro.test(gentoo$bill_length_mm)
shapiro.test(chinstrap$bill_length_mm)

shapiro.test(adelie$body_mass_g)
shapiro.test(gentoo$body_mass_g)
shapiro.test(chinstrap$body_mass_g)

pnorm(4200, mean(chinstrap$body_mass_g), sd(chinstrap$body_mass_g)) - pnorm(3200,mean(chinstrap$body_mass_g) ,sd(chinstrap$body_mass_g))
pnorm(4200, mean(adelie$body_mass_g), sd(adelie$body_mass_g)) - pnorm(3200,mean(adelie$body_mass_g) ,sd(adelie$body_mass_g))
pnorm(6000, mean(gentoo$body_mass_g), sd(gentoo$body_mass_g)) - pnorm(4500,mean(gentoo$body_mass_g) ,sd(gentoo$body_mass_g))






#Model
my.penguins <- my.penguins %>%
  filter(!is.na(sex)) %>%
  select(-year, -island)
my.penguins

penguin_split <- initial_split(my.penguins, strata = sex)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)
penguin_split

penguin_cv <- vfold_cv(data = penguin_train, v = 10, repeats = 10, strata = sex)
penguin_cv

glm_spec <- logistic_reg() %>%
  set_engine("glm")

penguin_wf <- workflow() %>%
  add_formula(sex ~ .)

### Parallel Processing makes things faster
### tidymodels support parallel processing

doParallel::registerDoParallel()

glm_rs <- penguin_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = penguin_cv,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

glm_rs
collect_metrics(glm_rs)


glm_rs %>% 
  unnest(.metrics) %>% 
  ggplot(aes(id2, .estimate, color = .metric)) + 
  geom_point() +
  labs(title = "Accurary and ACU over Folds and Repetitions",
       x = "Fold", 
       y = NULL,
       color = "Metric") +
  facet_wrap(.metric ~ id) +
  theme(axis.text.x = element_text(size=6, angle = 90)) 

glm_rs %>%
  conf_mat_resampled()

glm_rs %>%
  collect_predictions() %>%
  group_by(id, id2) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id2)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = TRUE, alpha = 0.5, size = 0.8) +
  coord_equal() +
  facet_wrap(~id) + 
  labs(color='Fold', x = "1 - Specificity", y = "Sensitivity", title = "ROC & AUC by Fold and Repeat") +
  theme_minimal()


penguin_final <- penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)

penguin_final

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)

collect_predictions(penguin_final) %>%
  sensitivity(sex, .pred_class)

collect_predictions(penguin_final) %>%
  specificity(sex, .pred_class)

collect_predictions(penguin_final) %>%
  precision(sex, .pred_class)



penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE) %>% 
  select(term, estimate) %>% 
  mutate(term = as.factor(term)) %>% 
  ggplot(aes(reorder(term, estimate), estimate, 
             fill =term)) +
  geom_bar(stat = "identity", show.legend = False, width = 0.7, fill = "#FF6666") + 
  labs(title = "Increase in odds of penguin being Female by one unit increase in each variable",
       x = "Variable", y = "Odds increase by Times") +
  geom_text(aes(label = round(estimate, 3)), 
            nudge_y = 0.15 , 
            size = 4.5, 
            colour = 'black', 
            fontface = 'bold') +
  coord_flip() +
  theme_light()

