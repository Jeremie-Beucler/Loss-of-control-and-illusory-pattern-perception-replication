library(pwr)
library(tidyverse)
library(plotly)

nb_participants <- seq(1:600)
effect_size <- seq(1:600)
echantillon_atteint <- 138 # nb of participations to our experiment


# t test
for (i in (1:600))
{sensitivity_t <- pwr.t.test(n = (i + 19), sig.level = 0.05, power = 0.80, type = "two.sample", alternative = "two.sided")
# we put the sensitivity analysis results in a list
nb_participants[i] <- ((i + 19) * 2)
effect_size[i] <- unlist(sensitivity_t[2]) }

data <- data.frame(nb_participants, effect_size)

# t test
possible_criterion <- min(effect_size) + sd(effect_size)

# t test
effet_actuel <- data[nb_participants == echantillon_atteint, 2]
effet_actuel <- round(effet_actuel, 2)

# we plot the distributions
plot <- ggplot(data = data, aes(x=nb_participants, y=effect_size)) + 
  geom_line(size = 1) +
  ggtitle("Analyse de sensibilité - test t groupes indépendants - Alpha = .05 - Puissance = .80") +
  xlab("Taille totale de l'échantillon") +
  ylab("d de Cohen détectable") +
   geom_vline(xintercept = echantillon_atteint, col = "red")  +
  geom_text(x=echantillon_atteint, y=0.36, fontface = 4, size = 3.15,
    label=paste("Echantillon actuel =\nd =", effet_actuel), col = "red") +
    xlim(20, 600)

plot
ggplotly(plot)
