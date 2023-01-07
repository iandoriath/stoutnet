# klc.R

library(lme4)
library(tidyverse)
library(nflplotR)

# klc.csv are Clean AUC calculations from KLC.ipynb
data = read.csv('klc.csv')

# fit a mixed effects model with 2 random intercepts
fit = lmer(clean_auc ~ player_diff +  (1|possessionTeam) + (1|defensiveTeam), data=data)

# calculate random effects for each team's offensive and defensive units
ranefs = ranef(fit)

# data frame with random effects
klc = tibble(team = rownames(klc_off), klc_offense = ranefs$possessionTeam$`(Intercept)`, klc_defense = ranefs$defensiveTeam$`(Intercept)`)

# plot KLC scatterplot
klc_plot = klc %>% 
  ggplot(aes(x=klc_offense, y=klc_defense)) +
  theme_bw() +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 1) +
  scale_y_reverse() +
  labs(title = 'KLC: Key Line Composite for Line Play', 
       subtitle = 'Blocking and Rushing on Pass Downs, 2021 Weeks 1-8',
       y = 'Pass Rush KLC',
       x = 'Pass Block KLC') 

# save image
ggsave('klc.jpg', klc_plot, dpi=300, width=2000, height=2000, units = 'px')
