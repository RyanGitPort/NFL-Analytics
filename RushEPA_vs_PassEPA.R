##This is from Tej Seth and can be found here: https://www.youtube.com/watch?v=r9SelUodNas
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")
install.packages("gtExtras")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

pbp <- load_pbp(2021:2022)

nrow(pbp)

pbp |> head()

names(pbp)

pbp_rp <- pbp |>
  filter(pass==1 | rush==1)|>
  filter(!is.na(epa))


##Getting top rushers for the Lions
pbp_rp |> 
  filter(posteam == "DET", rush == 1,!is.na(rusher_player_name)) |>
  group_by(rusher_player_name) |>
  summarize(rushes = n(), epa_rush=mean(epa)) |> 
  filter(rushes>=10) |> 
  arrange(-epa_rush)
  


pass_eff<- pbp_rp |> 
  filter(season==2021, pass==1) |> 
  group_by(posteam) |> 
  summarize(passes=n(), pass_epa=mean(epa))

rush_eff<- pbp_rp |> 
  filter(season==2021, rush==1) |> 
  group_by(posteam) |> 
  summarize(rushes=n(), rush_epa=mean(epa))

total_eff<- pass_eff |> 
  left_join(rush_eff, by = "posteam")

View(teams_colors_logos)

total_eff<- total_eff |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


total_eff |> 
  ggplot(aes(x=pass_epa,y=rush_epa)) +
  geom_image(aes(image=team_logo_espn),size=.05,asp=16/9) +
  theme_bw()+
  labs(x="Epa Per Pass",y="EPA Per Rush")


total_eff |> 
  ggplot(aes(x=pass_epa,y=rush_epa)) +
  geom_hline(yintercept=mean(total_eff$rush_epa),linetype="dashed") +
  geom_vline(xintercept = mean(total_eff$pass_epa),linetype="dashed") +
  geom_smooth(method="lm") + 
  geom_image(aes(image=team_logo_espn),size=.05,asp=16/9) +
  theme_bw()+
  labs(x="Epa Per Pass",y="EPA Per Rush",title="EPA/pass and EPA/rush in 2021",
       subtitle="Regular Season and playoffs included",
       caption="By Ryan McCallister | From Tej Seth")
