##Code taught by Tej Seth and can be found here: https://www.youtube.com/watch?v=uJNjCxuBTn8&ab_channel=MFANS
library(tidyverse)
library(nflfastR)
library(vip) ##tells us variable importance
library(ggimage)

pbp<- load_pbp(2018:2022)

fourth_downs<- pbp |> 
  filter(down == 4,!is.na(play_type))

fourth_downs |> 
  group_by(play_type) |> 
  tally(sort = 1) ##counts each occurance of a word

fourth_downs<-fourth_downs |> 
  mutate(went_for_it = ifelse(play_type %in% c("pass","run"), 1, 0))


fourth_downs |> 
  group_by(ydstogo) |> 
  summarize(count=n(),
            went_for_it_rate = mean(went_for_it)) |> 
  filter(count>=5) |> 
  ggplot(aes(x=ydstogo,y=went_for_it_rate))+
  geom_bar(aes(fill=went_for_it_rate),stat="identity")+
  theme_bw()

fourth_downs |> 
  group_by(yardline_100) |> 
  summarize(count=n(),
            went_for_it_rate=mean(went_for_it)) |> 
  filter(count>=5) |> 
  ggplot(aes(x=yardline_100,y=went_for_it_rate))+
  geom_bar(aes(fill=went_for_it_rate),stat="identity")+
  theme_bw()


log_fourth <- glm(went_for_it ~ ydstogo+yardline_100+wp,  ##first variable is one we try to predict, the rest are what we think factor into it
                  data=fourth_downs)

log_fourth


summary(log_fourth)


vip(log_fourth) ##shows t value (importance)


log_fourth_co<- glm(went_for_it ~ (yardline_100 + ydstogo +wp)^2,
                    data=fourth_downs)
summary(log_fourth_co)

fourth_downs |> 
  mutate(pred_prob = log_fourth$fitted.values) |> 
  ggplot(aes(x=ydstogo)) +
  geom_smooth(aes(y=pred_prob), color = "black",size=2)+
  geom_point(aes(y=went_for_it, 
             color = ifelse(went_for_it == 1, "darkgreen","darkred")),
             alpha=.8)+
  scale_color_identity()+
  theme_bw()+
  labs(x="Yards to go",
       y="Chance tema will go for it 0-1")

fourth_downs<- fourth_downs |> 
  mutate(pred_prob = log_fourth$fitted.values) |> 
  mutate(fourths_oe = went_for_it - pred_prob)

team_fourth_22 <- fourth_downs |> 
  filter(season == 2022) |> 
  group_by(posteam) |> 
  summarize(count=n(),
            exp_fourths=sum(pred_prob),
            actual_fourths=sum(went_for_it),
            fourths_oe=sum(fourths_oe)) |> 
  left_join(teams_colors_logos, by = c("posteam"="team_abbr"))


team_fourth_22 |> 
  ggplot(aes(x=exp_fourths, y=actual_fourths))+
  geom_hline(yintercept = mean(team_fourth_22$actual_fourths), linetype="dashed")+
  geom_vline(xintercept = mean(team_fourth_22$exp_fourths), linetype="dashed")+
  geom_smooth(method="lm",color="black",size=1.5, alpha=.5,se=FALSE) +
  geom_image(aes(image=team_logo_espn), asp=16/9, size=.05)+
  theme_bw()+
  labs(x= "Expected 4th Down Go's",
       y="Actual 4th Down Go's",
       title="Team 4th Down Actual and Expected Go's",
       subtitle="Based on a logistic regression model")+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8))

pass_plays<- pbp |> 
  filter(pass==1) |> 
  filter(!is.na(air_yards),!is.na(down),!is.na(wp),!is.na(ydstogo),
         !is.na(half_seconds_remaining))

pass_play_model<-pass_plays |> 
  select(air_yards,down,wp,ydstogo,half_seconds_remaining,season) |> 
  mutate(down=as.factor(down))

str(pass_play_model)

colSums(is.na(pass_play_model))

air_yards_lm<- lm(air_yards~ down +wp+ydstogo+half_seconds_remaining +
                    as.factor(season), data=pass_play_model)

summary(air_yards_lm)


vip(air_yards_lm)

air_yards_preds<- data.frame(predict.lm(air_yards_lm, newdata=pass_play_model)) |> 
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_play_model.)


air_yards_projs<-cbind(pass_plays,air_yards_preds)

ayoe_22<- air_yards_projs |> 
  mutate(ayoe = air_yards - exp_air_yards) |> 
  filter(season==2022) |> 
  group_by(passer_id,passer) |> 
  summarize(passes=n(),
            exp_air=mean(exp_air_yards),
            adot=mean(air_yards),
            avg_ayoe=mean(ayoe)) |> 
  filter(passes>=140)

summary(ayoe_22)

