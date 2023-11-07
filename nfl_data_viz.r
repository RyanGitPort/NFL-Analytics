##this code is from Tej Seth and can be found here https://www.youtube.com/watch?v=vVOagBhHo9A&t=1564s&ab_channel=MFANS
pbp<- load_pbp(2019:2022)

qb_epa_play<- pbp |> 
  filter(pass == 1 | rush==1) |> 
  filter(!is.na(epa)) |> 
  group_by(id) |> 
  summarize(name = first(name),
            team=last(posteam),
            plays=n(),
            epa_play=mean(epa),
            pass_attempts=sum(incomplete_pass+complete_pass, na.rm=1)) |> 
  filter(plays>=1000,pass_attempts>=150) |> 
  mutate(pass_rate = pass_attempts/plays) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))




qb_epa_play |>
  ggplot(aes(x = pass_rate,y= epa_play)) +
  geom_point(aes(fill=team_color,color=team_color2,size=plays),
             shape=21, alpha=0.9) +
  scale_color_identity(aesthetics=c("fill","color"))+
  ggrepel::geom_label_repel(aes(label=name))+
  theme_bw() + 
  geom_hline(yintercept = mean(qb_epa_play$epa_play), linetype="dashed") +
  geom_vline(xintercept = mean(qb_epa_play$pass_rate), linetype="dashed") +
  labs(x= "Pass Rate",
       y="EPA/Play",
       title= "EPA/Play and Pass Rate, 2019-2022",
       subtitle = "Minimum of 1000 plays and 150 pass attempts to be included",
       caption="By Ryan McCallister | From Tej Seth") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))+
  theme(plot.title = element_text(size=22, hjust=.5, face = "bold"),
        plot.subtitle=element_text(size=16, hjust=.5))
ggsave('epa-pass-rate.png',width=14,height=10,dpi="retina")


qb_epa_play |> 
  ggplot(aes(x=epa_play, y=fct_reorder(name,epa_play)))+
  geom_bar(aes(fill = team_color, color = team_color2), stat="identity",alpha=.9) +
  scale_color_identity(aesthetics = c("fill","color")) +
  theme_bw() +
  geom_image(aes(image = team_logo_espn,x=ifelse(epa_play>0, epa_play+.01,epa_play-.01)),
             asp=16/9, size=.02)+
  labs(x= "EPA/Play",
       y="Quarterback",
       title= "Each Quarterback EPA/Play, 2019-2022",
       subtitle = "Minimum of 1000 plays and 150 pass attempts to be included",
       caption="By Ryan McCallister | From Tej Seth")+
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=22, hjust=.5, face = "bold"),
        plot.subtitle=element_text(size=16, hjust=.5))
ggsave('bar-epa-qb.png',width=14,height = 10,dpi='retina')
  
qb_gt<-qb_epa_play |> 
  arrange(-epa_play) |>
  mutate(rank=row_number()) |> 
  dplyr::select(rank,name,team_wordmark,pass_attempts,plays,pass_rate,epa_play) |> 
  mutate(pass_rate = 100*round(pass_rate,3),
         epa_play=round(epa_play,2)) |> 
  gt() |> 
  cols_align(align="center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(rank="Rank",
             name="Quarterback",
             team_wordmark="Team",
             pass_attempts="Pass Attempts",
             plays="Plays",
             pass_rate="Pass Rate",
             epa_play="EPA Per Play") |> 
  gtExtras::gt_theme_espn() |> ##makes rows look like espn
  gtExtras::gt_hulk_col_numeric(epa_play) ##heat color indicated column with 
  
gtsave(qb_gt,"qb_qt.png")
 
