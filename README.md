Chad’s 2025 MLB Report
================

*Interested in the underlying code that builds this report?* Check it
out on GitHub:
<a href="https://github.com/chadallison/mlb25" target="_blank">mlb25</a>

------------------------------------------------------------------------

### Contents

- [Team Standings](#team-standings)
- [Runs Scored and Allowed per Game](#runs-scored-and-allowed-per-game)
- [NPR](#npr)
- [Offensive and Defensive NPR](#offensive-and-defensive-npr)
- [Run Differentials](#run-differentials)
- [Pythagorean Wins](#pythagorean-wins)
- [Actual vs Pythagorean Win
  Percentages](#actual-vs-pythagorean-win-percentages)
- [Winning and Losing Streaks](#winning-and-losing-streaks)
- [Home and Away Splits](#home-and-away-splits)
- [Rolling Window Pythagorean Wins](#rolling-window-pythagorean-wins)
- [Rolling Window NPR](#rolling-window-npr)
- [Records Against Teams with Winning vs Losing
  Records](#records-against-teams-with-winning-vs-losing-records)
- [Performance Against Teams with Winning
  Record](#performance-against-teams-with-winning-record)
- [NPR Trends](#npr-trends)
- [Runs Scored and Volatility](#runs-scored-and-volatility)
- [Performance by Strength of
  Schedule](#performance-by-strength-of-schedule)
- [One Run Games](#one-run-games)
- [Pythagorean Wins in Last Ten](#pythagorean-wins-in-last-ten)
- [Pythagorean Wins in Last 25](#pythagorean-wins-in-last-25)
- [Adjusted Pythagorean Wins](#adjusted-pythagorean-wins)
- [Raw vs Adjusted Pythagorean Wins](#raw-vs-adjusted-pythagorean-wins)
- [Pythagorean Wins in Groups of Seven - in
  progress](#pythagorean-wins-in-groups-of-seven---in-progress)

------------------------------------------------------------------------

### Team Standings

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

------------------------------------------------------------------------

### Runs Scored and Allowed per Game

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

### NPR

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**What is NPR?**

NPR, Naive Performance Rating, is a metric I devised as a measure of
team performance above/below expected. The logic behind it is this: I
calculate each team’s expected runs scored in each game by taking the
average of their runs scored per game and their opponent’s runs allowed
per game. I then compare this expected value to the actual value of runs
scored or allowed to calculate each team’s offensive and defensive NPR
for each game. Here is an example.

Suppose the Cubs are playing the Cardinals. Let’s say the Cubs, on
average, score 4.5 runs per game and allow 3.25 runs per game. And let’s
say the Cardinals score 3.75 runs per game and allow 2.75 runs per game.
We calculate the Cubs’ expected run value as the average of their runs
scored per game and the Cardinals’ runs allowed per game, so (4.5 +
2.75) / 2 = 3.63. We would calculate the Cardinals’ expected run value
the same way, so (3.75 + 3.25) / 2 = 3.5. We now have the Cubs’ expected
run value as 3.63 and the Cardinals’ expected run value as 3.5.

Suppose that the final score of the game is a Cubs victory, 5-3. We
would calculate the Cubs’ offensive NPR as their actual score minus
their expected score: 5 - 3.63 = 1.37. We would calculate their
defensive NPR as the Cardinals’ expected score minus their actual score:
3.5 - 3 = 0.5 (we do it in this order so positive values are good). For
the Cardinals, their offensive NPR is their actual score minus their
expected score, 3 - 3.5 = -0.5, and their defensive NPR is the Cubs’
expected score minus their actual score, 3.63 - 5 = -1.37. Notice how
these numbers are opposite each other. So each team will have an
offensive and defensive NPR for each game, which are aggregated in the
plot above.

Of course, there are so many other factors that would play into a team’s
true expected value, such as any injuries, starting pitchers, weather,
and more. That is why I have named it Naive Performance Rating, because
it assumes matchup metrics are independent of each other and does not
take external factors into account. Which, of course, will lead to flaws
in the metric, but is done for the sake of simplicity and
interpretability.

------------------------------------------------------------------------

### Offensive and Defensive NPR

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

------------------------------------------------------------------------

### Run Differentials

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

------------------------------------------------------------------------

### Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

------------------------------------------------------------------------

### Actual vs Pythagorean Win Percentages

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

------------------------------------------------------------------------

### Winning and Losing Streaks

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

------------------------------------------------------------------------

### More to come as the season goes on!

------------------------------------------------------------------------

### Home and Away Splits

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

------------------------------------------------------------------------

### Rolling Window Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

------------------------------------------------------------------------

### Rolling Window NPR

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

------------------------------------------------------------------------

### Records Against Teams with Winning vs Losing Records

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

------------------------------------------------------------------------

### Performance Against Teams with Winning Record

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

------------------------------------------------------------------------

### NPR Trends

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

------------------------------------------------------------------------

### Runs Scored and Volatility

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

------------------------------------------------------------------------

### Performance by Strength of Schedule

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

------------------------------------------------------------------------

### One Run Games

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

------------------------------------------------------------------------

### Pythagorean Wins in Last Ten

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

------------------------------------------------------------------------

### Pythagorean Wins in Last 25

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

------------------------------------------------------------------------

### Adjusted Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

------------------------------------------------------------------------

### Raw vs Adjusted Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

------------------------------------------------------------------------

``` r
min_dt = min(end_games$date)
max_dt = max(end_games$date)
pitch = bref_daily_pitcher(t1 = min_dt, t2 = max_dt)

pitch |>
  filter(!str_detect(Team, ",")) |>
  separate(IP, into = c("Full_IP", "Part_IP"), sep = "\\.", remove = F, convert = T) |>
  mutate(Bases = BB + HBP + X1B + 2 * X2B + 3 * X3B + 4 * HR,
         BPIP = round(Bases / (Full_IP + coalesce(Part_IP, 0) / 3), 3)) |>
  slice_max(IP, prop = 0.25) |>
  mutate(pred = predict(lm(BPIP ~ WHIP, data = cur_data())),
         dist_from_line = abs(BPIP - pred)) |>
  mutate(label = if_else(rank(-dist_from_line) <= 10, Name, NA)) |>
  ggplot(aes(WHIP, BPIP, label = label)) +
  geom_point(col = "springgreen4", alpha = 0.25) +
  geom_line(stat = "smooth", formula = y ~ x, method = "lm", linetype = "dashed") +
  ggrepel::geom_text_repel(na.rm = T, size = 3.5, max.overlaps = 10) +
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.2)) +
  labs(title = "WHIP vs. BPIP",
       subtitle = "Players below the dashed line are better than their WHIP suggests")
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

------------------------------------------------------------------------

### Offensive and Defensive NPR Ranks

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

------------------------------------------------------------------------

### Pythagorean Wins in Groups of Seven - in progress

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

------------------------------------------------------------------------

``` r
end_games |>
  select(date, team = home_team, scored = home_score, allowed = away_score) |>
  bind_rows(
    end_games |>
      select(date, team = away_team, scored = away_score, allowed = home_score)
  ) |>
  arrange(team, date) |>
  mutate(rdiff = scored - allowed) |>
  mutate(cum_diff = cumsum(rdiff),
         game_num = row_number(),
         .by = "team") |>
  inner_join(team_divisons, by = "team") |>
  ggplot(aes(game_num, cum_diff)) +
  geom_line(aes(col = team), linewidth = 1.25, show.legend = F) +
  scale_color_manual(values = team_hex) +
  facet_wrap(vars(division)) +
  labs(x = "Game Number", y = "Cumulative Run Differential",
       title = "Cumulative Run Differentials by Division") +
  scale_x_continuous(breaks = seq(0, 162, by = 20))
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
top_teams = team_records |>
  slice_max(win_pct, n = 10, with_ties = F) |>
  pull(team)

# end_with_npr |>
#   transmute(date, team = home_team, npr = home_off_npr + home_def_npr) |>
#   bind_rows(
#     end_with_npr |>
#       transmute(date, team = away_team, npr = away_off_npr + away_def_npr)
#   ) |>
#   arrange(team, date) |>
#   mutate(game_num = row_number(),
#          cum_npr = cumsum(npr),
#          .by = "team") |>
#   inner_join(teams_info, by = "team") |>
#   ggplot(aes(game_num, cum_npr)) +
#   geom_line(aes(col = team), linewidth = 1.25, show.legend = F) +
#   scale_color_manual(values = team_hex)

top_teams = team_records |>
  slice_max(win_pct, n = 10, with_ties = F) |>
  pull(team)

end_with_npr |>
  transmute(date, team = home_team, npr = home_off_npr + home_def_npr) |>
  bind_rows(
    end_with_npr |>
      transmute(date, team = away_team, npr = away_off_npr + away_def_npr)
  ) |>
  arrange(team, date) |>
  mutate(game_num = row_number(),
         cum_npr = cumsum(npr),
         .by = "team") |>
  inner_join(teams_info, by = "team") |>
  mutate(in_top = team %in% top_teams) |>
  ggplot(aes(game_num, cum_npr, group = team)) +
  geom_line(aes(col = team, alpha = in_top), linewidth = 1.25, show.legend = F) +
  scale_color_manual(values = team_hex) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.1)) +
  ggrepel::geom_text_repel(
    data = \(d) d |>
      group_by(team) |>
      slice_tail(n = 1) |>
      filter(team %in% top_teams),
    aes(label = abb, col = team),
    size = 3.5, nudge_x = 5, hjust = 0, direction = "y", segment.color = NA, show.legend = F) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(0, 162, by = 10)) +
  scale_y_continuous(breaks = seq(-300, 300, by = 20))
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
top_teams = team_records |>
  slice_max(win_pct, n = 7, with_ties = F) |>
  pull(team)

plot_data = end_games |>
  transmute(date, team = home_team, margin = home_score - away_score) |>
  bind_rows(
    end_games |>
      transmute(date, team = away_team, margin = away_score - home_score)
  ) |>
  arrange(team, date) |>
  mutate(game_num = row_number(),
         cum_marg = cumsum(margin),
         .by = "team") |>
  filter(team %in% top_teams) |>
  mutate(team = factor(team, levels = top_teams)) |>
  left_join(teams_info |> select(team, abb), by = "team")

# get last point per team for labeling
label_data = plot_data |>
  group_by(team) |>
  slice_max(game_num, n = 1) |>
  ungroup()

ggplot(plot_data, aes(game_num, cum_marg, group = team)) +
  geom_line(aes(col = team), linewidth = 2, show.legend = F) +
  ggrepel::geom_text_repel(data = label_data, aes(label = abb, col = team),
                  hjust = 0, nudge_x = 5, direction = "y", show.legend = F) +
  scale_color_manual(values = setNames(teams_info$hex[match(top_teams, teams_info$team)], top_teams)) +
  labs(x = "Game Number", y = "Cumulative Run Margin",
       title = "Season-long run margin, top five teams by win percentage") +
  scale_x_continuous(breaks = seq(0, 162, by = 10), expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(breaks = seq(-100, 300, by = 20))
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
max_margin = end_games |>
  filter(home_score > away_score) |>
  mutate(margin = home_score - away_score) |>
  slice_min(margin, prop = 0.8, with_ties = F) |>
  pull(margin) |>
  max()

top_teams = team_records |>
  slice_max(win_pct, n = 10, with_ties = F) |>
  pull(team)

plot_data = end_games |>
  transmute(date, team = home_team, scored = home_score, allowed = away_score) |>
  bind_rows(
    end_games |>
      transmute(date, team = away_team, scored = away_score, allowed = home_score)
  ) |>
  arrange(team, date) |>
  filter(abs(scored - allowed) <= max_margin) |>
  mutate(margin = scored - allowed,
         game_num = row_number(),
         cum_marg = cumsum(margin),
         .by = "team") |>
  filter(team %in% top_teams) |>
  left_join(teams_info |> select(team, abb, hex), by = "team") |>
  mutate(team = factor(team, levels = top_teams))

label_data = plot_data |>
  group_by(team) |>
  slice_max(game_num, n = 1) |>
  ungroup()

ggplot(plot_data, aes(game_num, cum_marg, group = team)) +
  geom_line(aes(col = team), linewidth = 1.25, alpha = 0.2, show.legend = F) +
  geom_line(aes(col = team), stat = "smooth", formula = y ~ x, method = "loess", linewidth = 1.25, show.legend = F) +
  ggrepel::geom_text_repel(data = label_data, aes(label = abb, col = team),
                  hjust = 0, nudge_x = 5, direction = "y", show.legend = F) +
  scale_color_manual(values = setNames(teams_info$hex[match(top_teams, teams_info$team)], top_teams)) +
  labs(x = "Game number", y = "Cumulative run margin",
       title = "Cumulative run margins among top teams",
       subtitle = "Games with 80th percentile win margin or greater removed") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
get_team_adj_sos = function(tm) {
  home = end_games |>
    filter(home_team == tm) |>
    count(opp = away_team, name = "games_played")

  away = end_games |>
    filter(away_team == tm) |>
    count(opp = home_team, name = "games_played")

  opps = bind_rows(home, away) |>
    group_by(opp) |>
    summarise(games_played = sum(games_played),
              .groups = "drop")

  opp_win_pct = end_games |>
    filter(home_team != tm & away_team != tm) |>
    mutate(winner = case_when(home_score > away_score ~ home_team,
                              away_score > home_score ~ away_team)) |>
    pivot_longer(cols = c(home_team, away_team), names_to = "loc", values_to = "team") |>
    group_by(team) |>
    summarise(wins = sum(winner == team),
              games = n(),
              win_pct = wins / games,
              .groups = "drop")

  sos = opps |>
    inner_join(opp_win_pct, by = c("opp" = "team")) |>
    summarise(sos = sum(win_pct * games_played) / sum(games_played)) |>
    pull(sos) |> round(3)

  return(sos)
}

data.frame(team = all_teams) |>
  mutate(adj_sos = sapply(team, get_team_adj_sos)) |>
  inner_join(team_records, by = "team") |>
  mutate(win_pct = win_pct / 100) |>
  ggplot(aes(adj_sos, win_pct)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  scale_color_manual(values = team_hex) +
  ggrepel::geom_text_repel(aes(label = abb), size = 3, max.overlaps = 30) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.005), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  labs(x = "Adjusted Strength of Schedule", y = "Win Percentage",
       title = "Win percentage by adjusted opponent strength of schedule")
```

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
games_long = end_games |>
  transmute(date, team = home_team, opp = away_team, margin = home_score - away_score, win_flg = as.integer(home_team == win_team)) |>
  bind_rows(
    end_games |>
      transmute(date, team = away_team, opp = home_team, margin = away_score - home_score, win_flg = as.integer(away_team == win_team))
  )

team_opp_stats = games_long |>
  group_by(team, opp) |>
  summarise(wins_vs_opp = sum(win_flg),
            games_vs_opp = n(),
            .groups = "drop")

team_totals = team_opp_stats |>
  group_by(team) |>
  summarise(total_wins = sum(wins_vs_opp),
            total_games = sum(games_vs_opp),
            .groups = "drop")

team_win_excl = team_opp_stats |>
  inner_join(team_totals, by = "team") |>
  mutate(wins_excl = total_wins - wins_vs_opp,
         games_excl = total_games - games_vs_opp,
         win_pct_excl = ifelse(games_excl > 0, wins_excl / games_excl, NA)) |>
  select(team, excluded_opp = opp, win_pct_excl)

loss_weighting = 1

team_win_pts = games_long |>
  left_join(team_win_excl, by = c("opp" = "team", "team" = "excluded_opp")) |>
  mutate(win_pct_excl = coalesce(win_pct_excl, 0),
         pts = round(ifelse(win_flg == 1, win_pct_excl, -loss_weighting * win_pct_excl), 3)) |>
  group_by(team) |>
  summarise(pts = sum(pts), .groups = "drop") |>
  mutate(pl = ifelse(pts >= 0, round(pts, 2), ""),
         nl = ifelse(pts < 0, round(pts, 2), ""))

team_win_pts |>
  ggplot(aes(reorder(team, pts), pts)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = pl), size = 3, hjust = -0.25) +
  geom_text(aes(label = nl), size = 3, hjust = 1.25) +
  coord_flip(ylim = c(min(team_win_pts$pts) * 1.1, max(team_win_pts$pts) * 1.1)) +
  scale_fill_manual(values = team_hex) +
  labs(x = NULL, y = "Win Points",
       title = "Team Win Points") +
  scale_y_continuous(breaks = seq(-100, 100, by = 5))
```

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
library(plotly)

plot = games_long |>
  left_join(team_win_excl, by = c("opp" = "team", "team" = "excluded_opp")) |>
  mutate(win_pct_excl = coalesce(win_pct_excl, 0),
         pts = round(ifelse(win_flg == 1, win_pct_excl, -0.5 * win_pct_excl), 3)) |>
  arrange(team, date) |>
  mutate(game_num = row_number(),
         cum_pts = cumsum(pts),
         .by = "team") |>
  mutate(roll_cum = rollapply(cum_pts, width = 5, align = "right", FUN = "mean", fill = NA)) |>
  na.omit() |>
  ggplot(aes(game_num, roll_cum)) +
  geom_line(aes(col = team), linewidth = 1.25, show.legend = F) +
  scale_color_manual(values = team_hex) +
  labs(x = "Game number", y = "Cumulative win value", title = "Abstract art") +
  scale_x_continuous(breaks = seq(0, 162, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5))

mode = "normal"
# mode = "plotly"

if (mode == "normal") {
  plot
} else if (mode == "plotly") {
  ggplotly(plot)
}
```

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
top_teams = team_records |>
  slice_max(win_pct, n = 8, with_ties = F) |>
  pull(team)

ordered_hex = teams_info |>
  filter(team %in% top_teams) |>
  mutate(team = factor(team, levels = top_teams)) |>
  arrange(team) |>
  select(team, hex) |>
  deframe()

games_long |>
  left_join(team_win_excl, by = c("opp" = "team", "team" = "excluded_opp")) |>
  mutate(win_pct_excl = coalesce(win_pct_excl, 0),
         pts = round(ifelse(win_flg == 1, win_pct_excl, -1 * win_pct_excl), 3)) |>
  arrange(team, date) |>
  mutate(game_num = row_number(),
         cum_pts = cumsum(pts),
         .by = "team") |>
  mutate(pts_marg = log(abs(margin) + 1) * pts) |>
  filter(team %in% top_teams) |>
  mutate(cpm = cumsum(pts_marg),
         cpm2 = rollapply(cpm, width = 5, align = "right", FUN = "mean", fill = NA),
         .by = "team") |>
  na.omit() |>
  mutate(team = factor(team, levels = top_teams)) |>
  ggplot(aes(date, cpm2)) +
  geom_line(aes(col = team), linewidth = 1.5) +
  scale_color_manual(values = ordered_hex) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(x = NULL, y = "Cumulative victory points", col = NULL,
       title = "Season-long cumulative victory points among top teams")
```

![](README_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->
