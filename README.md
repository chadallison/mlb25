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
  filter(team == "Milwaukee Brewers") |>
  arrange(date) |>
  mutate(rs = rollapply(scored, width = 7, FUN = "mean", align = "right", fill = NA),
         ra = rollapply(allowed, width = 7, FUN = "mean", align = "right", fill = NA),
         game_num = row_number()) |>
  select(game_num, team, rs, ra) |>
  na.omit() |>
  pivot_longer(cols = c("rs", "ra"), names_to = "which", values_to = "runs") |>
  mutate(which = ifelse(which == "rs", "Runs Scored", "Runs Allowed"),
         which = factor(which, levels = c("Runs Scored", "Runs Allowed"))) |>
  ggplot(aes(game_num, runs)) +
  geom_line(aes(col = which), linewidth = 1.5) +
  labs(x = "Game Number", y = "Runs Scored/Allowed", col = NULL,
       title = "Milwaukee Brewers: Runs Scored vs. Allowed in 7-Game Rolling Windows") +
  scale_color_manual(values = c("springgreen4", "indianred3")) +
  scale_x_continuous(breaks = seq(0, 162, by = 10)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1))
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

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
  labs(x = "Game Number", y = "Cumulative Run Differential") +
  scale_x_continuous(breaks = seq(0, 162, by = 20))
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
