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

``` r
team_runs_scored_by_game = end_games |>
  select(date, team = home_team, runs = home_score) |>
  rbind(end_games |>
  select(date, team = away_team, runs = away_score)) |>
  arrange(team, date)

team_runs_avg_sd = team_runs_scored_by_game |>
  group_by(team) |>
  summarise(avg = mean(runs, trim = 0.1),
            sd = sd(runs)) |>
  mutate(lb = avg - sd,
         ub = avg + sd,
         range = ub - lb)

team_runs_avg_sd |>
  inner_join(teams_info, by = "team") |>
  ggplot(aes(avg, sd)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(team_runs_avg_sd$avg), linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = mean(team_runs_avg_sd$sd), linetype = "dashed", alpha = 0.25) +
  ggrepel::geom_text_repel(aes(label = abb), size = 3, max.overlaps = 32) +
  scale_color_manual(values = team_hex) +
  scale_x_continuous(breaks = seq(0, 10, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5)) +
  labs(x = "Avg. Runs Scored", y = "Runs Scored Volatility (SD)",
       title = "Team runs scored vs. volatility",
       subtitle = "Middle 80% of values -- upper & lower 10% (per team) excluded")
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
team_scored_allowed |>
  mutate(game_num = row_number(), .by = "team") |>
  mutate(group_of_7 = ((game_num - 1) %/% 7) + 1) |>
  group_by(team, group_of_7) |>
  summarise(scored = sum(scored),
            allowed = sum(allowed),
            .groups = "drop") |>
  mutate(py = scored ^ 2 / (scored ^ 2 + allowed ^ 2)) |>
  ggplot(aes(group_of_7, py)) +
  geom_line(aes(col = team), linewidth = 1.5, show.legend = F) +
  facet_wrap(vars(team), scales = "free_x") +
  scale_color_manual(values = team_hex) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
team_game_margins |>
  group_by(team) |>
  summarise(reg = mean(margin),
            trim = mean(margin, trim = 0.1)) |>
  mutate(diff = trim - reg,
         reg_rank = rank(-reg),
         trim_rank = rank(-trim, ties.method = "min")) |>
  arrange(desc(trim))
```

    ## # A tibble: 30 × 6
    ##    team                    reg  trim    diff reg_rank trim_rank
    ##    <chr>                 <dbl> <dbl>   <dbl>    <dbl>     <int>
    ##  1 New York Yankees      2.13  1.91  -0.219         1         1
    ##  2 Detroit Tigers        1.76  1.67  -0.0894        2         2
    ##  3 Los Angeles Dodgers   1.41  1.52   0.101         4         3
    ##  4 New York Mets         1.63  1.42  -0.210         3         4
    ##  5 San Diego Padres      1.23  1     -0.231         5         5
    ##  6 Chicago Cubs          1.22  0.848 -0.371         6         6
    ##  7 Philadelphia Phillies 0.575 0.781  0.206        11         7
    ##  8 Boston Red Sox        0.690 0.765  0.0742        7         8
    ##  9 St. Louis Cardinals   0.585 0.697  0.112         9         9
    ## 10 San Francisco Giants  0.675 0.656 -0.0188        8        10
    ## # ℹ 20 more rows
