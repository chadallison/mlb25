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
- [Pythagorean Wins in Last Seven](#pythagorean-wins-in-last-seven)

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

### Pythagorean Wins in Last Seven

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

------------------------------------------------------------------------

### Biggest Pythagorean Changes in Last 25

``` r
team_scored_allowed |>
  arrange(team, date) |>
  mutate(game_num = row_number(),
         games_played = n(),
         .by = "team") |>
  filter(game_num < games_played - 25) |>
  group_by(team) |>
  summarise(scored = sum(scored),
            allowed = sum(allowed)) |>
  transmute(team, py_prev = round(scored ^ 2 / (scored ^ 2 + allowed ^ 2), 3)) |>
  inner_join(team_pythag_small, by = "team") |>
  inner_join(teams_info, by = "team") |>
  mutate(diff = pythag - py_prev) |>
  arrange(desc(diff)) |>
  ggplot(aes(py_prev, pythag)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.25) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.25) +
  # geom_abline(linetype = "solid", col = "red") +
  scale_color_manual(values = team_hex) +
  ggrepel::geom_text_repel(aes(label = abb), size = 3, max.overlaps = 30) +
  labs(x = "Pythagorean Wins as of 25 Games Ago",
       y = "Current Pythagorean Wins",
       title = "Pythagorean Movement in Past 25 Games",
       subtitle = "Teams above dashed line have improved in their last 25 relative to other teams") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent)
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
team_names = c()
model_coefs = c()

for (tm in all_teams) {
  data = team_scored_allowed |> filter(team == tm)
  model = lm(allowed ~ scored, data = data)
  coef = round(model$coefficients[[2]], 3)
  team_names = append(team_names, tm)
  model_coefs = append(model_coefs, coef)
}

res = data.frame(
  team = team_names,
  model_coef = model_coefs
)

res |>
  arrange(desc(model_coef))
```

    ##                     team model_coef
    ## 1   San Francisco Giants      0.332
    ## 2   Arizona Diamondbacks      0.283
    ## 3   Washington Nationals      0.245
    ## 4       Colorado Rockies      0.222
    ## 5  Philadelphia Phillies      0.174
    ## 6          Miami Marlins      0.145
    ## 7         Boston Red Sox      0.135
    ## 8       New York Yankees      0.124
    ## 9    St. Louis Cardinals      0.105
    ## 10    Los Angeles Angels      0.093
    ## 11        Tampa Bay Rays      0.078
    ## 12          Chicago Cubs      0.053
    ## 13       Cincinnati Reds      0.036
    ## 14    Pittsburgh Pirates      0.035
    ## 15        Houston Astros      0.023
    ## 16      San Diego Padres      0.000
    ## 17    Kansas City Royals     -0.007
    ## 18   Los Angeles Dodgers     -0.014
    ## 19        Atlanta Braves     -0.022
    ## 20         New York Mets     -0.024
    ## 21     Milwaukee Brewers     -0.033
    ## 22      Seattle Mariners     -0.040
    ## 23         Texas Rangers     -0.059
    ## 24   Cleveland Guardians     -0.090
    ## 25       Minnesota Twins     -0.102
    ## 26        Detroit Tigers     -0.145
    ## 27     Toronto Blue Jays     -0.148
    ## 28     Baltimore Orioles     -0.221
    ## 29     Chicago White Sox     -0.309
    ## 30             Athletics     -0.323
