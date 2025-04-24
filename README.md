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
plot below.

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

``` r
team_records
```

    ## # A tibble: 30 × 8
    ##    team                  wins losses    gp win_pct record abb   hex    
    ##    <chr>                <dbl>  <dbl> <dbl>   <dbl> <chr>  <chr> <chr>  
    ##  1 Arizona Diamondbacks    14     10    24    58.3 14-10  ARI   #A71930
    ##  2 Athletics               11     13    24    45.8 11-13  ATH   #003831
    ##  3 Atlanta Braves          10     14    24    41.7 10-14  ATL   #CE1141
    ##  4 Baltimore Orioles        9     14    23    39.1 9-14   BAL   #DF4601
    ##  5 Boston Red Sox          14     12    26    53.8 14-12  BOS   #BD3039
    ##  6 Chicago Cubs            16     10    26    61.5 16-10  CHC   #0E3386
    ##  7 Chicago White Sox        5     19    24    20.8 5-19   CWS   #27251F
    ##  8 Cincinnati Reds         12     13    25    48   12-13  CIN   #C6011F
    ##  9 Cleveland Guardians     14     10    24    58.3 14-10  CLE   #0C2340
    ## 10 Colorado Rockies         4     18    22    18.2 4-18   COL   #33006F
    ## # ℹ 20 more rows

``` r
team_npr_by_game |>
  transmute(date, team, npr = off_npr + def_npr) |>
  arrange(team, date) |>
  mutate(game_num = row_number(), .by = "team") |>
  inner_join(select(team_records, team, gp), by = "team") |>
  mutate(l10_group = ifelse(game_num > gp - 10, "last_10", "other")) |>
  group_by(team, l10_group) |>
  summarise(npr = sum(npr),
            .groups = "drop") |>
  pivot_wider(id_cols = "team", names_from = "l10_group", values_from = "npr") |>
  mutate(total = last_10 + other)
```

    ## # A tibble: 30 × 4
    ##    team                 last_10   other  total
    ##    <chr>                  <dbl>   <dbl>  <dbl>
    ##  1 Arizona Diamondbacks  11.8     3.62   15.4 
    ##  2 Athletics              3.38  -11.1    -7.72
    ##  3 Atlanta Braves        -0.173  -5.61   -5.78
    ##  4 Baltimore Orioles    -15.5     0.272 -15.2 
    ##  5 Boston Red Sox        -3.42   -6.60  -10.0 
    ##  6 Chicago Cubs          13.4    13.7    27.0 
    ##  7 Chicago White Sox    -27.2     7.89  -19.3 
    ##  8 Cincinnati Reds       14.5    -0.320  14.2 
    ##  9 Cleveland Guardians   -4.79   -2.43   -7.22
    ## 10 Colorado Rockies     -11.2   -14.0   -25.2 
    ## # ℹ 20 more rows
