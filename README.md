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
- [Rolling Window Pythagorean Wins (in
  progress!)](#rolling-window-pythagorean-wins-in-progress)
- [Rolling Window NPR (in progress!)](#rolling-window-npr-in-progress)

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

### Rolling Window Pythagorean Wins (in progress!)

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

------------------------------------------------------------------------

### Rolling Window NPR (in progress!)

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

------------------------------------------------------------------------

``` r
cubs_pk = 778305

baseballr::mlb_pbp(game_pk = cubs_pk) |>
  filter(result.description == "Nico Hoerner triples (1) on a sharp line drive to center fielder Jason Heyward. Dansby Swanson scores.")
```

    ## # A tibble: 8 × 152
    ##   game_pk game_date  index startTime    endTime isPitch type  playId pitchNumber
    ##     <dbl> <chr>      <int> <chr>        <chr>   <lgl>   <chr> <chr>        <int>
    ## 1  778305 2025-04-16     7 2025-04-16T… 2025-0… TRUE    pitch f351b…           3
    ## 2  778305 2025-04-16     6 2025-04-16T… 2025-0… TRUE    pitch 31c67…           2
    ## 3  778305 2025-04-16     5 2025-04-16T… 2025-0… TRUE    pitch 6019e…           1
    ## 4  778305 2025-04-16     0 2025-04-16T… 2025-0… FALSE   acti… <NA>            NA
    ## 5  778305 2025-04-16     1 2025-04-16T… 2025-0… FALSE   acti… <NA>            NA
    ## 6  778305 2025-04-16     2 2025-04-16T… 2025-0… FALSE   acti… <NA>            NA
    ## 7  778305 2025-04-16     3 2025-04-16T… 2025-0… FALSE   acti… <NA>            NA
    ## 8  778305 2025-04-16     4 2025-04-16T… 2025-0… FALSE   step… f6bda…          NA
    ## # ℹ 143 more variables: details.description <chr>, details.event <chr>,
    ## #   details.awayScore <int>, details.homeScore <int>,
    ## #   details.isScoringPlay <lgl>, details.hasReview <lgl>, details.code <chr>,
    ## #   details.ballColor <chr>, details.isInPlay <lgl>, details.isStrike <lgl>,
    ## #   details.isBall <lgl>, details.call.code <chr>,
    ## #   details.call.description <chr>, count.balls.start <int>,
    ## #   count.strikes.start <int>, count.outs.start <int>, player.id <int>, …
