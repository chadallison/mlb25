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
- [Biggest Pythagorean Changes in Last
  25](#biggest-pythagorean-changes-in-last-25)
- [Adjusted Pythagorean Wins](#adjusted-pythagorean-wins)
- [Raw vs Adjusted Pythagorean Wins](#raw-vs-adjusted-pythagorean-wins)

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

### Biggest Pythagorean Changes in Last 25

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

------------------------------------------------------------------------

### Adjusted Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

------------------------------------------------------------------------

### Raw vs Adjusted Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

------------------------------------------------------------------------

``` r
start_date = min(end_games$date)
end_date = max(end_games$date)
hitting = baseballr::bref_daily_batter(t1 = start_date, t2 = end_date)

hitting |>
  mutate(pa_pct = percent_rank(PA)) |>
  filter(pa_pct >= 0.25) |>
  mutate(Bases = BB + HBP + X1B + 2 * X2B + 3 * X3B + 4 * HR,
         BPPA = Bases / PA) |>
  arrange(desc(BPPA))
```

    ## # A tibble: 457 × 33
    ##    bbref_id  season Name     Age Level Team      G    PA    AB     R     H   X1B
    ##    <chr>      <int> <chr>  <dbl> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 judgeaa01   2025 Aaron…    33 Maj-… New …   100   447   367    89   128    66
    ##  2 acunaro01   2025 Ronal…    27 Maj-… Atla…    49   211   174    46    57    34
    ##  3 raleica01   2025 Cal R…    28 Maj-… Seat…    98   435   364    67    93    39
    ##  4 ohtansh01   2025 Shohe…    30 Maj-… Los …    99   455   386    94   106    52
    ##  5 kurtzni01   2025 Nick …    22 Maj-… Athl…    62   252   221    39    62    27
    ##  6 suareeu01   2025 Eugen…    33 Maj-… Ariz…    99   408   362    64    93    39
    ##  7 schwaky01   2025 Kyle …    32 Maj-… Phil…   100   443   367    70    91    44
    ##  8 marteke01   2025 Ketel…    31 Maj-… Ariz…    69   302   256    53    74    41
    ##  9 buxtoby01   2025 Byron…    31 Maj-… Minn…    81   348   312    68    91    50
    ## 10 smithwi05   2025 Will …    30 Maj-… Los …    78   314   261    49    85    54
    ## # ℹ 447 more rows
    ## # ℹ 21 more variables: X2B <dbl>, X3B <dbl>, HR <dbl>, RBI <dbl>, BB <dbl>,
    ## #   IBB <dbl>, uBB <dbl>, SO <dbl>, HBP <dbl>, SH <dbl>, SF <dbl>, GDP <dbl>,
    ## #   SB <dbl>, CS <dbl>, BA <dbl>, OBP <dbl>, SLG <dbl>, OPS <dbl>,
    ## #   pa_pct <dbl>, Bases <dbl>, BPPA <dbl>

``` r
pitching = baseballr::bref_daily_pitcher(t1 = start_date, t2 = end_date)

bpip = pitching |>
  mutate(Bases = BB + X1B + 2 * X2B + 3 * X3B + 4 * HR) |>
  separate(IP, into = c("Full_Innings", "Partial_Innings"), sep = "\\.", remove = F, convert = T) |>
  mutate(Partial_Innings = coalesce(Partial_Innings, 0),
         Real_IP = Full_Innings + Partial_Innings / 3,
         BPIP = round(Bases / Real_IP, 3),
         IP_Pct = percent_rank(IP),
         WHIP_Pct = percent_rank(WHIP),
         diff = BPIP - WHIP) |>
  filter(IP_Pct >= 0.75 & WHIP_Pct <= 0.25)

true_worse = bpip |> slice_max(diff, n = 10, with_ties = F) |> pull(Name)
true_better = bpip |> slice_min(diff, n = 10, with_ties = F) |> pull(Name)

bpip |>
  mutate(lbl = ifelse(Name %in% c(true_worse, true_better), Name, "")) |>
  ggplot(aes(WHIP, BPIP)) +
  geom_point() +
  geom_line(stat = "smooth", formula = y ~ x, method = "lm", linetype = "solid", col = "springgreen4") +
  ggrepel::geom_text_repel(aes(label = lbl), size = 3, max.overlaps = 10)
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
pitching |>
  mutate(Bases = BB + X1B + 2 * X2B + 3 * X3B + 4 * HR) |>
  separate(IP, into = c("Full_Innings", "Partial_Innings"), sep = "\\.", remove = F, convert = T) |>
  mutate(Partial_Innings = coalesce(Partial_Innings, 0),
         Real_IP = Full_Innings + Partial_Innings / 3,
         BPIP = round(Bases / Real_IP, 3),
         IP_Pct = percent_rank(IP)) |>
  filter(IP_Pct >= 0.75) |>
  select(Name, Age, Team, IP, ERA, WHIP, BPIP) |>
  slice_min(BPIP, n = 10, with_ties = F) |>
  ggplot(aes(reorder(Name, -BPIP), BPIP)) +
  geom_col() +
  coord_flip()
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
