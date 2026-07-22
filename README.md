# 🏁 Does Qualifying Really Matter? The Causal Impact of Advancing to Q2 and Q3 on Formula 1 Race Outcomes 🎯

Hi there! 👋

Welcome to my repo, where econometrics meets the world of Formula 1! 🚦🏎️

This README provides an overview of the project — including the motivation behind it, the methodology, the data used, and the key findings.

The project applies a **Regression Discontinuity Design (RDD)** to investigate a question as old as F1 itself:
> Does qualifying performance on Saturday impact a driver's race outcome on Sunday?

Thanks for checking out the project!
If you're into F1 and econometrics, you're in the right place. 🧠📈🏎️

📄 **You can read the full research paper here:**
[Does Qualifying Really Matter? The Causal Impact of Advancing to Q2 and Q3 on Formula 1 Race Outcomes](https://github.com/ainefernandez/F1RDDQualy/blob/main/DoesQualiReallyMatter.pdf)

## 📁 Repo Structure

```
F1RDDQualy/
├── FinalDataSet.csv          # The full dataset used for the analysis
├── 0.DataExtraction.ipynb    # Jupyter notebook that collects and cleans the raw data using FastF1
├── 1.Results.R               # Runs the main RD estimates using the estimation equation for Q1 and Q2 transitions
├── 2.RobustnessAnalysis.R    # Performs robustness checks by varying bandwidths and cutoffs
└── DoesQualiReallyMatter.pdf # Full research paper
```

## 💡 Motivation

For Formula 1 fans, Saturdays mean one thing: Qualifying. It's when drivers push their cars, and themselves, to the absolute limit in pursuit of that perfect lap.

But as any fan knows, things don't always go to plan. Maybe your favorite driver locks up at the worst moment, or a yellow flag ruins their flying lap. Suddenly, they're knocked out early... and you're left dreading what's likely to be a tough Sunday 😬.

This taps into a common belief, or maybe even a myth, among fans, that a poor qualifying result ruins the race. The idea is that starting further back makes it much harder to recover and score big points.

But is that actually true? 🤔

That's the question I set out to explore in this project. How much does qualifying really matter? More specifically,
**does making it to Q2 or Q3 significantly affect a driver's race performance?**

## 🏎️ F1 Qualifying Format

> **📌 Note on replicating with newer data:** This analysis is based on data from **2019 to 2024** (2024 through the Italian GP at Monza), when the grid had 20 drivers. Starting in 2026, the grid expanded to 22 drivers. If you want to extend this analysis with 2026+ data, the knockout thresholds shift accordingly: Q1 and Q2 each eliminate 6 drivers instead of 5 (22 → 16 → 10), while the Q3 top-10 shootout stays exactly the same. The running variable definition and RDD logic in this repo carry over unchanged, you'd just need to recompute `GapToKnockout` against the new 16th-fastest (Q1) and 10th-fastest (Q2) times.

F1 qualifying is a knockout format split into three sessions:

| Session | Cars | Time | What happens |
|---|---|---|---|
| **Q1** | 20 | 18 min | 5 slowest drivers eliminated → positions 16th–20th set |
| **Q2** | 15 | 15 min | 5 slowest drivers eliminated → positions 11th–15th set |
| **Q3** | 10 | 12 min | Fastest driver takes pole → positions 1st–10th set |

Each session resets,  the order from Q1 doesn't carry into Q2, and so on. To advance, a driver simply needs to beat the knockout threshold: the 15th-fastest time in Q1, or the 10th-fastest time in Q2. That's what makes this setting so useful for causal inference, it's a hard cutoff, not a fuzzy one.


I use this cutoff to build two treatment variables:
- **MadeItToQ2** — did the driver advance out of Q1?
- **MadeItToQ3** — did the driver advance out of Q2?

## 🧮 Methodology

The knockout structure of F1 qualifying is what makes a **Regression Discontinuity Design (RDD)** such a natural fit. Drivers who just barely scrape into the next session and drivers who just barely miss out are, in principle, very similar in skill and car performance — the only thing separating them is which side of the knockout line they landed on. That lets us treat "making it through" as good-as-random for drivers close to the cutoff, and isolate its effect on what happens the next day.

**Running variable:** `GapToKnockout`, the (standardized) difference between a driver's best lap and the knockout time in that session. A positive gap means the driver advanced; negative means eliminated.

**Design type:** This is a **Sharp RDD**: the probability of advancing jumps cleanly from 0 to 1 exactly at the cutoff, with no fuzziness in treatment assignment.

**Estimating equation:**

$$
Outcome_i = \tau_{SRD}\,MadeItToQ2/Q3_i + \alpha_0\,GapToKnockout_i + \alpha_1\,(GapToKnockout_i \times MadeItToQ2/Q3_i) + \lambda_{circuit\text{-}team_i} + \epsilon_i
$$

where $\tau_{SRD}$ is the causal effect of interest, and $\lambda_{circuit\text{-}team_i}$ are circuit–team fixed effects that absorb the fact that some teams are simply faster at some tracks than others.

**Identifying assumptions:**
- **Continuity**: outcomes and covariates should evolve smoothly through the cutoff; any jump *at* the cutoff is attributable to treatment.
- **No sorting**: drivers shouldn't be able to precisely manipulate their lap time to land just above the threshold.

I check both assumptions graphically (plotting outcomes and covariates against the running variable) and formally, using a **McCrary (2008) density test** for sorting around the cutoff.

One thing that is worth noting is that the McCrary test *does* detect some density discontinuity at the cutoff, but this is less worrying than it sounds. Drivers and engineers know exactly what time they need to beat, and strategically manage tyre usage and lap timing around it (saving a fresh set of softs, pushing harder on a "banker lap," etc.). That's *strategic behavior*, not the kind of fine-grained manipulation that would threaten the design. 

To test how reliable the results are, I run two robustness checks:
- **Bandwidth sensitivity**: re-estimating across bandwidths from 0.05s to 1.05s
- **Cutoff placebo test**: artificially shifting the cutoff to -1, -0.5, 0.5, and 1 to confirm effects only show up at the true cutoff (0)

## 📊 Data

- **Source:** Formula 1's official timing data, pulled via the [FastF1](https://github.com/theOehrly/Fast-F1) Python library
- **Coverage:** every Grand Prix weekend from **2019 to 2024** (2024 season included through the Italian GP at Monza)
- **Sample size:** 2,319 driver-race observations
- **Unit of observation:** one driver, one race weekend

| Category | Variables |
|---|---|
| **Running variables** | `GapToKnockoutQ1`, `GapToKnockoutQ2` |
| **Treatment variables** | `MadeItToQ2`, `MadeItToQ3` (binary) |
| **Covariates** | `GapToFastestFP1/2/3`, `ChampionshipPoints`, `TeamChampionshipPoints`, `LastRacePosition`, `TeammateLastRacePosition` |
| **Outcomes** | `GridPosition`, `RacePosition`, `Points`, `DNF` (overall / driving / mechanical), `StartingTyre`, `NumberOfPitStops`, `AlternativeStrategy`, `Pitlane` |

Covariates are chosen because they *shouldn't* jump at the cutoff, they reflect who the driver is and how their weekend/season has gone so far, not what happened in that specific qualifying session. Confirming they're balanced around the cutoff is what makes the design credible.



## 📈 Results

**Covariate balance:** Nearly every covariate is balanced across the cutoff for both Q2 and Q3, supporting the validity of the design. The one exception is `LastRacePosition` around the Q2 cutoff, which is significant at the 1% level.

**Headline effects** (estimated at the bandwidth that minimizes MSE):

| Outcome | Effect of Making It to Q2 | Effect of Making It to Q3 |
|---|---|---|
| Grid Position | **−3.63 positions**\*\*\* | **−1.86 positions**\*\*\* |
| Race Position | **−2.02 positions**\*\*\* | **−0.94 positions**\* |
| Points | **+1.04 points**\*\* | not significant |
| Probability of DNF | **reduces by 0.096**\*\*\* | not significant |
| Probability of Mechanical DNF | **reduces by 0.050**\*\* | not significant |
| Probability of Driving DNF | not significant | not significant |
| Alternative Tyre Strategy | not significant | not significant |
| Number of Pit Stops | not significant | not significant |
| Probability of Pitlane Start | **reduces by 0.056**\*\*\* | not significant |

*(\*\*\* p<0.01, \*\* p<0.05, \* p<0.10 — negative = better/lower position or lower probability)*

A few things stand out:

- **Advancing to Q2 matters the most.** It's the treatment with the largest, most consistent effects — better grid position, better race finish, more points, fewer DNFs (especially mechanical ones), and a lower chance of a pitlane start.
- **Advancing to Q3 still helps, but less.** The effect on grid and race position is smaller and, for race position, only mildly significant. Its effects on points, DNF probability, and pitlane starts aren't statistically significant — likely because Q3 drivers are already starting from strong positions, so there's less room left to gain.
- **Tyre strategy is more nuanced.** Making it to Q2 shifts drivers *away* from starting on hards and *toward* mediums, while making it to Q3 significantly increases the chance of starting on softs — consistent with Q3 drivers using their fresh soft sets from the pole shootout. But neither treatment significantly affects the number of pit stops or the likelihood of running an alternative strategy — surprising, since one might expect drivers who qualify poorly to gamble more on strategy to recover positions.

**Robustness:** Results for grid position, race position, and overall DNF are robust across bandwidths (0.05s–1.05s) and hold up under a cutoff-placebo test, significant effects appear only at the true cutoff, not at artificially shifted ones. A handful of secondary results (the Mechanical DNF effect for Q2, the Points effect for Q3, and the Medium tyre effect for Q2) are less robust, with significance appearing in less than half of the bandwidths tested.

## 🏆 Conclusions

So, does qualifying really matter? Yes, but mostly in one specific way: **making it to Q2**.

- Advancing to Q2 has the largest and most robust impact on race outcomes: roughly **3.6 grid places** and **2 race positions** better, plus a **meaningfully lower chance of DNF**.
- Advancing to Q3 helps too, but the marginal gain is smaller,  about **1.9 grid places** and **1 race position**.
- Some outcomes teams might expect to be affected, points (for Q3), pit stop strategy, alternative strategies — show no significant effect, suggesting these are shaped more by in-race circumstances than by qualifying position itself.

**Practical takeaway for teams:** if you're on the edge of elimination in Q1, fighting for Q2 should be the priority, it's where the biggest performance gains live, even if Q3 slips out of reach.

**The bigger picture:** part of what determines whether a driver makes it through a knockout session — a yellow flag at the wrong moment, a rival's mistake, a red flag saving your lap — is pure chance. This project's findings echo a broader pattern in the causal inference literature: from exam luck shaping long-term outcomes in Norway (Landaud et al., 2022) to marginal college admission cutoffs affecting lifetime earnings (Zimmerman, 2014), small, arguably random advantages at a critical threshold can meaningfully shape what happens next. In F1, that means a driver's Sunday can hinge as much on a lucky (or unlucky) Saturday as it does on raw pace.

Thanks for reading! If you want the full detail, literature review, all the graphical RDD diagnostics, and the complete set of robustness tables, check out the [paper](https://github.com/ainefernandez/F1RDDQualy/blob/main/DoesQualiReallyMatter.pdf). 🏁
