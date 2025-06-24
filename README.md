# 🏁 Does Qualifying Really Matter? The Causal Impact of Advancing to Q2 and Q3 on Formula 1 Race Outcomes 🎯

Hi there! 👋

Welcome to my repo, bringing econometrics into the world of Formula 1! 🚦🏎️  

This README will give you an overview of the project — including why I did it, how I approached the analysis, the data I used, and what I discovered. 

This project uses a Regression Discontinuity Design (RDD) to explore a question as old as F1 itself:  
Does qualifying performance on Saturday really impact a driver's race on Sunday?

Thanks for checking out the project!
If you’re into F1 and econometrics, you’re in the right place. 🧠📈🏎️

## 📁 Repo Structure 

```
F1RDDQualy/
├── FinalDataSet.csv         # The full dataset used for the analysis
├── 0.DataExtraction.ipynb   # Jupyter notebook that collects and cleans the raw data using FastF1
├── 1.Results.R              # Runs the main RD estimates using the estimation equation for Q1 and Q2 transitions
└── 2.RobustnessAnalysis.R  # Performs robustness checks by varying bandwidths and cutoffs
```
## 💡 Motivation

For Formula 1 fans, Saturdays mean one thing: Qualifying. It’s when drivers push their cars, and themselves, to the absolute limit in pursuit of that perfect lap. 

But as any fan knows, things don’t always go to plan. Maybe your favorite driver locks up at the worst moment, or a yellow flag ruins their flying lap. Suddenly, they're knocked out early... and you're left dreading what’s likely to be a tough Sunday 😬.

This taps into a common belief, or maybe even a myth, among fans — that a poor qualifying result ruins the race. The idea is that starting further back makes it much harder to recover and score big points.

But is that actually true? 🤔

That’s the question I set out to explore in this project. How much does qualifying really matter? More specifically,  
**does making it to Q2 or Q3 significantly affect a driver's race performance?**


## 🏎️ F1 Qualifying Format



