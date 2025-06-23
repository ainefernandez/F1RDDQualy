# 🏁 Does Qualifying Really Matter? The Causal Impact of Advancing to Q2 and Q3 on Formula 1 Race Outcomes 🎯

Hi there! 👋

Welcome to my repo — bringing econometrics into the world of Formula 1! 🚦🏎️  
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




























