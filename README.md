# QMES: Quantile Matching Estimation for Tail Risk in Insurance

This repository implements a simulation-based framework to estimate and model sparse, high-volatility losses in insurance excess layers. It uses a dual Pareto distribution fitting method, configurable frequency models, and Monte Carlo simulations to generate a robust understanding of tail risks and their implications for pricing, reinsurance, and capital allocation.

## 🔍 Problem
Insurance excess layers suffer from:
- Sparse and volatile claim data
- Layer-specific risk dynamics
- Complex policy structures (deductibles, limits, reinstatements)

## 🎯 Solution
This project uses **Quantile Matching Estimation (QME)** to fit two Pareto distributions:
1. **Upper Layer Fit:** Tail-optimized for excess losses above a deductible or excess threshold
2. **Lower Layer Fit:** Modeled via MLE for frequent, lower-severity claims

It supports log-normal, Poisson, and fixed-count frequency models, paired with parametric sampling and account-specific logic to simulate full loss distributions.

## 🔄 Methodology
1. **Fit Tail Distributions**:
   - Fit Pareto to upper layer using QME
   - Option to use 'default' or 'at_excess_threshold' tail-fitting methods

2. **Frequency Modeling**:
   - Supports Poisson, log-normal, or fixed claim count

3. **Run Simulations**:
   - Simulates frequency × severity using parametric bootstrapping
   - Applies deductible logic and coverage limits
   - Tracks cumulative deductible consumption and excess payout logic

4. **Evaluation & Diagnostics**:
   - Empirical bootstrap comparison
   - Distributional visualizations: histogram, Q-Q, P-P plots
   - Fit summaries saved as output files

## 💡 Features
- Dual-severity modeling (lower + excess layers)
- Adaptive tail threshold search
- Deductible limit tracking and coverage cap enforcement
- Simulation over multiple iterations for robustness
- YAML-configured parameter control
- Reproducible outputs saved to unique project folders

## 📁 Structure
- `src/`: Core functions (fitters, diagnostics, utilities)
- `Deductible Tracking/`: YAML configs and model execution
- `plots/`: Output fit plots and histograms
- `example.R`: Full simulation pipeline with deductible tracking

## 🧪 Example Output Files
- `fit_pareto_plot.jpg` and `fit_pareto_lower_layer_plot.jpg`
- `simulation_counts.jpg`
- `simulated_loss_distribution.csv`
- `fit_summary.txt`, `final_estimate.txt`, and `empirical_bootstrapped_mean.txt`

## 📦 Usage
Ensure the following before running:
- Required packages installed (see R script)
- Custom YAML file with parameters in `Deductible Tracking/config.yaml`
- Input data column names must include: `loss_amount`, `year_of_loss`

Then simply run:
```r
source('Deductible Tracking/main_simulation_script.R')
```

## 📌 Applications
- Pricing and reserving for excess layer insurance products
- Tail-sensitive capital planning
- Reinsurance purchasing strategy based on exposure analytics
