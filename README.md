# CoACA: Cognitive Activity-Based Credit Assignment

Implementation of the **CoACA** framework and hold-out strategy inference method, as described in:

> **Strategy Inference During Learning via Cognitive Activity-Based Credit Assignment Models**
> Ashwin James*, Patricia Reynaud-Bouret*, Giulia Mezzadri, Francesca Sargolini, Ingrid Bethus, Alexandre Muzy
> *Scientific Reports (Nature), 13, 9408 (2023)*
> DOI: [10.1038/s41598-023-33604-2](https://doi.org/10.1038/s41598-023-33604-2)
> *(Equal first-author contribution)*

---

## Overview

CoACA is a cognitive variant of Activity-Based Credit Assignment (ACA) for modeling and
inferring the learning strategies of individuals from free behavioral data — without probe
tests or auxiliary variables.

The framework combines:
- **CoACA models** — six chunking strategies (Path, Hybrid1–4, Turn) representing different
  granularities of spatial decision-making in a T-maze
- **Hold-out strategy inference** — a novel cross-validation method adapted for non-i.i.d.
  sequential learning data, enabling strategy selection from a single individual

Applied to rat T-maze data, the method identified the **Hybrid 3 model** (deliberation at
Junction A) as the dominant strategy — validated by dorsomedial striatum neural recordings.

---

## Repository Structure

| File/Folder | Description |
|---|---|
| `src/` | C++ simulation code (via Rcpp) |
| `R/` | R analysis scripts, hold-out procedure, MLE estimation |
| `data/` | Rat behavioral trajectory data |
| `results/` | Model selection outputs and figures |

---

## Dependencies

- R (≥ 4.0)
- `Rcpp`, `Rmpi` — for parallel simulation
- `DEoptim` — for maximum likelihood estimation
- `ggplot2` — for visualization

Install R dependencies:
```r
install.packages(c("Rcpp", "Rmpi", "DEoptim", "ggplot2"))
```

---

## Usage

```r
# Run MLE parameter estimation
source("R/run_mle.R")

# Run hold-out strategy selection
source("R/run_holdout.R")

# Generate figures
source("R/plot_results.R")
```

> **Note:** Full simulations were run on the NEF/OPAL HPC cluster (320+ cores).
> Local runs may require reducing the number of parallel jobs.

---

## Key Results

All four rats were best described by the **Hybrid 3** model — deliberating at Junction A —
consistent with neuronal coding in the dorsomedial striatum showing action anticipation
before reaching Junction B.

| Rat | Selected Model | Log-likelihood |
|---|---|---|
| Rat 1 | Hybrid 3 | −739.04 |
| Rat 2 | Hybrid 3 | −878.77 |
| Rat 3 | Hybrid 3 | −446.99 |
| Rat 4 | Hybrid 3 | −715.69 |

---

## Citation

```bibtex
@article{james2023coaca,
  title={Strategy inference during learning via cognitive activity-based credit assignment models},
  author={James, Ashwin and Reynaud-Bouret, Patricia and Mezzadri, Giulia and
          Sargolini, Francesca and Bethus, Ingrid and Muzy, Alexandre},
  journal={Scientific Reports},
  volume={13},
  pages={9408},
  year={2023},
  doi={10.1038/s41598-023-33604-2}
}
```

---
