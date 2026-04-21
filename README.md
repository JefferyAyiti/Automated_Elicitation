# Automated Priors — Project Context

## Overview

This project investigates the use of Large Language Models (LLMs) to automate the elicitation of Bayesian priors in clinical/healthcare settings. The central research question is:

> **"How many patients could we save with LLM priors?"**

The goal is to replace or augment the traditional (manual, expert-driven) prior elicitation process with LLM-generated priors, and to quantify the potential clinical benefit of doing so.

---

## Background

### Bayesian Prior Elicitation

In Bayesian statistics, a **prior distribution** encodes existing beliefs or knowledge about a parameter before observing data. Eliciting informative priors from domain experts is valuable but:

- Time-consuming and resource-intensive
- Subject to cognitive biases
- Difficult to scale across studies

### LLM-Generated Priors

LLMs trained on large corpora of scientific and clinical literature may encode implicit knowledge that can be leveraged to construct informative priors automatically. This project explores whether and how well LLMs can serve as a proxy for expert elicitation.

### Project Brief

A drop-in module for prior elicitation when fitting probabilistic Bayesian models in common statistical frameworks such as Stan. While defining the model likelihood, the tool facilitates elicitation of informative priors, either by designing an elicitation exercise to give to human domain experts, or by synthesizing such priors from a pre-trained generative model. The goal is to enable adoption of informative Bayesian priors based on human or encoded latent world knowledge, and to estimate the reduction in required sample size this may allow.

### Expectation

A simple tool that can be used alongside rstan, rstanarm or brms, evaluated on some example models.

---

## Project Components

### 1. Research Document

**`LLM_Elicitation.pdf`** — Foundational paper/report (9 pages) describing:
- The research question and motivation
- Methodology for using LLMs to elicit priors
- Evaluation framework and results

### 2. Dataset

**`ae_count.csv`** — Clinical trial dataset (NCT00617669):
- 470 patients across 125 sites
- Columns: `study`, `site_number`, `patnum`, `ae_count_cumulative`
- Column mapping for Prior_Validation app: `patnum` → Patient ID, `site_number` → Site ID, `ae_count_cumulative` → AE count

### 3. Elicitation Wizard (R Shiny App)

**`Elicitation_Wizard/`** — Interactive app for LLM-based prior elicitation.

| File | Purpose |
|------|---------|
| `global.R` | Loaded first by Shiny: libraries, shared parsing/utility functions, `source()` calls |
| `ui.R` | Layout only; calls `*_ui()` functions from `pages/` |
| `server.R` | Shared reactive state, elicitation logic; delegates to `*_server()` functions |
| `helpers.R` | Elicitation model list, coding model list, `stan_coder_model_id`, system prompts |
| `chatai_client.R` | Standalone chat client reference |
| `pages/expert_responses.R` | Expert Responses tab — UI + server |
| `pages/prior_summary.R` | Prior Summary tab — UI + server |
| `pages/opinion_pooling.R` | Opinion Pooling tab — UI + server (linear opinion pooling) |
| `pages/delphi.R` | Delphi Method tab — UI + server (multi-round iterative elicitation) |

Run with: `shiny::runApp('Elicitation_Wizard')`

### 4. Prior Validation (R Shiny App)

**`Prior_Validation/`** — Standalone app for validating priors using the Stan model.

| File | Purpose |
|------|---------|
| `ui.R` | UI layout (dataset upload, manual prior inputs, results tables) |
| `server.R` | Server logic (data wrangling, Stan compilation, CV + SE runs) |
| `experiment.R` | Pure R functions: `compute_lpd`, `run_cv`, `run_sample_efficiency`, etc. |
| `poisson_gamma.stan` | Hierarchical Poisson-Gamma Stan model |

Run with: `shiny::runApp('Prior_Validation')`

---

## Technology Stack

| Layer | Technology |
|-------|-----------|
| Frontend / App | R Shiny + bslib |
| Statistical Computing | R + rstan |
| Bayesian Model | Stan (hierarchical Poisson-Gamma) |
| LLM Integration | ellmer (`chat_openai_compatible`) |
| Async / Parallelism | future + promises (`plan(multisession)`, `future_promise()`) |
| UI State Management | shinyjs (button disable/enable during async calls) |
| Markdown + LaTeX Rendering | commonmark (`render_markdown()`), MathJax (`withMathJax()`) |
| Documentation | PDF, Markdown |

---

## Available LLM Models

### Elicitation Models

| Label | Model ID |
|-------|----------|
| MedGemma-27b-it | `medgemma-27b-it` |
| Gemma-3-27b-it | `gemma-3-27b-it` |
| Deepseek-R1-L-70b | `deepseek-r1-distill-llama-70b` |
| Llama-3.3-70b | `llama-3.3-70b-instruct` |
| Qwen-3-I-30b | `qwen3-30b-a3b-instruct-2507` |
| Mistral-3-675B | `mistral-large-3-675b-instruct-2512` |

### Coding Models (Stan Generation)

| Label | Model ID |
|-------|----------|
| Qwen-Coder (default) | `qwen3-coder-30b-a3b-instruct` |
| Devstral-2-123b | `devstral-2-123b-instruct-2512` |

---

## Key Research Questions

1. Can LLMs reliably generate informative priors that match expert-elicited priors?
2. What is the potential patient benefit (lives saved, outcomes improved) from using LLM priors vs. non-informative priors?
3. How should LLM-generated priors be validated and calibrated before use in clinical trials?

---
## Reproducibility
To reproduce the LLM elicitation experiments, an API key for the ChatAI service provided by AcademicCloud at [GWDG](https://docs.hpc.gwdg.de/services/ai-services/saia/index.html) is required. Users must set the `BASE_URL` and `API_KEY` environment variables or enter them directly in the Elicitation Wizard interface.
