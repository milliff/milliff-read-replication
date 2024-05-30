# Replication Package for "Understanding Majoritarian Violence"

## Description

This repository contains the replication package for the paper:

Milliff, Aidan and Blair Read. 2024. "Understanding Majoritarian Violence: Evidence from Hate Crimes and Crowd Violence in India."

The replication package provides all the necessary resources and instructions to reproduce the experiments and results presented in the paper.

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Replication](#replication)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)

## Introduction

A brief introduction to the paper and its objectives.

## Installation

We use the `renv` package to create an `R` "environment" that makes it easy to install (correct versions of) all the software necessary to reproduce our analysis. 

To set up the correct environment, simply:

  1. Clone this repository, and;
```
usethis::create_from_github(
  'milliff/milliff-read-replication',
  destdir = "[YOUR TARGET DIRECTORY]",
  fork = F,
  protocol = 'https'
)
```
  2. Run the following commands to load all the required packages (listed in /milliff-read-repository/renv.lock) into your environment
```
renv::init()
renv::restore()
```

Note: We used `R 4.3.1 (Beagle Scouts)` on an `aarch64-apple-darwin20 (64-bit)` platform running macOS Sonoma 14.5 for all the results in the paper.

## Replication

Each numbered item in the list below refers to an individual source code file in /milliff-read-replication/code/. For each file, we provide a brief description of the analyses, list the data sources it calls from /milliff-read-replication/data/, and the outputs it creates in /milliff-read-replication/results/.

1. 01-hcw-descriptive-statistics.R
    Description: Summarizes hate crime data from India Religious Hate Crime Watch
    Data: hc_locs.csv; subdistricts.json
    Outputs: Figure 1; Figure 3B; Figure A.1; Table A.1; Supplementary Figures

2. 02-vb-descriptive-statistics.R
    Description: Summarizes Vidhya Bharati schools data
    Data: vb_locs.csv; subdistricts.json
    Outputs: Figure 3a; Figure 6; Supplementary Figures/Tables

3. 03-main-estimation.R
    Description: Estimates primary results (relationship between VB schools and hate crimes)
    Data: main_df.RData
    Outputs: Figure 4; Figure 5; Table A.2; Table A.3; Table A.4

4. 04-treatment-duration-estimation.R
    Description: Estimates VB-hate crime relationship as a function of VB duration in community
    Data: main_df.RData
    Outputs: Figure 7; Figure 8; Figure 9; Figure A.2; Table A.8

 5. 05-additional-estimation.R
    Description: Re-estimates main results, showing robustness to different functional forms and model families.
    Data: main_df.RData
    Outputs: Table A6; Table A7

6. 06-state-weakness.R
    Description: Builds/compares measures of state capacity, a key alternative explanation
    Data: main_df.RData
    Outputs: Figure 10; Table A.5; Table A.10; Table A.11

7. 07-vb-placement.R
    Description: Models placement of VB schools, the main IV
    Data: main_df.RData
    Outputs: Table A.12; Table A.13

8. 08-diff-in-diff-estimation.R
    Description: Estimates VB-Hate Crime association as staggered-treatment difference-in-differences
    Data: did_df.RData
    Outputs: Figure A.3; Figure A.4; Table A.14

9. 09-matching-estimation.R
    Description: Estimates VB-Hate Crime association using matching and balancing
    Data: main_df.RData; genmatch.RData; nnmatch.RData
    Outputs: Supplementary Figures

10. 10-alternative-explanations.R
    Description: Addresses additional alternative explanations including school expansion (non VB), perpetrator impunity (via governing party), schools following HCs
    Data: main_df.RData
    Outputs: Supplementary Figures

11. 11-effects-pre-2014.R
    Description: Separates estimation into pre- and post-2014 election
    Data: main_df.RData
    Outputs: Supplementary Figures

12. 12-scaling-estimation.R
    Description: Replicates 3 (main-estimation.R) with scaled variables to ease effect interpretation
    Data: main_df.RData
    Outputs: Supplementary Figure

13. __data-creation-pseudocode.R
    Description: Provides instructions for re-creating main datasets
    Data: Does not include data; Many constituent data-sets cannot be reshared in original format
    Outputs: main_df.RData; did_df.RData

## License

The software in this replication package is licensed under CC BY-NC 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc/4.0/.

This license requires that reusers give credit to the creator. It allows reusers to distribute, remix, adapt, and build upon the material in any medium or format, for noncommercial purposes only.

## Contact

Direct questions, complaints, and compliments to ![Aidan Milliff](mailto:milliff.a@gmail.com)

## Citation

If you use this replication package or data in your research, please cite the original paper:

Milliff, Aidan and Blair Read. 2024. "Understanding Majoritarian Violence: Evidence from Hate Crimes and Crowd Violence in India."

