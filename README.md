# brachiopod_diversity

This repository contains data and code for the brachiopod diversity case study in Allen, Warnock & Dunhill (2025) “A history of the world imperfectly kept”: Will we ever know how biodiversity has changed over deep time? EcoEvoRxiv, https://doi.org/10.32942/X2DD1V

# File structure

The files are arranged as follows:
1. Data
   - brachiopods_clean.csv: The cleaned occurrence dataset
   - counts.csv: A summary table containing the diversity estimates from the different metrics
   - differences.csv: A summary table describing the proportional gains and losses of species and genera across each temporal bin boundary
2. R
   - 1_cleaning.R: Script to run cleaning and preprocessing steps on Paleobiology Database data
   - 2_raw.R: Quantification of the raw diversity curve
   - 3_range_through.R: Quantification of the range-through diversity curve, uses palaeoverse (Jones et al. 2021)
   - 4_rarefaction.R: Quantification of the rarefied diversity curve
   - 5_squares.R: Quantification of the Squares extrapolator diversity curve
   - 6_sqs.R: Quantification of the coverage-based diversity curve, uses iNEXT (Hsieh et al. 2016)
   - 7_residual.R: Quantification of the residual modelling curve, uses code from Lloyd (2012)
   - 8a_DeepDive_prep.R: Preparation of files for DeepDive using DeepDiveR (Cooper et al. 2025)
   - 8b_DeepDive_processing.R: Extraction of the DeepDive diversity curve from Python output (see below)
   - 9_plot_all.R: Script to plot all diversity curves and geological axis
   - 10_differences.R: Script to quantify changes in diversity across temporal bin boundaries (creates differences.csv)
3. DeepDive_inputs
   - brachiopod_config_gen.ini: Configuration file for running genus-level DeepDive analysis
   - brachiopod_deepdive_input_gen.csv: Data file for running genus-level DeepDive analysis
   - brachiopod_config_sp.ini: Configuration file for running species-level DeepDive analysis
   - brachiopod_deepdive_input_sp.csv: Data file for running species-level DeepDive analysis
   - Brachiopod_realms.pdf: Figure showing the occurrence data split into seven geographic regions across which the DeepDive analysis was conducted
   - DeepDive.yml: File for setting up the Jupyter Notebook environment ready to run DeepDive
4. DeepDive_outputs
   - Empirical_predictions_genera.csv: Genus-level diversity estimates for each stage bin (columns) for each of the 20 age replicates (rows)
   - - Empirical_predictions_species.csv: Species-level diversity estimates for each stage bin (columns) for each of the 20 age replicates (rows)
