# Getting started (on my mac)

## Installing PyRate
1. install python3
2. Clone the PyRate repo
    git clone https://github.com/dsilvestro/PyRate.git
2. Run the following to create a virtual environment & install dependencies within that environment
    python3 -m venv pyrate_env
    source pyrate_env/bin/activate
    python3 -m ensurepip --upgrade
    python3 -m pip install --upgrade pip
    python3 -m pip install -r PyRate/requirements.txt

See tutorial 0 for more details.

## Sanity check
PyRate is set up to run from the PyRate directory (I don't see a way to change this easily). If everything is set up correctly, the following will output the FAs and LAs for each taxa
    cd PyRate
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -data_info

See tutorial 1 for more info about file formats.
See also example file: PyRate/example_files/Ursidae_PyRate.py.
Note age uncertainty is handled within the data input file.

## Quick start
To run the analysis you can simply run
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py

The output will appear in the working directory that contains the input file. Models and default settings described below.

# Model options

The standard version of PyRate implements a standard birth-death process model, with constant rates or  variation in rates through time. Rates can be estimated for fixed intervals or rate shifts can be estimated using revisible jump MCMC. There are many options for modeling variation in fossil preservation, including variation across intervals, lineages, or both. The tutorials describe the options for the birth-death and fossil preservation processes separately, beginning with the latter, which I initially found confusing coming from the FBD world. I've tried to summarise the relevant options below. Most of this info comes from tutorial 1.

Based on our other analyses, I recommend we try the following, options detailed below:
1. Piecewise constant variation in birth, death, and sampling, with fixed intervals for all 3 parameters (I think this is most similar to the trad paleo approaches).
2. Piecewise constant variation in birth, death, and sampling. Use fixed intervals for sampling only. Estimate shifts in birth and death using RJ MCMC. 
3. Same as 2 but add variation across lineages.

    # 1
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -A 2 -fixShift ~/brachiopod_diversity/pyrate/epochs.txt -qShift ~/brachiopod_diversity/pyrate/epochs.txt
    # 2
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -qShift ~/brachiopod_diversity/pyrate/epochs.txt
    # 3 (although I don't think this works with Gibbs sampling)
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -qShift ~/brachiopod_diversity/pyrate/epochs.txt -mG

## Variation in birth and death
By default PyRate uses the option with piecewise constant variation in birth and death, with rate shifts estimated using RJ MCMC. This can be toggled on and off or made explicit using the `-A` flag.

    # for the RJ MCMC - the default
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -A 4
    # for the standard BD MCMC
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -A 2

The RJ MCMC uses a uniform prior on the time of rate shifts, with a default interval of 1 myr (see tutorial 3). 
Although it isn't stated, I assume if you use `-A 2` without any user specified time bins, it will just use constant rates [#Q].

To specify user timebins for birth and death use the following
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -fixShift ~/brachiopod_diversity/pyrate/epochs.txt

"This model half-Cauchy prior distribution for [the magintude of change in?] speciation and extinction rates between shifts, with a hyper-prior on the respective scale parameter to reduce the risk of over parameterization" - I don't fully understand this [#Q]. I'm also not sure how it interacts with the `-A` option [#Q].

## Fossil preservation options

### NHPP
By default PyRate uses the non-homogeneous Poisson process of preservation (NHPP) model. Preservation follows a bell curve over the lifespan of a taxon, although there is a single overall preservation rate "q". Note the RJ MCMC does not apply to fossil preservation.

### HPP
Under the homogeneous Poisson process preservation is constant through time.
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -mHPP

### TPP
The time-variable Poisson process (TPP) model allows for piecewise constant varation in peservation, with predefined user intervals.
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -qShift ~/brachiopod_diversity/pyrate/epochs.txt

The default prior on preservation rates is described as: "The default prior on the vector of preservation rates is a single gamma distribution with shape = 1.5 and rate = 1.5" - I don't know whether this means all intervals are assigned the same prior or whether a single distribution is discretized across intervals [#Q]. The parameters of the gamma distribution can be changed by adding the command `-pP 2 0.1`. Or the rate parameter can be estimated by changing the rate to 0 and adding `-pP 1.5 0` - when this setting is used a "vague exponential hyper-prior" is assigned to the rate.

## Rate heterogeneity across lineages
We can account for variation in sampling across lineages by using "a gamma model", in combination with any other preservation model, simply by adding `-mG`
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -mG

## Dealing with age uncertainty
The input `genus.py` contains replicates of fossil ages to deal with age uncertainty.
Using the `-j` flag you can indicate which replicate should be analysed. 
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -j 2

Although it works even if you specify a replicate that doesn't exist...
There might be an efficient way to parallelize this [#Q].

## Chain settings
The default chain settings are 10,000,000 iterations, sampling every 1,000 iterations. This can be changed using the `-n` and `-s` flags.
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -n 20000000 -s 5000

# Gibbs sampling 
To use Gibbs sampling simply add `-se_gibbs`. 
    python3 PyRate.py ~/brachiopod_diversity/pyrate/genus/genus.py -qShift ~/brachiopod_diversity/pyrate/epochs.txt -se_gibbs

Note this option only works with homogenous or time-variable Poisson processes (HPP or TPP). I'm not sure if it works with the `-mG` (although it does run). See tutorial 3. There are other options you can change related to this that I don't understand.
