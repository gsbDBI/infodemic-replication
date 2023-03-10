import glob
import os
print(os.getcwd())

from utils import *



random.seed(60637)
np.random.seed(60637)

# import data
list_of_files = glob.glob('../../data/cleaned-data_*.csv')
latest_file = max(list_of_files, key=os.path.getctime)
print(latest_file)

full_data = pd.read_csv(latest_file, quotechar='"', skipinitialspace=True)
full_data = full_data[full_data['batch'] < 5]  # keep only learning data

# configuration information
xvars = ["male", "age", "age_flag", "age_check_flag", "ed", "ed_flag", "urban", "rel_christian", "rel_muslim",
         "denom_pentecostal", "religiosity", "religiosity_flag", "locus", "locus_flag", "science",
         "science_flag", "dli", "fb_post", "fb_post_flag", "fb_msg", "fb_msg_flag", "crt", "hhi", "hhi_flag", "cash",
         "hh", "hh_flag", "pol", "cov_concern", "cov_concern_flag", "cov_efficacy", "cov_efficacy_flag", "nigeria",
         "strat_send_false0", "strat_send_false1", "strat_send_false2",
         "strat_send_true0", "strat_send_true1", "strat_send_true2",
         "strat_timeline_false0", "strat_timeline_false1", "strat_timeline_false2",
         "strat_timeline_true0", "strat_timeline_true1", "strat_timeline_true2"]

# full contexts
xs_full = full_data[xvars]

config = {
    'floor': 0.1,
    'num_mc': 1000,
    'K': 40
}

K = config['K']
pf = [0, 1]
T = xs_full.shape[0]

# save probability matrix; dimensions are: time, context, arms
probs = np.zeros((T, T, K))
# first batch probabilities are uniform
probs[full_data['batch'] == 1, :, :] = 1 / K

print('Running bandit loops')

for s in sorted(full_data['batch'].unique())[:-1]:  # loop through batches, excluding last
    print('Starting loop for batch', s)
    idx = full_data['batch'] <= s  # observations in this batch
    idnext = full_data['batch'] == s + 1  # observations in the next batch (what the model will be used for)

    # define variables
    xs_t = full_data.loc[idx, xvars]  # observed contexts up to date
    ws_t = full_data.loc[idx, 'W']  # treatments assigned up to date
    ws_t = arm_decoding_to_number(ws_t)  # recode treatments from text to numeric
    ys_t = full_data.loc[idx, 'Y']  # observed responses up to date

    probs_t = full_data.loc[idx].filter(regex='probs_*', axis=1)  # probabilities assigned in the real experiment
    balwts_t = 1 / collect(probs_t, ws_t)  # balance weights from the probabilities

    # expand data to have treatment x covariate interactions
    xs_full_t = data_transform_new(xs_t, ws_t.astype(int), K).to_numpy()
    cache = (xs_full_t, ws_t.astype(int))  # cached data is used for model

    # setup for analysis
    omegas = [12, xs_full_t.shape[1] - 12]  # number of times to repeat penalty factor
    lambda_min = ridge_glmnet(xs_full_t, np.array(ys_t.astype(float)), pf, omegas)  # store cross-validated lambda
    alpha = np.repeat([0, lambda_min], omegas)

    # generate model on each batch of data
    model = update_weighted_ridge_thompson(xs_full_t, np.array(ys_t.astype(float)), balwts_t, alpha,
                                           intercept=False)

    for c in list(range(T)):  # loop through contexts
        print('Batch', s, 'context', c, '...')
        # get probability vector for a given context under the defined model
        cc = draw_weighted_ridge_thompson(xs_full.iloc[c], model, config, cache, intercept=False)
        # store those probabilities for all of a given context within a given batch
        probs[idnext, c, :] = np.stack([cc[1]] * idnext.sum())


print('Saving probabilities')
np.save("../../data/contextual_probabilities.npy", probs)
print('Complete.')
