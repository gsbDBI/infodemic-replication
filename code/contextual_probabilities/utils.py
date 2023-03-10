import numpy as np
import pandas as pd
import random

import rpy2
import rpy2.robjects as robj
from rpy2.robjects import numpy2ri
from rpy2.robjects.packages import importr

rpy2.robjects.numpy2ri.activate()
glmnet = importr("glmnet")

def arm_decoding_to_number(variable):
    arm_decoding_to_number = {
        'H_control_R_control' : 0,
        'H_factcheck_R_control' : 1,
        'H_more_info_R_control' : 2,
        'H_real_info_R_control' : 3,
        'H_related_R_control' : 4,
        'H_control_R_accuracy' : 5,
        'H_control_R_deliberation' : 6,
        'H_control_R_emotion' : 7,
        'H_control_R_pledge' : 8,
        'H_control_R_tips_africacheck' : 9,
        'H_control_R_tips_facebook' : 10,
        'H_control_R_video' : 11,
        'H_factcheck_R_accuracy' : 12,
        'H_factcheck_R_deliberation' : 13,
        'H_factcheck_R_emotion' : 14,
        'H_factcheck_R_pledge' : 15,
        'H_factcheck_R_tips_africacheck' : 16,
        'H_factcheck_R_tips_facebook' : 17,
        'H_factcheck_R_video' : 18,
        'H_more_info_R_accuracy' : 19,
        'H_more_info_R_deliberation' : 20,
        'H_more_info_R_emotion' : 21,
        'H_more_info_R_pledge' : 22,
        'H_more_info_R_tips_africacheck' : 23,
        'H_more_info_R_tips_facebook' : 24,
        'H_more_info_R_video' : 25,
        'H_real_info_R_accuracy' : 26,
        'H_real_info_R_deliberation' : 27,
        'H_real_info_R_emotion' : 28,
        'H_real_info_R_pledge' : 29,
        'H_real_info_R_tips_africacheck' : 30,
        'H_real_info_R_tips_facebook' : 31,
        'H_real_info_R_video' : 32,
        'H_related_R_accuracy' : 33,
        'H_related_R_deliberation' : 34,
        'H_related_R_emotion' : 35,
        'H_related_R_pledge' : 36,
        'H_related_R_tips_africacheck' : 37,
        'H_related_R_tips_facebook' : 38,
        'H_related_R_video' : 39,
    }
    return variable.replace(arm_decoding_to_number)


def data_transform_new(xs, ws, K):
    if type(xs).__module__ != np.__name__:
        xs = xs.to_numpy()

    w_matrix = np.zeros((len(ws), K))
    rows = np.arange(len(ws))
    # w_matrix.insert(0,"index",ws.index)
    # w_matrix.set_index("index",inplace=True)
    w_matrix[rows, ws] = 1
    w_matrix = pd.DataFrame(w_matrix).add_prefix("w").astype(int)
    w_matrix.drop('w0', axis=1, inplace=True)

    xs = pd.DataFrame(xs).astype(float)
    xs = xs.add_prefix("x")
    interact_matrix = pd.DataFrame()
    for w in range(1, K):
        sub = xs.mul(w_matrix.iloc[:, w - 1], axis=0)
        sub = sub.add_prefix("w" + str(w) + ":")
        interact_matrix = pd.concat([interact_matrix, sub], axis=1)

    for w in range(12, K):  # 12 equals number of headline treatments + number of respondent treatments + 1 (control)
        idx = ws == w
        try:
            idx.reset_index(drop=True, inplace=True)
        except:
            pass
        if np.any(idx):
            headline = (w - 12) // 7 + 1
            respondent = ((w - 11) % 7 if (w - 11) % 7 != 0 else 7)
            # print(f"headline ={headline}, respondent = {respondent}")# to check, print this row
            w_matrix.loc[idx, f"w{headline}"] = 1
            w_matrix.loc[idx, f"w{respondent + 4}"] = 1

    model_matrix = pd.concat([w_matrix, xs, interact_matrix], axis=1)
    return model_matrix

def draw_weighted_ridge_thompson(xt, model, config, cache, intercept=True):
    """
     model should contain theta and sigma2 from the update_weighted_ridge_thompson function
     cache should contain past history
     """

    """ Config and initialization """
    K, num_mc, floor = config["K"], config["num_mc"], config["floor"]
    floor = floor / K

    xw_full = data_transform_new(np.tile(xt, (K, 1)), np.arange(K), K).to_numpy()

    # historical data
    xh_full, ws = cache

    # standardize based on historical data
    xw_full = np.array(xw_full)
    xw_full = (xw_full - xh_full.mean(0))
    xw_full = xw_full / (xh_full.std(0) + 1e-6)

    if intercept:
        xw_full = np.hstack([np.ones((len(xw_full), 1)), xw_full])

    theta, sigma2 = model
    draws = np.empty((K, num_mc))

    """ Monte Carlo simulation """
    coeff = np.random.multivariate_normal(theta, sigma2, size=num_mc, tol=1e-6)
    for w in range(K):
        draws[w] = coeff @ xw_full[w]
    # calculate proportion of max and draw according to the probability
    ps = np.bincount(np.argmax(draws, axis=0), minlength=K) / num_mc
    ps = apply_floor(ps, floor)
    w = np.random.choice(K, p=ps)
    return w, ps


def ridge_glmnet(X_full, Y, pf, omegas):
    Y = Y.reshape(-1, 1)
    pfac = np.repeat(pf, omegas)
    T = X_full.shape[0]
    k = 10
    foldid = gen_folds(k, T)
    modelcv = glmnet.cv_glmnet(X_full, Y, alpha=0, intercept=False, foldid=foldid, **{"penalty.factor": pfac})
    # mse_min = modelcv.rx2('cvm')[np.where(np.round(modelcv.rx2('lambda'), 4) == round(modelcv.rx2('lambda.min')[0], 4))[0][0]]
    return modelcv.rx2('lambda.min')[0]

def collect(a, indices):
    if type(a) == np.ndarray:
        a = pd.DataFrame(a)
    assert len(a) == len(indices)
    if len(indices) == 1:
        out = [a.iloc[0, int(indices[0])]]
    else:
        out = np.column_stack([a.iloc[n, int(i)] for n, i in zip(np.arange(len(indices)), indices)])
    return out[0]

def update_weighted_ridge_thompson(xs_full, yobs, balwts, alpha, intercept=True):
    T, p = xs_full.shape
    W = np.diag(balwts)

    xs_fulls, yts = standardization(xs_full), standardization(yobs)
    if intercept:
        p += 1
        alpha = np.concatenate(([0], alpha))
        xs_fulls = np.hstack([np.ones((len(xs_fulls), 1)), xs_fulls])
    A = np.dot(np.transpose(xs_fulls) @ W, xs_fulls) + alpha * np.identity(p)
    Ainv = np.linalg.inv(A)
    theta = Ainv @ xs_fulls.T @ W @ yts
    sigma2 = Ainv * ((yts - xs_fulls @ theta).T @ W @ (yts - xs_fulls @ theta))
    return theta, sigma2


def standardization(X, center=True, scale=True):
    X = np.array(X)
    if center: X = (X - X.mean(0))
    if scale: X = X / (X.std(0) + 1e-6)
    return X

def apply_floor(a, amin):
    new = np.maximum(a, amin)
    total_slack = np.sum(new) - 1
    individual_slack = new - amin
    c = total_slack / np.sum(individual_slack)
    return new - c * individual_slack

def gen_folds(k, T):
    s = list(range(1, k+1))
    s = s * (T // k) + s[:(T % k)]
    np.random.shuffle(s)
    s = np.array(s)
    return s