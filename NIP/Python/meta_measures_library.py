import numpy as np
import pandas as pd
from scipy.stats import norm, multinomial




# Dayan's metainformation measures ---------------------------------------------

def estimate_meta_I(contingency_table):
    '''
    Estimate Dayan (2023)'s meta-I from a contingency table. 
    We compute it as the information minus the minimal information, 
    
        meta-I = I(Y; \hat{Y}, C) - I(Y; \hat{Y}), 
    
    which is equivalent to Dayan's formulation 
    
        meta-I = I(Y = \hat{Y}; C).
       
    @param contingency_table Matrix with frequency of observations, rows = true
                             label and cols = responses
 
    @return Meta-I value
    '''
    contingency_table = contingency_table / np.sum(contingency_table)
    p = np.sum(contingency_table, axis=1)  # Prior

    info = get_information(contingency_table)
    a = get_accuracy(contingency_table)
    info_lower = get_lower_info_for_one(p, a)

    meta_I = info - info_lower

    return meta_I


def get_entropy(p, base=2):
    '''
    @param p Probability vector
    @param base Base for the logarithm. If base = 2, the entropy is measured 
                in bit. If base = e, the entropy is measured in nat. 
                Default is base = 2.
                
    @return Entropy H(p)
    '''
    assert np.round(np.sum(p) - 1, 6) == 0, "Sum of probabilities should be 1."

    # Remove zero probabilities
    # By convention, 0*log(1/0) = 0
    # because lim_{p->0} p*log(1/p) = 0
    p = np.array(p)
    p = p[p != 0]
    
    products = p * np.log(1/p) / np.log(base)
    
    entropy = np.sum(products)
    return entropy

H = get_entropy


def get_binary_entropy(p):
    '''
    @param p Probability of one realiziation. Which of the two realiziations 
             of a binary variable doesn't matter because of symmetry, 
             H2(p) = H2(1-p).

    @return Entropy of the binary random variable
    '''
    if p.ndim == 0:
        p = np.expand_dims(p, axis=0)
    binary_entropy = np.apply_along_axis(lambda x: get_entropy([x, 1 - x]), axis=0, arr=p)
        
    return binary_entropy

H2 = get_binary_entropy


def pmi(c, prior):
    '''
    Pointwise Mutual Information (PMI)
    
    Transmitted information of a random variable and response with 
    confidence vector c. Transmitted information and mutual information 
    is the same thing, there are just different conventional names.
    
    @param c Confidence vector
    @param p Base rate
    
    @return Mutual Information between binary random variable and prediction
    '''
    return H(prior) - H(c)

get_pointwise_mutual_information = pmi

def pmi2(c, p = 0.5): 
    '''
    Binary Pointwise Mutual Information
    
    Mutual information of a binary random variable (typically with 
    equal base rates, p = 0.5) and a prediction with confidence x
    
    @param c Confidence in the predicted label
    @param p Base rate
    
    @return Mutual Information between binary random variable and prediction
    '''
    return H2(p) - H2(c) 

pointwise_mutual_information = pmi2


def construct_line(x1, y1, x2, y2):
    '''
    Linear Function Through Given Points
    
    We construct a function corresponding to the line that runs 
    through (x1, y1) and (x2, y2). 
    
    @param x1 X-coordinate (accuracy) of the first point
    @param y1 Y-coordinate (information) of the first point 
    @param x2 X-coordinate (accuracy) of the second point
    @param y2 Y-coordinate (information) of the second point
    
    @return A linear function that takes c (confidence) as input and 
            returns linear increasing y (information) values.
    '''
    return lambda c: y1 + (c - x1) / (x2 - x1) * (y2 - y1)


def get_intersection(f1, f2, range):
    '''
    Compute Intersection Recursively
    
    We compute the intersection between two (potentially nonlinear) functions,
    f1 and f2, within the given range. It is assumed that difference of
    functions is monotone and the functions do intersect.
    
    @param f1 Function 1.
    @param f2 Function 2.
    @param range Vector of two numbers indicating the interval 
                 in which the intersection is to be computed
    @param n_steps Number of recursive iterations for the computation of the
                   intersection
                   
    @return A vector of three values: (1) the x-coordinate of the intersection, 
            (2) the y-coordinate evaluated with f1, and (3) the y-coordinate 
            evaluated with f2. Outputs (2) and (3) are to verify the quality of 
            the returned intersection point. They should be equal or at east very close.
    '''
    def get_recursive_intersection(f1, f2, range, n_steps):
        # When no more recursive steps are to be done, 
        # return the center of the remaining range
        if n_steps == 0:
            return np.mean(range)

        # Take a small sample of the steps and compute the match
        support = np.linspace(range[0], range[1], num=21)
        best_match_index = np.argmin((f1(support) - f2(support))**2)

        # Set the new range for the next recursion step to be
        # around the closest match
        l_index = max(1, best_match_index - 1)
        u_index = min(len(support), best_match_index + 1)
        l = support[l_index]
        u = support[u_index]
        new_range = [l, u]

        # Repeat
        return get_recursive_intersection(f1, f2, new_range, n_steps=n_steps - 1)

    n_steps = 7
    x = get_recursive_intersection(f1, f2, range, n_steps)

    # Return intersection point and both functions' values
    result = [x, f1(x), f2(x)]
    return result


def pmi2_inverse(info, p=0.5):
    '''
    Inverse Binary Pointwise Mutual Information
    
    Compute the confidence that corresponds to the input transmitted
    information.
    
    @param info Transmitted binary poitnwise information
    @param p Base rate
    
    @return Confidence corresponding to the Mutual Information
    '''
    
    line = construct_line(0.5, info, 1, info)
    accuracy = get_intersection(pmi2, line, [0.5, 1])[0]
    return accuracy

get_accuracy_from_information = pmi2_inverse

def get_accuracy(classifier):
    '''
    Compute accuracy of a classifier
    The accuracy is the sum of highest entries for each response in the
    classifier contingency matrix. 
    
    @param classifier Contingency table with labels as rows and
                      responses as columns
    
    @return Accuracy of the classifier
    '''
    accuracy = np.sum(np.apply_along_axis(np.max, 0, classifier))
    return accuracy


def get_information(classifier):
    '''
    Compute transmitted information of a classifier
    
    @param classifier Contingency table with labels as rows and 
                      responses as columns
    
    @return Transmitted information of the classifier's output about the label
    '''
    assert np.round(np.sum(classifier) - 1, 6) == 0, "Sum of classifier should be 1."

    info = H(np.sum(classifier, axis=0)) + H(np.sum(classifier, axis=1)) - H(classifier)
    return info



def estimate_meta_Ir1(contingency_table):
    '''
    Estimate Dayan (2023)'s meta-Ir1 from a contingency table. We compute
    meta-I as usual and compute the meta-I for normal noise by getting the
    information of a normal noise classifier with the same accuracy and
    subtracting the minimal information, which yields meta-I(d') as the
    normalizer,
    
        meta-Ir1 = meta-I / meta-I(d').
    
    @param contingency_table Matrix with frequency of observations, rows = true
                             label and cols = responses
                             
    @return Meta-Ir1 value
    '''
    contingency_table = contingency_table / np.sum(contingency_table)

    meta_I = estimate_meta_I(contingency_table)

    p = np.sum(contingency_table, axis=1)
    a = get_accuracy(contingency_table)
    info = get_information(contingency_table)
    info_lower = get_lower_info_for_one(p, a)

    info_normal = get_normal_noise_information(accuracies=a)['info'].values
    meta_I_normal = info_normal - info_lower

    meta_I_r1 = meta_I / meta_I_normal

    return meta_I_r1[0]



def get_normal_noise_information(accuracies=None, sensitivities=None):
    '''
    Determine the transmitted information of a normal noise classifier. 
    Either accuracies or sensitivities have to be specified to determine the
    corresponding transmitted information. If both are specified(not advised),
    accuracies are prioritized.
    
    @param accuracies Vector of accuracies
    @param sensitivities Vector of sensitivities
 
    @return Transmitted information of a normal noise classifier
    '''
    if sensitivities is None:
        sensitivities = transform_normal_accuracy_to_sensitivity(accuracies)

    if accuracies is None:
        accuracies = transform_normal_sensitivity_to_accuracy(sensitivities)

    normal_noise_information = pd.DataFrame()
    it_sensitivities = np.nditer(sensitivities)
    it_accuracies = np.nditer(accuracies)
    
    for sensitivity, accuracy in zip(it_sensitivities, it_accuracies):
        classifier = get_normal_noise_classifier(sensitivity=sensitivity)
        info = get_information(classifier)
        
        row = pd.DataFrame({
            'accuracy': [accuracy],
            'sensitivity': [sensitivity],
            'info': [info]
        })
        
        normal_noise_information = pd.concat([normal_noise_information, row], 
                                             axis=0, ignore_index=True)

    return normal_noise_information


def get_normal_noise_classifier(sensitivity=None, accuracy=None):
    '''
    Define a classifier with underlying normal distributions on observations.
    The normal distribution is of equal variance (= 1) and 
    the means are shifted by the sensitivity.
     
    Either accuracy or sensitivity has to be specified to determine the 
    classifier. If both are specified (not advised), accuracies are
    prioritized.
     
    @param sensitivity Shift of the normal distributions
    @param accuracy Resulting classifier accuracy
     
    @return Normal noise classifier
    '''
    if sensitivity is None:
        sensitivity = transform_normal_accuracy_to_sensitivity(accuracy)

    if accuracy is None:
        accuracy = transform_normal_sensitivity_to_accuracy(sensitivity)

    prior = np.array([0.5, 0.5])

    if sensitivity == 0:
        return np.asmatrix(prior)

    if np.isinf(sensitivity):
        return np.diag(prior)

    x = np.linspace(-7 - sensitivity, 7 + sensitivity, num=5000)
    delta = x[1] - x[0]

    f1 = norm.pdf(x, loc=-sensitivity/2) * delta * 1/2
    f2 = norm.pdf(x, loc=+sensitivity/2) * delta * 1/2

    classifier = np.vstack((f1, f2))
    return classifier

def transform_normal_accuracy_to_sensitivity(accuracy):
    sensitivity = 2 * norm.ppf(accuracy)
    return sensitivity

def transform_normal_sensitivity_to_accuracy(sensitivity):
    accuracy = norm.cdf(sensitivity / 2)
    return accuracy



def estimate_meta_Ir2(contingency_table):
    '''
    Estimate Dayan (2023)'s meta-Ir1 from a contingency table. We compute 
    meta-I as usual and compute H2(accuracy) as the normalizer,
    
        meta-Ir1 = meta-I / H(Y = \hat{Y}). 
    
    @param contingency_table Matrix with frequency of observations, rows = true
                             label and cols = responses
                             
    @return Meta-Ir2 value
    '''
    contingency_table = contingency_table / np.sum(contingency_table)

    meta_I = estimate_meta_I(contingency_table)

    a = get_accuracy(contingency_table)
    H_accuracy = H2(a)

    meta_I_r2 = meta_I / H_accuracy

    return meta_I_r2


# Meyen's metainformation measure ----------------------------------------------

def estimate_RMI(contingency_table):
    ''' 
    Compute the meta-information as a relative measure in the possible range
    given the accuracy: Highest information for a given accuracy produces 
    RMI = 1, lowest information for a given accuracy produces RMI = 0.
    
    @param contingency_table Matrix with frequency of observations, rows = true
                             label and cols = responses
    
    @return Estimated RMI value using naive plugin-estimation
    '''
    contingency_table = contingency_table / np.sum(contingency_table)
    prior = np.sum(contingency_table, axis=1)
    accuracy = get_accuracy(contingency_table)  
    information = get_information(contingency_table)  

    # Get bounds
    information_bounds = get_analytic_information_bounds(prior, accuracy)

    lower_bound = information_bounds['lowest'].values
    upper_bound = information_bounds['highest'].values

    # Normalize by bounds
    RMI = (information - lower_bound) / (upper_bound - lower_bound)

    # Where bounds collapse because of accuracy edge cases, return NaN
    RMI[lower_bound == upper_bound] = np.nan

    return RMI[0]

def get_analytic_binary_information_bounds(prior, accuracies):
    '''
    Given the accuracies, compute the highest and lowest transmitted
    information of classifiers.
     
    @param accuracy Accuracy of classifiers
     
    @return Data frame with highest and lowest possible transmitted information
    '''
    H_prior = get_entropy(prior)  

    information_bounds = pd.DataFrame({
        'accuracies': accuracies,
        'highest': H_prior - 2 * (1-accuracies),
        'lowest': H_prior - H2(accuracies)  
    })

    return information_bounds

def get_analytic_information_bounds(prior, accuracies):
    '''
    Given the accuracies, compute the highest and lowest transmitted
    information of classifiers. Information is given in bit (log base is 2).
    
    @param accuracy Accuracy of classifiers
    
    @return Data frame with highest and lowest possible transmitted information
    '''
    upper = np.vectorize(lambda a: get_upper_info_for_one(prior, a))(accuracies)
    lower = np.vectorize(lambda a: get_lower_info_for_one(prior, a))(accuracies)

    data = {'accuracies': accuracies,
            'highest': upper,
            'lowest': lower}
    information_bounds = pd.DataFrame(data, index=range(np.size(accuracies)))

    return information_bounds

def get_upper_info_for_one(prior, accuracy):
    m1 = int(np.floor(1 / accuracy))
    m2 = m1 + 1

    HY = get_entropy(prior) 
    HYC = ((1/m1 - accuracy) * np.log2(m2) + 
           (accuracy - 1/m2) * np.log2(m1)) / (1/m1 - 1/m2)

    info = HY - HYC
    return info

def get_lower_info_for_one(prior, accuracy):
    a = accuracy
    p = np.sort(prior)[::-1]  # Sort in descending order

    if a == 1:
        if np.sum(p == 0) > 0:
            info = np.nan
        else:
            info = np.sum(p * np.log2(1/p))
        return info

    if a == np.max(p):
        info = 0
        return info
    
    ref = (np.cumsum(p) - a)[1:] / (np.arange(1, len(p) + 1) - 1)[1:]
    ref = np.insert(ref, 0, float('-inf'))
    s = p >= ref
    m3 = np.where(s)[0][-1] + 1
    q = np.sum(p[:m3])
    pl = p[:m3]

    HY = np.sum(pl * np.log2(1/pl))
    HYC = a * np.log2(1/a) + (q - a) * np.log2((m3 - 1) / (q - a))

    info = HY - HYC
    return info

def estimate_RMI_with_bias_reduction(sample_tab):
    ''' 
    Remove bias from RMI estimate by simulating RMI values from the observed
    data by simulating data sets from the estimated contingency table and
    subtracting the simulated mean RMI from the RMI of the given table. This
    is not perfect but the better the actual contingency table is estimated,
    the more accurately the bias is removed.
    
    @param sample_tab Matrix with number of observations, rows = true 
                      label and cols = responses
    
    @return Debiased (not unbiased) estimate of RMI
    '''
    n = np.unique(np.sum(sample_tab, axis=1))[0]

    # Estimate bias
    estimated_RMI = estimate_RMI(sample_tab)
    expected_RMI = get_expected_RMI(sample_tab, n)
    estimated_bias = expected_RMI - estimated_RMI

    # Subtract estimated bias
    estimated_RMI_debiased = estimated_RMI - estimated_bias
    return estimated_RMI_debiased

def get_expected_RMI(classifier, n):
    ''' 
    Simulate classifier by row-wise sampling n observations from its joint
    probability distribution. For each simulation, compute the mean RMI.
    Repeat simulations until the mean RMI is sufficiently precisely determined.
    
    @param classifier Classifier as joint probability distribution matrix
    @param n Number of trials per label (row)
    
    @return Simulated estimate for the mean RMI to be expected from sampling
            the classifier.
    '''
    nsim = 1000
    se = float('inf')
    se_crit = 0.001  # Desired standard error from simulation noise
    precision = 0

    simulated_RMIs = []
    
    while se > se_crit:
        # Simulate classifier multiple times
        for i in range(int(nsim + precision * nsim)):
            # Simulate one classifier row-wise
            counts = np.zeros_like(classifier)
            for j in range(classifier.shape[0]):
                counts[j, :] = multinomial.rvs(n, classifier[j, :] / np.sum(classifier[j, :]))

            simulated_classifier = counts / np.sum(counts)

            simulated_RMIs = np.append(simulated_RMIs, estimate_RMI(simulated_classifier))

        simulated_RMIs = simulated_RMIs[~np.isnan(simulated_RMIs)]

        # Calculate standard error and, if too low, repeat simulation
        se = np.std(simulated_RMIs) / np.sqrt(len(simulated_RMIs))
        precision += 1

    expected_RMI = np.mean(simulated_RMIs)
    return expected_RMI

