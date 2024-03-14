import numpy as np

import os
os.chdir("C:/Users/saschameyen/Desktop/NIP_Workshop2024_Meyen/NIP/Python")
os.getcwd()

from meta_measures_library import *
from fit_meta_d_MLE import *

# Example usage:
tab = np.array([[16, 17, 14, 3], [2, 11, 19, 18]])

# Evaluate meta-d' -------------------------------------------------------------
meta_fit = fit_meta_d_MLE(tab[0,], tab[1,])

print("Estimated d'                   =", meta_fit["da"])
print("Estimated meta-d'              =", meta_fit["meta_da"])
print("Estimated M-ratio = meta-d'/d' =", meta_fit["M_ratio"])

# Evaluate information-theoretic measures --------------------------------------
print("Estimated meta-I               =", estimate_meta_I(tab))
print("Estimated meta-Ir1             =", estimate_meta_Ir1(tab))
print("Estimated meta-Ir2             =", estimate_meta_Ir2(tab))

print("Estimated RMI                  =", estimate_RMI(tab))
print("Estimated Debiased RMI         =", estimate_RMI_with_bias_reduction(tab))
