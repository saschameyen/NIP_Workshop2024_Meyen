import sys
from fit_meta_d_MLE import *

args = sys.argv
del args[0] # Get rid of the python script name

# Example
# args = ["16", "17", "14", "3", "2", "11", "19", "18"]
args = list(map(int, args))

n_response = int(len(args)/2)
row1 = args[0:n_response]
row2 = args[n_response:(2*n_response)]

meta_fit = fit_meta_d_MLE(row1, row2)
print(meta_fit['da'])
print(meta_fit['meta_da'])
print(meta_fit['M_ratio'])
