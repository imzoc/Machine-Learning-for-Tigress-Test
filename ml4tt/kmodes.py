''' Time to do k-means! First, import packages and prepare data for processing.
'''

from kmodes.kmodes import KModes 

kmodes = KModes(n_clusters=100, init='Huang', n_init=2, verbose=1)
clusters = kmodes.fit_predict(X)
