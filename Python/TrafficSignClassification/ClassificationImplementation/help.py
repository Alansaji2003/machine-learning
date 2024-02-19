
# test program for another purpose delete after use

import matplotlib.pyplot as plt
from scipy.cluster.hierarchy import dendrogram, linkage

# Given data
data = {
    'A': [1, 1],
    'B': [1.5, 1.5],
    'C': [5, 5],
    'D': [3, 4],
    'E': [4, 4],
    'F': [3, 3.5]
}

# Convert the data to a matrix
data_matrix = [data[key] for key in data]

# Perform hierarchical clustering
Z = linkage(data_matrix, 'ward')

# Create a dendrogram
dendrogram(Z, labels=list(data.keys()))

# Display the plot
plt.title('Dendrogram for Hierarchical Clustering')
plt.xlabel('Objects')
plt.ylabel('Distance')
plt.show()