
import numpy as np
import math
import matplotlib.pyplot as plt
import matplotlib.animation as animation

fig = plt.figure()

# generate data
x = np.random.rand(100)
y = np.random.rand(100)


ax = fig.add_subplot(1,1,1)

ax.scatter(x,y)

plt.tick_params(labelbottom='off')
plt.tick_params(labelleft='off')
fig.show()
