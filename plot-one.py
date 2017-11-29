
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

def anim():
    graphs = []
    fig = plt.figure()
    x = np.arange(0, 3, 0.1)
    y = 0.3 * x ** 2 - 2
    for i in range(80):
        plt.plot(x, y,"w")
        g = plt.plot(i, 0.3 * i ** 2 - 2,'o','r')
        graphs.append(g)
    ani = animation.ArtistAnimation(fig, graphs, interval=20)
    plt.tick_params(labelbottom='off')
    plt.tick_params(labelleft='off')
    plt.show()
    ani.save('anim.gif', writer='imagemagick')

if __name__ == '__main__':
    anim()

