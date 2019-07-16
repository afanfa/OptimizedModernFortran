from scipy.io import FortranFile
from PIL import Image, ImageDraw

m = 50000
n = 50000

f = FortranFile('mandel.txt', 'r')

mat = f.read_ints(dtype=np.int32).reshape(m,n)

palette = []

im = Image.new('RGB', (m, n), (0, 0, 0))
draw = ImageDraw.Draw(im)

for x in range(0, WIDTH):
    for y in range(0, HEIGHT):
        m = mat(x,y)
        color = 255 - int(m * 255 / MAX_ITER)
        # Plot the point
        draw.point([x, y], (color, color, color))
im.save('output.png', 'PNG')
