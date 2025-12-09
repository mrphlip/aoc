#!/usr/bin/python
from aocimports import *

dat = [
	(97978,50277),
	(97978,51488),
	[...]
]

x1a, y1a, x2a, y2a = [...my actual part 1 answer...]
x1b, y1b, x2b, y2b = [...my actual part 2 answer...]

#dat = [(7,1),(11,1),(11,7),(9,7),(9,5),(2,5),(2,3),(7,3),]; dat = [*dat[1:], dat[0]]; x1a, y1a, x2a, y2a = 11, 7, 2, 3; x1b, y1b, x2b, y2b = 9, 7, 2, 5

XMIN = min(x for x, y in dat)
XMAX = max(x for x, y in dat)
YMIN = min(y for x, y in dat)
YMAX = max(y for x, y in dat)
XBORDER = (XMAX - XMIN) / 10
YBORDER = (YMAX - YMIN) / 10
XMIN -= XBORDER
XMAX += XBORDER
YMIN -= YBORDER
YMAX += YBORDER

CX = 1000
CY = 1000

from PIL import Image, ImageDraw, ImageColor
from contextlib import contextmanager

with Image.new("RGBA", (CX, CY), "white") as img:
	def loc(x, y):
		return (x-XMIN)/(XMAX-XMIN)*CX, (y-YMIN)/(YMAX-YMIN)*CY
	def poly(draw, points, col):
		p = [loc(x,y) for x,y in points]
		col = ImageColor.getrgb(col)
		colbg = (*col, 64)
		draw.polygon(p, colbg, col, 2)
	def dot(draw, x, y, r, col):
		x, y = loc(x, y)
		draw.ellipse([x-r,y-r,x+r,y+r], col)
	def rect(draw, x1, y1, x2, y2, col):
		poly(draw, [(x1, y1), (x1, y2), (x2, y2), (x2, y1)], col)
		dot(draw, x1, y1, 5, col)
		dot(draw, x2, y2, 5, col)

	@contextmanager
	def subimg():
		with Image.new("RGBA", (CX, CY), (0,0,0,0)) as img2:
			yield ImageDraw.Draw(img2, "RGBA")
			img.paste(img2.convert("RGB"), mask=img2)

	with subimg() as draw:
		poly(draw, dat, "blue")
		for x, y in dat:
			dot(draw, x, y, 3, "blue")
	with subimg() as draw:
		rect(draw, x1a, y1a, x2a, y2a, "red")
	with subimg() as draw:
		rect(draw, x1b, y1b, x2b, y2b, "green")

	img.show()
