#!/usr/bin/python
dat = [
	[(2 ,"blue", 3 ,"red"),( 3 ,"green", 3 ,"blue", 6 ,"red"),( 4 ,"blue", 6 ,"red"),( 2 ,"green", 2 ,"blue", 9 ,"red"),( 2 ,"red", 4 ,"blue")],
	[(4 ,"red", 1 ,"green"),( 3 ,"red"),( 13 ,"green", 5 ,"red", 3 ,"blue"),( 3 ,"green", 2 ,"red"),( 3 ,"blue", 5 ,"red", 3 ,"green"),( 2 ,"red", 3 ,"blue", 12 ,"green")],
	[(4 ,"red", 1 ,"green", 1 ,"blue"),( 1 ,"red", 1 ,"blue"),( 6 ,"red", 1 ,"green"),( 6 ,"red", 3 ,"blue", 1 ,"green"),( 4 ,"red")],
	[(4 ,"blue", 12 ,"red", 4 ,"green"),( 6 ,"green", 3 ,"blue", 19 ,"red"),( 3 ,"blue", 2 ,"red", 2 ,"green")],
	[(1 ,"red", 5 ,"blue", 16 ,"green"),( 1 ,"red", 6 ,"green", 3 ,"blue"),( 2 ,"red", 12 ,"blue"),( 17 ,"blue", 3 ,"green"),( 7 ,"green", 2 ,"red", 6 ,"blue")],
	[(3 ,"green", 1 ,"blue", 5 ,"red"),( 5 ,"green", 5 ,"red"),( 2 ,"green", 2 ,"blue", 3 ,"red"),( 5 ,"green", 2 ,"red"),( 3 ,"green", 6 ,"red", 3 ,"blue"),( 5 ,"green", 4 ,"red")],
	[(15 ,"blue", 1 ,"red", 6 ,"green"),( 4 ,"blue", 7 ,"green", 2 ,"red"),( 14 ,"blue", 5 ,"green", 2 ,"red")],
	[(6 ,"blue", 3 ,"green", 10 ,"red"),( 2 ,"blue", 1 ,"green", 5 ,"red"),( 6 ,"blue", 3 ,"green", 12 ,"red"),( 11 ,"red", 1 ,"green", 1 ,"blue"),( 5 ,"blue", 14 ,"red", 3 ,"green"),( 3 ,"red")],
	[(15 ,"red", 3 ,"blue"),( 1 ,"blue", 16 ,"red"),( 1 ,"red", 3 ,"blue"),( 1 ,"blue", 1 ,"green", 9 ,"red")],
	[(1 ,"red"),( 1 ,"blue", 7 ,"green"),( 1 ,"green", 5 ,"blue"),( 3 ,"blue", 3 ,"green"),( 1 ,"green")],
	[(19 ,"blue", 13 ,"green"),( 19 ,"blue", 2 ,"green"),( 10 ,"blue", 3 ,"red", 12 ,"green"),( 11 ,"blue", 1 ,"red", 6 ,"green")],
	[(7 ,"green", 5 ,"blue"),( 6 ,"green", 3 ,"red", 6 ,"blue"),( 2 ,"red", 5 ,"blue", 15 ,"green"),( 2 ,"red", 1 ,"blue", 1 ,"green"),( 4 ,"red", 4 ,"green", 2 ,"blue"),( 3 ,"blue", 6 ,"green")],
	[(9 ,"red", 2 ,"blue", 2 ,"green"),( 1 ,"blue", 2 ,"red", 15 ,"green"),( 9 ,"green", 2 ,"blue", 9 ,"red"),( 5 ,"blue", 8 ,"green", 5 ,"red"),( 2 ,"blue", 11 ,"green", 5 ,"red")],
	[(9 ,"blue", 1 ,"red"),( 10 ,"blue", 4 ,"green", 3 ,"red"),( 2 ,"red", 6 ,"blue"),( 4 ,"green", 2 ,"blue", 1 ,"red"),( 5 ,"green", 2 ,"red", 11 ,"blue"),( 12 ,"blue", 2 ,"red", 1 ,"green")],
	[(9 ,"blue", 7 ,"green", 12 ,"red"),( 9 ,"red", 17 ,"green", 8 ,"blue"),( 6 ,"red", 4 ,"blue", 4 ,"green"),( 5 ,"red", 17 ,"green")],
	[(5 ,"green", 4 ,"red"),( 3 ,"blue", 3 ,"red", 14 ,"green"),( 6 ,"red", 5 ,"blue", 12 ,"green")],
	[(8 ,"blue", 5 ,"green", 2 ,"red"),( 6 ,"red", 6 ,"blue"),( 9 ,"red"),( 5 ,"blue", 2 ,"green", 8 ,"red"),( 13 ,"red", 4 ,"blue", 4 ,"green"),( 9 ,"blue", 3 ,"green", 5 ,"red")],
	[(8 ,"green", 1 ,"red", 2 ,"blue"),( 4 ,"green", 4 ,"red", 1 ,"blue"),( 6 ,"blue", 2 ,"red")],
	[(3 ,"green", 9 ,"blue"),( 4 ,"blue", 10 ,"red"),( 6 ,"red", 3 ,"green", 3 ,"blue"),( 6 ,"red", 4 ,"green", 9 ,"blue")],
	[(11 ,"green", 3 ,"blue"),( 6 ,"green"),( 3 ,"green", 6 ,"blue"),( 1 ,"red", 5 ,"green"),( 6 ,"blue", 7 ,"green")],
	[(1 ,"green", 1 ,"blue", 12 ,"red"),( 6 ,"red", 2 ,"blue"),( 5 ,"green", 4 ,"red", 2 ,"blue"),( 11 ,"red", 8 ,"green", 1 ,"blue")],
	[(10 ,"red"),( 1 ,"red", 13 ,"green", 9 ,"blue"),( 6 ,"blue", 12 ,"red", 12 ,"green"),( 10 ,"red", 8 ,"blue", 11 ,"green"),( 2 ,"green", 1 ,"red", 3 ,"blue"),( 7 ,"red", 1 ,"blue", 8 ,"green")],
	[(11 ,"red", 15 ,"blue"),( 10 ,"blue", 16 ,"red", 1 ,"green"),( 14 ,"blue", 5 ,"red"),( 1 ,"green", 9 ,"red", 9 ,"blue"),( 1 ,"red", 7 ,"blue", 3 ,"green"),( 6 ,"red", 2 ,"green", 3 ,"blue")],
	[(6 ,"blue", 11 ,"red"),( 16 ,"green", 2 ,"red", 1 ,"blue"),( 8 ,"red", 7 ,"blue"),( 14 ,"blue", 9 ,"green", 9 ,"red"),( 13 ,"green", 4 ,"red", 8 ,"blue"),( 2 ,"red", 7 ,"blue", 1 ,"green")],
	[(2 ,"green", 12 ,"blue", 1 ,"red"),( 10 ,"blue", 5 ,"red", 5 ,"green"),( 2 ,"blue", 9 ,"red", 3 ,"green"),( 5 ,"blue", 4 ,"red", 2 ,"green")],
	[(7 ,"blue", 6 ,"red", 1 ,"green"),( 2 ,"blue", 3 ,"green", 12 ,"red"),( 2 ,"blue", 6 ,"red", 5 ,"green")],
	[(2 ,"green", 3 ,"red"),( 4 ,"green"),( 2 ,"red", 1 ,"blue", 1 ,"green"),( 2 ,"red", 1 ,"green", 2 ,"blue")],
	[(11 ,"blue", 1 ,"red", 5 ,"green"),( 2 ,"blue", 2 ,"red", 4 ,"green"),( 10 ,"blue", 4 ,"red", 1 ,"green")],
	[(6 ,"blue", 17 ,"red", 1 ,"green"),( 8 ,"blue", 4 ,"red"),( 14 ,"blue", 1 ,"red", 3 ,"green")],
	[(2 ,"blue", 4 ,"green"),( 7 ,"green", 1 ,"blue", 1 ,"red"),( 1 ,"blue", 8 ,"green")],
	[(15 ,"blue", 9 ,"green", 2 ,"red"),( 5 ,"green", 4 ,"blue", 1 ,"red"),( 1 ,"green", 15 ,"red", 7 ,"blue"),( 5 ,"red", 2 ,"blue")],
	[(1 ,"blue", 5 ,"red", 3 ,"green"),( 3 ,"green", 8 ,"red", 1 ,"blue"),( 5 ,"green", 1 ,"red"),( 4 ,"green", 3 ,"blue", 15 ,"red"),( 2 ,"green", 1 ,"blue"),( 4 ,"blue", 15 ,"red", 4 ,"green")],
	[(3 ,"red", 10 ,"blue"),( 4 ,"red", 9 ,"blue"),( 1 ,"green", 10 ,"blue")],
	[(3 ,"blue", 1 ,"green", 9 ,"red"),( 4 ,"green", 2 ,"red", 9 ,"blue"),( 7 ,"blue", 3 ,"red"),( 6 ,"blue", 13 ,"red"),( 4 ,"green", 13 ,"blue", 9 ,"red")],
	[(14 ,"red", 1 ,"green"),( 1 ,"red", 2 ,"green", 4 ,"blue"),( 3 ,"blue", 10 ,"red", 6 ,"green"),( 5 ,"blue", 6 ,"red", 7 ,"green"),( 7 ,"blue", 5 ,"red")],
	[(2 ,"blue", 8 ,"red", 9 ,"green"),( 9 ,"green", 3 ,"red", 10 ,"blue"),( 6 ,"red", 8 ,"blue", 1 ,"green"),( 6 ,"green", 8 ,"red", 4 ,"blue")],
	[(10 ,"green", 3 ,"red", 6 ,"blue"),( 2 ,"blue", 9 ,"red", 5 ,"green"),( 13 ,"green", 9 ,"red", 10 ,"blue"),( 2 ,"blue", 4 ,"green", 9 ,"red")],
	[(4 ,"red", 14 ,"blue", 12 ,"green"),( 6 ,"red", 12 ,"green", 18 ,"blue"),( 6 ,"green", 1 ,"blue", 1 ,"red")],
	[(5 ,"red", 1 ,"blue", 3 ,"green"),( 1 ,"blue", 3 ,"green", 8 ,"red"),( 15 ,"red", 1 ,"blue", 5 ,"green"),( 3 ,"green", 5 ,"red"),( 1 ,"blue", 14 ,"red"),( 3 ,"green", 1 ,"blue", 12 ,"red")],
	[(8 ,"green", 4 ,"blue"),( 5 ,"blue", 7 ,"red", 8 ,"green"),( 5 ,"blue", 8 ,"green"),( 6 ,"green", 3 ,"red", 12 ,"blue"),( 14 ,"blue", 7 ,"green", 2 ,"red"),( 1 ,"green", 7 ,"red", 5 ,"blue")],
	[(7 ,"red", 10 ,"green"),( 10 ,"red", 6 ,"green"),( 9 ,"red", 7 ,"green", 1 ,"blue"),( 3 ,"red", 1 ,"blue")],
	[(3 ,"green", 2 ,"blue", 13 ,"red"),( 1 ,"blue", 3 ,"red"),( 11 ,"green", 16 ,"red"),( 3 ,"green", 1 ,"blue", 16 ,"red"),( 5 ,"red", 8 ,"green")],
	[(12 ,"blue", 9 ,"red"),( 16 ,"blue", 2 ,"red", 7 ,"green"),( 4 ,"red", 1 ,"blue", 11 ,"green"),( 15 ,"blue", 4 ,"red", 9 ,"green")],
	[(17 ,"green", 5 ,"blue", 2 ,"red"),( 9 ,"green", 11 ,"blue", 1 ,"red"),( 20 ,"green", 3 ,"blue", 8 ,"red"),( 2 ,"red", 13 ,"green", 9 ,"blue"),( 15 ,"green", 12 ,"blue"),( 4 ,"blue", 7 ,"green", 9 ,"red")],
	[(5 ,"green"),( 5 ,"green", 1 ,"red"),( 3 ,"green", 2 ,"blue"),( 1 ,"green", 1 ,"blue", 1 ,"red")],
	[(10 ,"red", 11 ,"green"),( 16 ,"green", 8 ,"blue", 12 ,"red"),( 9 ,"green", 9 ,"blue")],
	[(20 ,"green", 17 ,"red", 1 ,"blue"),( 16 ,"red", 2 ,"blue", 11 ,"green"),( 3 ,"blue", 19 ,"red", 1 ,"green"),( 3 ,"blue", 17 ,"red", 17 ,"green"),( 12 ,"green", 2 ,"blue", 7 ,"red")],
	[(1 ,"red", 4 ,"blue", 6 ,"green"),( 19 ,"green", 1 ,"red", 1 ,"blue"),( 16 ,"green", 3 ,"blue", 1 ,"red"),( 3 ,"blue", 17 ,"green"),( 4 ,"blue", 12 ,"green")],
	[(13 ,"green", 2 ,"blue", 1 ,"red"),( 1 ,"green", 8 ,"red", 2 ,"blue"),( 11 ,"red", 11 ,"green", 3 ,"blue"),( 7 ,"red", 8 ,"green", 4 ,"blue")],
	[(11 ,"blue", 1 ,"red", 2 ,"green"),( 1 ,"green", 10 ,"blue"),( 1 ,"blue"),( 6 ,"blue"),( 1 ,"green", 2 ,"blue")],
	[(3 ,"red", 3 ,"green", 1 ,"blue"),( 3 ,"green", 3 ,"red"),( 10 ,"green", 4 ,"red"),( 3 ,"red", 2 ,"green")],
	[(1 ,"red", 4 ,"blue"),( 1 ,"green", 11 ,"blue"),( 1 ,"green", 3 ,"red", 6 ,"blue"),( 4 ,"red", 1 ,"green", 4 ,"blue"),( 9 ,"blue", 1 ,"green"),( 10 ,"blue", 1 ,"green")],
	[(2 ,"blue", 4 ,"green", 1 ,"red"),( 8 ,"blue", 4 ,"red", 7 ,"green"),( 9 ,"red", 7 ,"blue", 6 ,"green"),( 3 ,"red", 7 ,"green", 1 ,"blue"),( 2 ,"red", 9 ,"blue", 5 ,"green"),( 1 ,"green", 7 ,"red", 10 ,"blue")],
	[(1 ,"red", 1 ,"blue", 5 ,"green"),( 2 ,"red", 1 ,"green", 2 ,"blue"),( 3 ,"green", 3 ,"blue", 2 ,"red"),( 4 ,"red"),( 12 ,"red", 5 ,"green", 2 ,"blue")],
	[(2 ,"red", 11 ,"blue"),( 16 ,"green", 7 ,"red", 16 ,"blue"),( 4 ,"blue", 11 ,"green", 7 ,"red"),( 8 ,"green", 18 ,"blue", 8 ,"red")],
	[(2 ,"blue", 2 ,"green", 1 ,"red"),( 1 ,"red", 1 ,"green"),( 1 ,"red"),( 4 ,"green"),( 1 ,"blue"),( 1 ,"blue", 7 ,"green")],
	[(4 ,"blue", 3 ,"green"),( 16 ,"green", 2 ,"red", 5 ,"blue"),( 1 ,"red", 13 ,"green", 2 ,"blue"),( 3 ,"blue", 12 ,"green", 2 ,"red"),( 2 ,"red", 5 ,"blue", 4 ,"green"),( 10 ,"green", 2 ,"blue")],
	[(3 ,"blue", 8 ,"green"),( 4 ,"green", 3 ,"blue"),( 7 ,"green", 5 ,"blue", 5 ,"red"),( 8 ,"green"),( 3 ,"red", 6 ,"blue", 9 ,"green"),( 2 ,"red", 10 ,"green", 4 ,"blue")],
	[(7 ,"blue", 6 ,"green", 5 ,"red"),( 7 ,"red", 2 ,"blue"),( 5 ,"red", 11 ,"green", 14 ,"blue"),( 8 ,"green", 17 ,"red")],
	[(3 ,"green", 8 ,"blue", 2 ,"red"),( 4 ,"green", 7 ,"blue", 6 ,"red"),( 13 ,"blue", 8 ,"green", 2 ,"red"),( 10 ,"red", 6 ,"blue", 5 ,"green"),( 11 ,"green", 3 ,"blue", 4 ,"red"),( 9 ,"red", 5 ,"green", 9 ,"blue")],
	[(4 ,"red", 18 ,"blue", 13 ,"green"),( 9 ,"green", 5 ,"red", 3 ,"blue"),( 4 ,"green", 3 ,"blue", 4 ,"red"),( 8 ,"red", 4 ,"green", 7 ,"blue"),( 8 ,"red", 4 ,"blue", 6 ,"green"),( 10 ,"green", 5 ,"red", 14 ,"blue")],
	[(12 ,"red", 14 ,"blue", 9 ,"green"),( 9 ,"blue", 6 ,"red", 4 ,"green"),( 2 ,"red", 5 ,"blue"),( 1 ,"red", 12 ,"blue")],
	[(11 ,"blue", 13 ,"red", 11 ,"green"),( 4 ,"blue", 9 ,"green"),( 8 ,"blue", 9 ,"red"),( 7 ,"red", 11 ,"green", 7 ,"blue")],
	[(10 ,"blue", 8 ,"red", 12 ,"green"),( 10 ,"red", 12 ,"blue", 9 ,"green"),( 3 ,"green", 17 ,"red"),( 12 ,"green", 15 ,"blue", 16 ,"red"),( 6 ,"green", 15 ,"blue", 1 ,"red"),( 9 ,"red", 6 ,"blue", 10 ,"green")],
	[(7 ,"red", 7 ,"blue"),( 3 ,"blue", 1 ,"red", 1 ,"green"),( 3 ,"red", 8 ,"blue")],
	[(1 ,"blue", 3 ,"red"),( 10 ,"green", 5 ,"blue"),( 4 ,"green"),( 3 ,"red", 11 ,"green"),( 3 ,"blue", 15 ,"green", 3 ,"red")],
	[(1 ,"red"),( 2 ,"blue", 2 ,"green", 1 ,"red"),( 6 ,"green", 1 ,"blue")],
	[(7 ,"red", 4 ,"blue"),( 4 ,"blue", 6 ,"red", 7 ,"green"),( 2 ,"green", 19 ,"red", 11 ,"blue"),( 11 ,"green", 9 ,"red")],
	[(4 ,"blue", 3 ,"green", 1 ,"red"),( 7 ,"blue", 1 ,"red", 3 ,"green"),( 5 ,"blue", 1 ,"green"),( 2 ,"blue", 10 ,"green", 2 ,"red"),( 2 ,"red", 6 ,"green", 5 ,"blue"),( 1 ,"red", 4 ,"green", 2 ,"blue")],
	[(9 ,"blue", 7 ,"red", 6 ,"green"),( 19 ,"blue", 4 ,"red", 5 ,"green"),( 6 ,"blue", 7 ,"red", 4 ,"green"),( 3 ,"blue", 4 ,"red", 2 ,"green")],
	[(6 ,"green", 12 ,"blue", 4 ,"red"),( 11 ,"red", 10 ,"green", 11 ,"blue"),( 3 ,"red", 14 ,"blue", 13 ,"green"),( 4 ,"blue", 3 ,"green")],
	[(2 ,"green", 1 ,"blue", 9 ,"red"),( 10 ,"red", 3 ,"green", 1 ,"blue"),( 11 ,"red", 2 ,"green"),( 2 ,"green", 1 ,"blue", 5 ,"red"),( 1 ,"red", 1 ,"blue", 3 ,"green"),( 13 ,"red", 4 ,"blue", 1 ,"green")],
	[(11 ,"green", 6 ,"blue"),( 7 ,"green", 6 ,"blue", 7 ,"red"),( 12 ,"green", 8 ,"blue", 11 ,"red"),( 4 ,"red", 2 ,"blue", 9 ,"green"),( 4 ,"green", 7 ,"blue", 2 ,"red")],
	[(3 ,"blue", 7 ,"red"),( 3 ,"blue", 5 ,"green", 2 ,"red"),( 5 ,"red", 1 ,"green", 3 ,"blue"),( 8 ,"green", 2 ,"blue", 11 ,"red"),( 3 ,"blue", 8 ,"green", 10 ,"red")],
	[(2 ,"green"),( 5 ,"blue"),( 1 ,"blue", 1 ,"red"),( 1 ,"red", 9 ,"blue", 2 ,"green"),( 2 ,"blue", 2 ,"green")],
	[(12 ,"blue", 13 ,"green"),( 5 ,"red", 11 ,"blue", 9 ,"green"),( 12 ,"green", 6 ,"red")],
	[(1 ,"blue", 15 ,"green", 12 ,"red"),( 15 ,"green", 5 ,"blue"),( 14 ,"green", 3 ,"blue", 8 ,"red")],
	[(11 ,"green", 8 ,"blue", 1 ,"red"),( 9 ,"green", 8 ,"blue", 1 ,"red"),( 13 ,"green", 5 ,"red", 6 ,"blue"),( 5 ,"red", 7 ,"green", 20 ,"blue"),( 10 ,"blue", 5 ,"red")],
	[(3 ,"blue"),( 6 ,"blue", 5 ,"red"),( 4 ,"red", 1 ,"green", 4 ,"blue"),( 7 ,"blue", 6 ,"red"),( 7 ,"red", 1 ,"blue"),( 1 ,"red", 1 ,"blue", 1 ,"green")],
	[(11 ,"green", 3 ,"red", 8 ,"blue"),( 2 ,"red", 15 ,"green", 2 ,"blue"),( 5 ,"green", 8 ,"blue", 2 ,"red"),( 8 ,"blue", 14 ,"green"),( 2 ,"blue", 13 ,"green")],
	[(9 ,"red", 4 ,"green"),( 7 ,"green", 4 ,"red"),( 2 ,"red", 4 ,"blue", 6 ,"green"),( 6 ,"red", 4 ,"blue", 9 ,"green"),( 1 ,"green", 3 ,"red"),( 6 ,"green", 1 ,"blue", 8 ,"red")],
	[(5 ,"blue", 3 ,"red", 3 ,"green"),( 5 ,"red"),( 2 ,"red", 3 ,"green", 8 ,"blue")],
	[(10 ,"green", 1 ,"red", 1 ,"blue"),( 3 ,"red", 1 ,"green", 1 ,"blue"),( 4 ,"red", 10 ,"green")],
	[(16 ,"red", 2 ,"green", 6 ,"blue"),( 6 ,"red", 3 ,"green", 8 ,"blue"),( 3 ,"green", 10 ,"red", 5 ,"blue"),( 4 ,"blue", 3 ,"green"),( 15 ,"red")],
	[(3 ,"green", 2 ,"red"),( 5 ,"green", 4 ,"blue"),( 5 ,"green", 8 ,"red", 3 ,"blue")],
	[(7 ,"green", 16 ,"blue", 7 ,"red"),( 1 ,"green", 12 ,"red", 2 ,"blue"),( 15 ,"green", 16 ,"blue", 7 ,"red")],
	[(1 ,"red", 6 ,"green", 5 ,"blue"),( 2 ,"green", 1 ,"blue"),( 2 ,"green", 1 ,"red", 1 ,"blue"),( 5 ,"green", 4 ,"blue")],
	[(3 ,"green", 3 ,"red", 4 ,"blue"),( 1 ,"red", 1 ,"green"),( 6 ,"blue", 9 ,"red", 1 ,"green"),( 1 ,"green", 11 ,"red", 3 ,"blue"),( 7 ,"red", 6 ,"blue")],
	[(2 ,"blue", 3 ,"red", 4 ,"green"),( 5 ,"red", 7 ,"blue", 14 ,"green"),( 8 ,"blue", 5 ,"red", 16 ,"green"),( 2 ,"blue", 5 ,"red", 7 ,"green"),( 5 ,"green", 9 ,"blue", 1 ,"red")],
	[(1 ,"blue", 3 ,"red", 7 ,"green"),( 11 ,"green", 4 ,"red", 1 ,"blue"),( 1 ,"red", 1 ,"blue", 6 ,"green"),( 2 ,"blue", 2 ,"green"),( 8 ,"green", 2 ,"blue"),( 3 ,"red", 2 ,"blue", 4 ,"green")],
	[(6 ,"blue", 4 ,"red", 1 ,"green"),( 8 ,"red", 3 ,"blue", 3 ,"green"),( 1 ,"green", 2 ,"blue", 5 ,"red"),( 1 ,"blue", 3 ,"green")],
	[(8 ,"green", 1 ,"red", 5 ,"blue"),( 2 ,"green", 7 ,"blue"),( 11 ,"blue", 5 ,"green", 8 ,"red"),( 7 ,"blue", 3 ,"red", 4 ,"green")],
	[(3 ,"green", 1 ,"red", 9 ,"blue"),( 13 ,"red", 5 ,"blue", 8 ,"green"),( 5 ,"green", 2 ,"red", 7 ,"blue")],
	[(4 ,"green", 10 ,"blue", 8 ,"red"),( 4 ,"red", 10 ,"blue", 2 ,"green"),( 2 ,"green", 10 ,"blue", 5 ,"red"),( 5 ,"green", 2 ,"red", 10 ,"blue")],
	[(5 ,"green", 1 ,"blue"),( 3 ,"blue", 11 ,"green", 8 ,"red"),( 8 ,"blue", 2 ,"red", 12 ,"green"),( 4 ,"green", 4 ,"blue", 4 ,"red")],
	[(1 ,"blue", 13 ,"green"),( 8 ,"blue", 3 ,"red", 4 ,"green"),( 1 ,"red", 3 ,"blue", 10 ,"green")],
	[(18 ,"green", 4 ,"red"),( 1 ,"blue", 2 ,"red", 9 ,"green"),( 6 ,"red", 3 ,"blue", 10 ,"green"),( 3 ,"blue", 15 ,"green", 4 ,"red")],
	[(2 ,"blue", 3 ,"green", 6 ,"red"),( 1 ,"green", 1 ,"blue", 8 ,"red"),( 8 ,"red", 3 ,"green", 1 ,"blue"),( 2 ,"blue"),( 8 ,"red", 2 ,"green", 2 ,"blue")],
	[(1 ,"green", 2 ,"red", 1 ,"blue"),( 8 ,"green", 4 ,"blue", 1 ,"red"),( 7 ,"blue", 1 ,"red", 11 ,"green"),( 9 ,"green", 3 ,"blue"),( 1 ,"red", 2 ,"blue"),( 1 ,"red", 6 ,"blue")],
	[(7 ,"blue", 9 ,"green", 2 ,"red"),( 5 ,"red", 9 ,"green"),( 1 ,"blue", 8 ,"red", 13 ,"green")],
]

#dat = [[(3 ,"blue", 4 ,"red"),( 1 ,"red", 2 ,"green", 6 ,"blue"),( 2 ,"green")], [(1 ,"blue", 2 ,"green"),( 3 ,"green", 4 ,"blue", 1 ,"red"),( 1 ,"green", 1 ,"blue")], [(8 ,"green", 6 ,"blue", 20 ,"red"),( 5 ,"blue", 4 ,"red", 13 ,"green"),( 5 ,"green", 1 ,"red")], [(1 ,"green", 3 ,"red", 6 ,"blue"),( 3 ,"green", 6 ,"red"),( 3 ,"green", 15 ,"blue", 14 ,"red")], [(6 ,"red", 1 ,"blue", 3 ,"green"),( 2 ,"blue", 1 ,"red", 2 ,"green")], ]

cap = {"red":12,"green":13,"blue":14}
def possible(game):
	for step in game:
		for i in range(0,len(step),2):
			if step[i] > cap.get(step[i+1],0):
				return False
	return True

print(sum(i for i, g in enumerate(dat, 1) if possible(g)))

def power(game):
	req = {"red":0,"green":0,"blue":0}
	for step in game:
		for i in range(0,len(step),2):
			if step[i] > req[step[i+1]]:
				req[step[i+1]] = step[i]
	return req["red"] * req["green"] * req["blue"]
print(sum(power(g) for g in dat))
