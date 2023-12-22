5 REM Solution for the Casio fx-5200P programmable calculator
10 R=1
20 INPUT "TIME=",T
30 INPUT "DIST=",D
40 A= SQR(T*T-4*D)
50 B=(T-A)/2
60 C=(T+A)/2
70 B= RND(B+0.5,-1)
80 C= RND(C+0.4999,-1)
90 R=R*(C-B)
100 PRINT R
110 GOTO 20
