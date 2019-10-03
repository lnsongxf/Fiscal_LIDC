* COPYRIGHT (c) 1964 AEA Technology
*######DATE 17 February 1994
C       Toolpack tool decs employed.
C       SAVE statements added.
C
      SUBROUTINE VD01AD(ITEST,X,F,MAXFUN,ABSACC,RELACC,XSTEP)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ABSACC,F,RELACC,X,XSTEP
      INTEGER ITEST,MAXFUN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION D,DA,DB,DC,FA,FB,FC,XINC
      INTEGER IINC,IS,MC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DABS
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Executable Statements ..
      GO TO (1,2,2) ITEST

    2 IS = 6 - ITEST
      ITEST = 1
      IINC = 1
      XINC = XSTEP + XSTEP
      MC = IS - 3
      IF (MC) 4,4,15
    3 MC = MC + 1
      IF (MAXFUN-MC) 12,15,15
   12 ITEST = 4
   43 X = DB
      F = FB
      IF (FB-FC) 15,15,44
   44 X = DC
      F = FC
   15 RETURN

    1 GO TO (5,6,7,8) IS

    8 IS = 3
    4 DC = X
      FC = F
      X = X + XSTEP
      GO TO 3

    7 IF (FC-F) 9,10,11
   10 X = X + XINC
      XINC = XINC + XINC
      GO TO 3

    9 DB = X
      FB = F
      XINC = -XINC
      GO TO 13

   11 DB = DC
      FB = FC
      DC = X
      FC = F
   13 X = DC + DC - DB
      IS = 2
      GO TO 3

    6 DA = DB
      DB = DC
      FA = FB
      FB = FC
   32 DC = X
      FC = F
      GO TO 14

    5 IF (FB-FC) 16,17,17
   17 IF (F-FB) 18,32,32
   18 FA = FB
      DA = DB
   19 FB = F
      DB = X
      GO TO 14

   16 IF (FA-FC) 21,21,20
   20 XINC = FA
      FA = FC
      FC = XINC
      XINC = DA
      DA = DC
      DC = XINC
   21 XINC = DC
      IF ((D-DB)* (D-DC)) 32,22,22
   22 IF (F-FA) 23,24,24
   23 FC = FB
      DC = DB
      GO TO 19

   24 FA = F
      DA = X
   14 IF (FB-FC) 25,25,29
   25 IINC = 2
      XINC = DC
      IF (FB-FC) 29,45,29
   29 D = (FA-FB)/ (DA-DB) - (FA-FC)/ (DA-DC)
      IF (D* (DB-DC)) 33,33,37
   37 D = 0.5D0* (DB+DC- (FB-FC)/D)
      IF (DABS(D-X)-DABS(ABSACC)) 34,34,35
   35 IF (DABS(D-X)-DABS(D*RELACC)) 34,34,36
   34 ITEST = 2
      GO TO 43

   36 IS = 1
      X = D
      IF ((DA-DC)* (DC-D)) 3,26,38
   38 IS = 2
      GO TO (39,40) IINC

   39 IF (DABS(XINC)-DABS(DC-D)) 41,3,3
   33 IS = 2
      GO TO (41,42) IINC

   41 X = DC
      GO TO 10

   40 IF (DABS(XINC-X)-DABS(X-DC)) 42,42,3
   42 X = 0.5D0* (XINC+DC)
      IF ((XINC-X)* (X-DC)) 26,26,3
   45 X = 0.5D0* (DB+DC)
      IF ((DB-X)* (X-DC)) 26,26,3
   26 ITEST = 3
      GO TO 43

      END
