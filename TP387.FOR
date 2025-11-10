      SUBROUTINE TP387(MODE)
      IMPLICIT DOUBLEPRECISION (A-H,O-Z)  
      INTEGER    MODE,NMAX,MMAX,N,NILI,NINL,NELI,NENL,NEX   
      DOUBLEPRECISION X,G,GF,GG,FX,XL,XU,FEX,XEX 
      LOGICAL    INDEX1,INDEX2,LXL,LXU,LEX    
      PARAMETER (NMAX=101, MMAX=50)
      COMMON     /L1/  N,NILI,NINL,NELI,NENL 
     /           /L2/  X(NMAX)                              
     /           /L3/  G(MMAX)           
     /           /L4/  GF(NMAX)          
     /           /L5/  GG(MMAX,NMAX)        
     /           /L6/  FX
     /           /L9/  INDEX1(MMAX)     
     /           /L10/ INDEX2(MMAX)
     /           /L11/ LXL(NMAX)       
     /           /L12/ LXU(NMAX)    
     /           /L13/ XL(NMAX)     
     /           /L14/ XU(NMAX)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I,J
      DOUBLEPRECISION C,A(10,15),B(10),D(15)
      DATA B/3.85D+2,4.7D+2,5.6D+2,5.65D+2,6.45D+2,4.3D+2,4.85D+2,
     F 4.55D+2,3.9D+2,4.6D+2/
      DATA A/1.D+2,9.D+1,7.D+1,2*5.D+1,4.D+1,3.D+1,2.D+1,1.D+1,5.D+0,
     F 2*1.D+2,5.D+1,0.D+0,1.D+1,0.D+0,6.D+1,3.D+1,7.D+1,1.D+1,
     F 2*1.D+1,2*0.D+0,7.D+1,5.D+1,3.D+1,4.D+1,1.D+1,1.D+2,5.D+0,
     F 3.5D+1,5.5D+1,6.5D+1,6.D+1,9.5D+1,9.D+1,2.5D+1,3.5D+1,5.D+0,
     F 1.D+1,2.D+1,2.5D+1,3.5D+1,4.5D+1,5.D+1,0.D+0,4.D+1,2.5D+1,2.D+1,
     F 0.D+0,5.D+0,2*1.D+2,4.5D+1,3.5D+1,3.D+1,2.5D+1,6.5D+1,5.D+0,
     F 2*0.D+0,4.D+1,3.5D+1,0.D+0,1.D+1,5.D+0,1.5D+1,0.D+0,1.D+1,
     F 2.5D+1,3.5D+1,5.D+1,6.D+1,3.5D+1,6.D+1,2.5D+1,1.D+1,3.D+1,3.5D+1,
     F 0.D+0,5.5D+1,2*0.D+0,6.5D+1,2*0.D+0,8.D+1,0.D+0,9.5D+1,
     F 1.D+1,2.5D+1,3.D+1,1.5D+1,5.D+0,4.5D+1,7.D+1,2.D+1,0.D+0,7.D+1,
     F 5.5D+1,2.D+1,6.D+1,0.D+0,7.5D+1,1.5D+1,2.D+1,3.D+1,2.5D+1,2.D+1,
     F 5.D+0,0.D+0,1.D+1,7.5D+1,1.D+2,2.D+1,2.5D+1,3.D+1,0.D+0,1.D+1,
     F 4.5D+1,4.D+1,3.D+1,3.5D+1,7.5D+1,0.D+0,7.D+1,5.D+0,1.5D+1,3.5D+1,
     F 2.D+1,2.5D+1,0.D+0,3.D+1,1.D+1,5.D+0,1.5D+1,6.5D+1,5.D+1,1.D+1,
     F 0.D+0,1.D+1,4.D+1,6.5D+1,0.D+0,5.D+0,1.5D+1,2.D+1,5.5D+1,3.D+1/
      DATA D/4.86D+2,6.4D+2,7.58D+2,7.76D+2,4.77D+2,7.07D+2,1.75D+2,
     F 6.19D+2,6.27D+2,6.14D+2,4.75D+2,3.77D+2,5.24D+2,4.68D+2,5.29D+2/
      GOTO (1,2,3,4,5),MODE 
    1 N=15
      NILI=0
      NINL=11
      NELI=0
      NENL=0
      DO 6 I=1,15
      X(I)=0.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=-0.82501417D+4
      XEX(1)=0.10125415D+1
      XEX(2)=0.10158505D+1
      XEX(3)=0.10309039D+1
      XEX(4)=0.99697018D+0
      XEX(5)=0.98528372D+0
      XEX(6)=0.10368532D+1
      XEX(7)=0.99349349D+0
      XEX(8)=0.97201160D+0
      XEX(9)=0.99994095D+0
      XEX(10)=0.99547294D+0
      XEX(11)=0.96953850D+0
      XEX(12)=0.10080569D+1
      XEX(13)=0.98236999D+0
      XEX(14)=0.99057993D+0
      XEX(15)=0.97760168D+0
      RETURN
    2 FX=0.D+0
      DO 20 I=1,15
   20 FX=FX-D(I)*X(I)
      RETURN
    3 DO 11 I=1,15
   11 GF(I)=-D(I)
      RETURN
    4 DO 7 I=1,10
      IF (.NOT.INDEX1(I)) GOTO 7
      C=0.D+0
      DO 9 J=1,15
    9 C=C+A(I,J)*X(J)**2
      G(I)=B(I)-C
    7 CONTINUE
      IF (.NOT.INDEX1(11)) GOTO 12
      C=0.D+0
      DO 14 J=1,15
   14 C=C+DBLE(J)*(X(J)-2.D+0)**2
      G(11)=C/2.D+0-6.1D+1
   12 RETURN
    5 DO 10 I=1,10
      IF (.NOT.INDEX2(I)) GOTO 10
      DO 13 J=1,15
   13 GG(I,J)=-2.D+0*A(I,J)*X(J)
   10 CONTINUE
      IF (.NOT.INDEX2(11)) GOTO 15
      DO 16 J=1,15
   16 GG(11,J)=DBLE(J)*(X(J)-2.D+0)
   15 RETURN
      END
