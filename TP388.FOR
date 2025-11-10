      SUBROUTINE TP388(MODE)
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
      INTEGER I,J,L
      DOUBLEPRECISION C,A(10,15),A1(4,15),B(15),D(15)
      DATA B/3.85D+2,4.7D+2,5.6D+2,5.65D+2,6.45D+2,4.3D+2,4.85D+2,
     F 4.55D+2,3.9D+2,4.6D+2,0.D+0,7.D+1,3.61D+2,2.65D+2,3.95D+2/
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
      DATA A1/1.D+0,4.5D+1,5.3D+1,1.2D+1,2.D+0,2.5D+1,7.4D+1,4.3D+1,
     F 3.D+0,3.5D+1,2.6D+1,5.1D+1,4.D+0,8.5D+1,1.7D+1,3.9D+1,5.D+0,
     F 4.D+1,2.5D+1,5.8D+1,6.D+0,7.3D+1,2.5D+1,4.2D+1,7.D+0,1.7D+1,
     F 2.6D+1,6.D+1,8.D+0,5.2D+1,2.4D+1,2.D+1,9.D+0,8.6D+1,8.5D+1,4.D+1,
     F 1.D+1,1.4D+1,3.5D+1,8.D+1,1.5D+1,3.D+1,1.4D+1,7.5D+1,1.6D+1,
     F 5.D+1,2.3D+1,8.5D+1,1.7D+1,4.D+1,3.7D+1,9.5D+1,1.8D+1,7.D+1,
     F 5.6D+1,2.3D+1,1.9D+1,6.D+1,1.D+1,6.7D+1/
      DATA D/4.86D+2,6.4D+2,7.58D+2,7.76D+2,4.77D+2,7.07D+2,1.75D+2,
     F 6.19D+2,6.27D+2,6.14D+2,4.75D+2,3.77D+2,5.24D+2,4.68D+2,5.29D+2/
      GOTO (1,2,3,4,5),MODE 
    1 N=15
      NILI=4
      NINL=11
      NELI=0
      NENL=0
      DO 6 I=1,15
      X(I)=0.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      DO 19 I=12,15
      DO 19 J=1,15
   19 GG(I-11,J)=-A1(I-11,J)
      LEX=.FALSE.
      NEX=1
      FEX=-0.58210842D+4
      XEX(1)=0.62683876D+0
      XEX(2)=0.14330999D+1
      XEX(3)=0.14625963D+1
      XEX(4)=0.73133338D+0
      XEX(5)=0.78614240D+0
      XEX(6)=0.12048598D+1
      XEX(7)=-0.11433978D+1
      XEX(8)=0.10611103D+1
      XEX(9)=-0.13389293D+0
      XEX(10)=0.11820107D+1
      XEX(11)=0.96917757D+0
      XEX(12)=-0.84501289D+0
      XEX(13)=0.48122454D+0
      XEX(14)=-0.33986164D+0
      XEX(15)=0.68589012D+0
      RETURN
    2 FX=0.D+0
      DO 20 I=1,15
   20 FX=FX-D(I)*X(I)
      RETURN
    3 DO 11 I=1,15
   11 GF(I)=-D(I)
      RETURN
    4 DO 7 I=12,15
      L=I-11
      IF (.NOT.INDEX1(L)) GOTO 7
      C=0.D+0
      DO 9 J=1,15
    9 C=C+A1(L,J)*X(J)
      G(L)=B(I)-C
    7 CONTINUE
      DO 14 I=1,10
      IF (.NOT.INDEX1(I+4)) GOTO 14
      C=0.D+0
      DO 16 J=1,15
   16 C=C+A(I,J)*X(J)**2
      G(I+4)=B(I)-C
   14 CONTINUE
      IF (.NOT.INDEX1(15)) GOTO 17
      C=0.D+0
      DO 18 J=1,15
   18 C=C+DBLE(J)*(X(J)-2.D+0)**2
      G(15)=C/2.D+0-193.121D+0
   17 RETURN
    5 DO 22 I=1,10
      IF (.NOT.INDEX2(I+4)) GOTO 22
      DO 24 J=1,15
   24 GG(I+4,J)=-2.D+0*A(I,J)*X(J)
   22 CONTINUE
      IF (.NOT.INDEX2(15)) GOTO 25
      DO 26 J=1,15
   26 GG(15,J)=DBLE(J)*(X(J)-2.D+0)
   25 RETURN
      END
