      SUBROUTINE TP385(MODE)
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
     F 4.55D+2,8.9D+2,4.6D+2/
      DATA A/1.D+2,9.D+1,7.D+1,2*5.D+1,4.D+1,3.D+1,2.D+1,1.D+1,5.D+0,
     F 2*1.D+2,5.D+1,0.D+0,1.D+1,0.D+0,6.D+1,3.D+1,7.D+1,1.D+1,
     F 2*1.D+1,2*0.D+0,7.D+1,5.D+1,3.D+1,4.D+1,1.D+1,1.D+2,5.D+0,
     F 3.5D+1,5.5D+1,6.5D+1,6.D+1,9.5D+1,9.D+1,2.5D+1,3.5D+1,5.D+0,
     F 1.D+1,2.D+1,2.5D+1,3.5D+1,4.5D+1,5.D+1,0.D+0,4.D+1,2.5D+1,2.D+1,
     F 0.D+0,5.D+0,2*1.D+2,4.5D+1,3.5D+1,3.D+1,2.5D+1,6.5D+1,5.D+0,
     F 2*0.D+0,4.D+1,3.5D+1,0.D+0,1.D+1,5.D+0,1.5D+1,0.D+0,1.D+1,
     F 2.5D+1,3.5D+1,5.D+1,6.D+1,3.5D+1,6.D+1,2.5D+1,1.D+1,3.D+1,3.5D+1,
     F 0.D+0,5.5D+1,2*0.D+0,6.5D+1,2*0.D+0,8.D+1,5.D+2,9.5D+1,
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
      NINL=10
      NELI=0
      NENL=0
      DO 6 I=1,15
      X(I)=0.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=-0.83152859D+4
      XEX(1)=0.81347013D+00
      XEX(2)=0.11327964D+01
      XEX(3)=0.10861185D+01 
      XEX(4)=0.99832982D+00
      XEX(5)=0.10754861D+01
      XEX(6)=0.10688758D+01 
      XEX(7)=0.62781562D+00
      XEX(8)=0.10929981D+01
      XEX(9)=0.91363214D+00
      XEX(10)=0.86191234D+00
      XEX(11)=0.10047312D+01
      XEX(12)=0.87742923D+00
      XEX(13)=0.98671497D+00
      XEX(14)=0.10411268D+01
      XEX(15)=0.11860997D+01
      RETURN
    2 FX=0.D+0
      DO 15 I=1,15
   15 FX=FX-D(I)*X(I)
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
      RETURN
    5 DO 10 I=1,10
      IF (.NOT.INDEX2(I)) GOTO 10
      DO 13 J=1,15
   13 GG(I,J)=-2.D+0*A(I,J)*X(J)
   10 CONTINUE
      RETURN
      END
