      SUBROUTINE TP284(MODE)
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
      DOUBLEPRECISION SUM
      INTEGER A(10,15),B(10),C(15)
      DATA C/20,40,400,20,80,20,40,140,380,280,80,40,140,40,120/
      DATA B/385,470,560,565,645,430,485,455,390,460/
      DATA A/100,90,70,2*50,40,30,20,10,5,2*100,50,0,10,
     F 0,60,30,70,3*10,2*0,70,50,30,40,10,100,5,35,55,
     F 65,60,95,90,25,35,5,10,20,25,35,45,50,0,40,25,
     F 20,0,5,2*100,45,35,30,25,65,5,2*0,40,35,0,10,5,
     F 15,0,10,25,35,50,60,35,60,25,10,30,35,0,55,2*0,
     F 65,2*0,80,0,95,10,25,30,15,5,45,70,20,0,70,55,
     F 20,60,0,75,15,20,30,25,20,5,0,10,75,100,20,25,
     F 30,0,10,45,40,30,35,75,0,70,5,15,35,20,25,0,30,
     F 10,5,15,65,50,10,0,10,40,65,0,5,15,20,55,30/
      GOTO(1,2,3,4,5),MODE
    1 N=15
      NILI=0
      NINL=10
      NELI=0
      NENL=0
      FEX=.0D+0
      DO 6 I=1,15
      X(I)=.0D+0
      XEX(I)=.1D+1
      FEX=FEX-DBLE(C(I))
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      RETURN
    2 FX=.0D+0
      DO 7 I=1,15
    7 FX=FX-DBLE(C(I))*X(I)
      RETURN
    3 DO 8 I=1,15
    8 GF(I)=-DBLE(C(I))
      RETURN
    4 DO 10 I=1,10
      SUM=.0D+0
      DO 9 J=1,15
    9 SUM=SUM+(DBLE(A(I,J))*X(J)**2)
   10 IF (INDEX1(I)) G(I)=DBLE(B(I))-SUM
      RETURN
    5 DO 12 J=1,10
      DO 11 I=1,15
      IF (.NOT.INDEX2(J)) GOTO 12
   11 GG(J,I)=-.2D+1*DBLE(A(J,I))*X(I)
   12 CONTINUE
      RETURN
      END
