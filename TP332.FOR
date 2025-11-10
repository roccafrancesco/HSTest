      SUBROUTINE TP332(MODE)
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
      DOUBLEPRECISION A,B,C,PI,TR,XXX,YYY,PIM,PBIG,PANGLE
      INTEGER I      
      DATA PI/.31415926535D+1/
      GOTO(1,2,3,4,3),MODE
    1 N=2
      NILI=0
      NINL=2
      NELI=0
      NENL=0
      DO 6 I=1,2
      X(I)=.75D+0
      LXU(I)=.TRUE.
      XU(I)=.15D+1
      LXL(I)=.TRUE.
    6 XL(I)=.0D+0
      LEX=.FALSE.
      NEX=1
      FEX=.11495015D+3
      XEX(1)=.91139872D+0
      XEX(2)=.29280207D-1
    3 RETURN
    2 PIM=PI/.36D+1
      FX=.0D+0
      DO 7 I=1,100
      TR=PI*((.1D+1/.3D+1)+((DBLE(I)-.1D+1)/.18D+3))
      A=DLOG(TR)
      B=DSIN(TR)
      C=DCOS(TR)
      XXX=((A+X(2))*B+X(1)*C)
      YYY=((A+X(2))*C-X(1)*B)
    7 FX=FX+PIM*(XXX**2+YYY**2)
      RETURN
    4 PBIG=-.36D+3
      PIM=.18D+3/PI
      DO 8 I=1,100
      TR=PI*((.1D+1/.3D+1)+((DBLE(I)-.1D+1)/.18D+3))
      A=.1D+1/TR-X(1)
      B=DLOG(TR)+X(2)
      PANGLE=PIM*DATAN(DABS(A/B))
    8 IF (PANGLE.GT.PBIG) PBIG=PANGLE
      IF (INDEX1(1)) G(1)=.3D+2-PBIG
      IF (INDEX1(2)) G(2)=PBIG+.3D+2
      RETURN
      END
