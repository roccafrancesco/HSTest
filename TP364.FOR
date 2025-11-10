      SUBROUTINE TP364(MODE)
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
      DOUBLEPRECISION XMU1,XMU2,TP364A
      INTEGER I
      GOTO(1,2,3,4,5),MODE
    1 N=6
      NILI=2
      NINL=2
      NELI=0
      NENL=0      
      X(1)=1.D+0
      X(2)=4.5D+0
      X(3)=4.D+0
      X(4)=5.D+0
      X(5)=3.D+0
      X(6)=3.D+0
      DO 11 I=1,4
   11 LXL(I)=.TRUE.
      LXL(5)=.FALSE.
      LXL(6)=.FALSE.
      LXU(5)=.FALSE.
      LXU(6)=.FALSE.
      LXU(1)=.TRUE.
      LXU(4)=.TRUE.
      LXU(2)=.FALSE.
      LXU(3)=.FALSE.
      XL(1)=.5D+0
      XL(2)=0.01D+0
      XL(3)=0.D+0
      XL(4)=2.D+0
      XU(1)=3.D+0
      XU(4)=10.D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.0606002
      XEX(1)=0.99616882
      XEX(2)=0.41960616D+01
      XEX(3)=0.29771652D+01
      XEX(4)=0.39631949D+01
      XEX(5)=0.16536702D+01
      XEX(6)=0.12543998D+01
      RETURN
    2 FX=TP364A(X)
    3 RETURN
    4 XMU1=.7853981633D+0
      XMU2=2.356194491D+0
      IF (INDEX1(1)) G(1)=-X(1)+X(2)+X(3)-X(4)
      IF (INDEX1(2)) G(2)=-X(1)-X(2)+X(3)+X(4)
      IF (INDEX1(3)) G(3)=-X(2)*X(2)-X(3)*X(3)+(X(4)-X(1))*
     F (X(4)-X(1))+2.D+0*X(2)*X(3)*DCOS(XMU1)
      IF (INDEX1(4)) G(4)=X(2)*X(2)+X(3)*X(3)-(X(4)+X(1))*
     F (X(4)+X(1))-2.D+0*X(2)*X(3)*DCOS(XMU2)
    5 RETURN
      END
