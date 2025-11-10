      SUBROUTINE TP356(MODE)
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
      DOUBLEPRECISION L,LOAD,TD,SIGD,FH,T1,M,R,J,T2,COSA,WP,
     /        T,SIG,E,EI,GH,GJ,EITC,EIDC,REITC,REIDC,PC,DEL,PHI(5)
      INTEGER I
      GOTO(1,2,3,4,3),MODE
    1 N=4
      NILI=1
      NINL=4
      NENL=0
      NENL=0
      X(1)=0.1D+1
      X(2)=0.7D+1
      X(3)=0.8D+1
      X(4)=0.1D+1
      DO 6 I=1,3
      LXU(I)=.FALSE.
    6 LXL(I)=.TRUE.
      LXL(4)=.FALSE.
      LXU(4)=.FALSE.
      XL(1)=0.125D+0
      XL(2)=0.0D+0
      XL(3)=0.0D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.23811648D+1
      XEX(1)=0.24436898D+0
      XEX(2)=0.62187934D+1
      XEX(3)=0.82914714D+1
      XEX(4)=0.24436898D+0
      RETURN
    2 FX=0.110471D+1*X(1)*X(1)*X(2)+0.4811D-1*X(3)*X(4)*(0.14D+2
     /   +X(2))
    3 RETURN
    4 L=0.14D+2
      LOAD=0.6D+4
      TD=0.136D+5
      SIGD=0.3D+5
      FH=LOAD
      T1=FH/(0.1414D+1*X(1)*X(2))
      M=FH*(L+(X(2)/0.2D+1))
      R=DSQRT((X(2)*X(2)/0.4D+1)+((X(3)+X(1))/0.2D+1)**2)
      J=0.2D+1*(0.707D+0*X(1)*X(2)*((X(2)*X(2)/0.12D+2)+((X(3)
     /   +X(1))/0.2D+1)**2))
      T2=M*R/J
      COSA=X(2)/(0.2D+1*R)
      WP=DABS(T1*T1+0.2D+1*T1*T2*COSA+T2*T2)
      IF (WP.GT.0.0) THEN
         T=DSQRT(WP)
      ELSE
         T=0.0
      ENDIF      
      SIG=0.6D+1*FH*L/(X(4)*X(3)*X(3))
      PHI(1)=(TD-T)/0.1D+5
      PHI(2)=(SIGD-SIG)/0.1D+5
      PHI(3)=X(4)-X(1)
      E=0.3D+8
      EI=E*X(3)*X(4)*X(4)*X(4)/0.12D+2
      GH=0.12D+8
      GJ=GH*X(3)*X(4)*X(4)*X(4)/0.3D+1
      EITC=EI*GJ
      EIDC=EI/GJ
      IF (EITC.GT.0.0) THEN
         REITC=DSQRT(EITC)
      ELSE
         REITC=0.0
      ENDIF      
      IF (EIDC.GT.0.0) THEN
         REIDC=DSQRT(EIDC)
      ELSE
         REIDC=0.0
      ENDIF      
      PC=0.4013D+1*REITC*(0.1D+1-(X(3)/(0.2D+1*L))*REIDC)/(L*L)
      PHI(4)=(PC-FH)/0.1D+5
      DEL=0.4D+1*FH*L*L*L/(E*X(4)*X(3)*X(3)*X(3))
      PHI(5)=0.25D+0-DEL
      IF (INDEX1(1)) G(1)=PHI(3)
      IF (INDEX1(2)) G(2)=PHI(1)
      IF (INDEX1(3)) G(3)=PHI(2)
      IF (INDEX1(4)) G(4)=PHI(4)
      IF (INDEX1(5)) G(5)=PHI(5)
      RETURN
      END
