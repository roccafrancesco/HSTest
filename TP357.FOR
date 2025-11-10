      SUBROUTINE TP357(MODE)
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
      COMMON     /D357/DALPHA,SUM,P1,Q1,R1,S1,ALPHA,CA,SA,PI,QI,
     /           A,B,C,AABB,TEST,AJ,PH,SP,CP,RI,SI,TEST1,CALCX,CALCY,SQL
      DOUBLEPRECISION DALPHA,SUM,P1,Q1,R1,S1,ALPHA,CA,SA,PI,QI,
     /           A,B,C,AABB,TEST,AJ,PH,SP,CP,RI,SI,TEST1,CALCX,CALCY,SQL
      DOUBLEPRECISION XPT(36),YPT(36),P0,Q0,R0,S0
      DATA(XPT(KI),KI=1,36)/0.113D+3,0.1101D+3,0.1062D+3,0.1013D+3,
     /             0.954D+2,0.888D+2,0.816D+2,0.74D+2,0.661D+2,
     /             0.584D+2,0.51D+2,0.443D+2,0.387D+2,0.345D+2,
     /             0.324D+2,0.329D+2,0.364D+2,0.428D+2,0.509D+2,
     /             0.59D+2,0.658D+2,0.715D+2,0.765D+2,0.811D+2,
     /             0.856D+2,0.902D+2,0.946D+2,0.989D+2,0.103D+3,
     /             0.1067D+3,0.1099D+3,0.1125D+3,0.1144D+3,
     /             0.1155D+3,0.1157D+3,0.1149D+3/,
     /    (YPT(KJ),KJ=1,36)/0.402D+2,0.468D+2,0.533D+2,0.594D+2,
     /             0.65D+2,0.699D+2,0.739D+2,0.769D+2,0.789D+2,
     /             0.798D+2,0.797D+2,0.785D+2,0.765D+2,0.736D+2,
     /             0.702D+2,0.66D+2,0.609D+2,0.543D+2,0.458D+2,
     /             0.361D+2,0.265D+2,0.181D+2,0.114D+2,0.62D+1,
     /             0.26D+1,0.3D+0,-0.7D+0,-0.6D+0,0.7D+0,0.31D+1,
     /             0.64D+1,0.105D+2,0.155D+2,0.21D+2,0.271D+2,
     /             0.336D+2/
      DATA P0,Q0,R0,S0/0.9D+2,0.0D+0,0.0D+0,0.0D+0/
      INTEGER I,K
      GOTO(1,2,3,3,3),MODE
    1 N=4
      NILI=0
      NINL=0
      NENL=0
      NENL=0
      X(1)=0.136D+3
      X(2)=0.0D+0
      X(3)=0.748D+2
      X(4)=0.757D+2
      DO 6 I=1,4
      LXL(I)=.TRUE.
    6 LXU(I)=.TRUE.
      LEX=.FALSE.
      NEX=1
      FEX=0.35845660D+0
      XEX(1)=0.13600762D+3
      XEX(2)=0.31371415D-1
      XEX(3)=0.73594390D+2
      XEX(4)=0.72187426D+2
      DO 7 K=1,4
    7 XL(K)=0.0D+0      
      XU(1)=0.150D+3
      XU(2)=0.5D+2
      XU(3)=0.1D+3
      XU(4)=0.1D+3
      RETURN
    2 DALPHA=0.3141527D+1/0.18D+2
      SUM=0.0D+0
      P1=X(1)
      Q1=X(2)
      R1=X(3)
      S1=X(4)
      DO 54 I=2,36
      ALPHA=DALPHA*(I-1)
      CA=DCOS(ALPHA)
      SA=DSIN(ALPHA)
      PI=P1*CA-Q1*SA+P0*(0.1D+1-CA)+Q0*SA
      QI=P1*SA+Q1*CA+Q0*(0.1D+1-CA)-P0*SA
      A=R0*S1-S0*R1-Q1*R0+P1*S0+PI*Q1-P1*QI+QI*R1-PI*S1
      B=-R0*R1-S0*S1+P1*R0+Q1*S0-P1*PI-Q1*QI+PI*R1+QI*S1
      C=-R1*R0-S1*S0+PI*R0+QI*S0+P1*R1+Q1*S1-(P1*P1+Q1*Q1+PI*PI+QI
     F*QI)/0.2D+1
      AABB=A*A+B*B
      AJ=0.1D+1
      TEST=0.0D0
      IF (AABB.LT.0.1D-29) THEN 
         IF (DABS(A).LT.1.0D-29) A = 1.0D-29
         GOTO 50
      ENDIF   
      TEST=C/DSQRT(AABB)
      IF(DABS(TEST).GT.0.1D+1) GOTO 51
   52 PH=DASIN(TEST)-DATAN(B/A)
   55 SP=DSIN(PH)
      CP=DCOS(PH)
      RI=R1*CP-S1*SP+PI-P1*CP+Q1*SP
      SI=R1*SP+S1*CP+QI-P1*SP-Q1*CP
      TEST1=(R1-R0)**2+(S1-S0)**2
      IF(TEST1.LT.0.1D-9) TEST1=0.1D-9
      IF(DABS((TEST1-(RI-R0)**2-(SI-S0)**2)/TEST1).LT.0.1D-2)GOTO53
      IF(AJ.EQ.0.2D+1) GOTO 51
      TEST=-TEST
      AJ=0.2D+1
      GOTO 52
   50 PH=-DATAN(B/A)
      GOTO 55
   51 FX=0.1D+21
      RETURN
   53 CALCX=XPT(1)*CP-YPT(1)*SP+PI-P1*CP+Q1*SP
      CALCY=XPT(1)*SP+YPT(1)*CP+QI-P1*SP-Q1*CP
   54 SUM=SUM+(CALCX-XPT(I))**2+(CALCY-YPT(I))**2
      SQL=(R1-R0)**2+(S1-S0)**2+(R1-P1)**2+(S1-Q1)**2+(P1-P0)**2+
     F(Q1-Q0)**2
      FX=SUM/0.1D+3+SQL/0.625D+5
    3 RETURN
      END
