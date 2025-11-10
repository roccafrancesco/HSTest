      SUBROUTINE TP349(MODE)
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
      COMMON     /D349/P1,XK1,XK2,V,C1,UT,ARGU,XLMTD,HEAT,AREA,ARE,HEA,
     /        DIA,PRESS,WATE,C2,C3,C4,C5,C6,C7,COST,VEST,C0,U,
     /        PHI,A11,A12,A13,A22,A23,A31,A32,A33,TEMP1,TEMP2,TEMP3
      DOUBLEPRECISION P1,XK1,XK2,V,C1,UT,ARGU,XLMTD,HEAT,AREA,ARE,HEA,
     /        DIA,PRESS,WATE,C2,C3,C4,C5,C6,C7,COST,VEST,C0,U,
     /        PHI(9),A11,A12,A13,A22,A23,A31,A32,A33,TEMP1,TEMP2,TEMP3
      DOUBLEPRECISION P2,C1F,C2F,H1,H2,E1,E2,CPP,P
      DATA    P2,C1F,C2F,H1,H2,E1,E2,CPP,P/0.1D+2,0.75D-1,0.25D-1,
     /        0.8D+4,0.8D+4,0.1D+4,0.1D+4,0.36938503D+1,0.2D+2/
      INTEGER I
      GOTO (1,2,3,4,3),MODE
    1 N=3
      NILI=0
      NINL=9
      NELI=0
      NENL=0
      X(1)=0.5D+4
      X(2)=0.2D+3
      X(3)=0.1D+3
      DO 6 I=1,2
      LXL(I)=.TRUE.
    6 LXU(I)=.TRUE.
      LXL(3)=.FALSE.
      LXU(3)=.FALSE.
      XL(1)=0.1D+4
      XL(2)=0.1D+3
      XU(1)=0.8D+4
      XU(2)=0.5D+3
      LEX=.FALSE.
      NEX=1
      FEX=-0.41489499D+1
      XEX(1)=0.78287954D+4
      XEX(2)=0.18881406D+3
      XEX(3)=0.11381406D+3
      RETURN
    2 P1=0.1D+3
      DO 35 I=1,3
   35 IF(X(I).LT.0.1D-5) X(I)=0.1D-5
      XK1=P1*DEXP(-E1/(0.46D+3+X(2)))
      XK2=P2*DEXP(-E2/(0.46D+3+X(2)))
      V=P*X(1)/(XK2*(X(1)*C2F-P))
      C1=(X(1)*C1F-P)/(X(1)+V*XK1)
      UT=0.43D+2+0.452D-1*X(2)
   39 ARGU=(X(2)-X(3)-0.75D+2)/(X(2)-0.1D+3)
      IF(ARGU.EQ.0.0D+0) GOTO 48
      XLMTD=(0.25D+2-X(3))/DLOG(DABS(ARGU))
      HEAT=X(1)*CPP*(0.1D+3-X(2))+XK1*(X(1)*C1F-P)*V*H1/(X(1)+V*XK1)
     /    +P*H2
      AREA=HEAT/(UT*XLMTD)
      ARE=DABS(AREA)
      HEA=DABS(HEAT)
      DIA=(V/0.1272D+2)**0.33333333
      IF(X(2).LT.0.2D+3) GOTO 40
      PRESS=0.236D+2+0.33D-5*(X(2)**3)
      GOTO 41
   48 X(2)=X(2)*0.10001D+1
      GOTO 39
   40 PRESS=0.5D+2
   41 WATE=(0.909D-1*(DIA**3)+0.482D+0*(DIA**2))*PRESS+0.366D+2*(DIA**2)
     /     +0.1605D+3*DIA
      C1 =0.48D+1*(WATE**0.782)
      IF(X(2).LT.0.2D+3) GOTO 42
      C2=(0.172D+2+0.133D-1*X(2))*DIA**2
      GOTO 43
   42 C2=0.0D+0
   43 IF(PRESS.LT.0.15D+3) GOTO 44
      C3=0.27D+3*(ARE**0.546)*(0.962D+0+0.168D-6*(X(2)**3))
      GOTO 45
   44 C3=0.27D+3*(ARE**0.546)
   45 C4=0.14D+4+0.14D+3*DIA
      C5=0.875D+3*((0.5D-1*V)**0.3)
      C6=0.812D+3*(((0.695D-3+0.459D-10*(X(2)**3))+X(1))**0.467)
      IF(X(2).LT.0.25D+3) GOTO 46
      C7=0.1291D+4*((0.298D+3*HEA/X(3))**0.467)
      GOTO 47
   46 C7=0.812D+3*((0.298D+3*HEA/X(3))**0.467)
   47 COST=C1+C2+C3+C4+C5+C6+C7
      VEST=0.5D+1*COST
      C0=0.22D+5+0.18D+0*VEST+0.31D+1*V+0.611D+2*((0.695D-3+0.459D-10
     F   *(X(2)**3))*X(1))+0.115D-2*HEAT+0.692D+1*HEAT+0.574D+3
     F   *X(1)*(C1F-C1)+0.1148D+6
      FX=(0.688D+6-C0)/(0.2D+1*VEST)*(-0.1D-2)
    3 RETURN
    4 P1=0.1D+3
      DO 37 I=1,3
   37 IF(X(I).LT.0.1D-5) X(I)=0.1D-5
      XK1=P1*DEXP(-E1/(0.46D+3+X(2)))
      XK2=P2*DEXP(-E2/(0.46D+3+X(2)))
      V=P*X(1)/(XK2*(X(1)*C2F-P))
      C1=(X(1)*C1F-P)/(X(1)+V*XK1)
      UT=0.43D+2+0.452D-1*X(2)
   36 ARGU=(X(2)-X(3)-0.75D+2)/(X(2)-0.1D+3)
      IF(ARGU.EQ.0.0D+0) GOTO 49
      XLMTD=(0.25D+2-X(3))/DLOG(DABS(ARGU))
      HEAT=X(1)*CPP*(0.1D+3-X(2))+XK1*(X(1)*C1F-P)*V*H1/(X(1)+V*XK1)
     /     +P*H2
      AREA=HEAT/(UT*XLMTD)
      DIA=(V/0.1272D+2)**0.33333333
      IF(X(2).LT.0.2D+3) GOTO 50
      PRESS=0.236D+2+0.33D-5*(X(2)**3)
      GOTO 51
   49 X(2)=X(2)*0.10001D+1
      GOTO 36
   50 PRESS=0.5D+2
   51 PHI(1)=DIA-0.125D+1
      PHI(2)=0.967D+1-DIA
      PHI(3)=AREA-0.5D+2
      PHI(4)=0.4D+4-AREA
      A11=XK1+X(1)/V
      A12=XK2
      A13=(X(1)*C1F-PRESS)*XK1*E1/((X(1)+V*XK1)*((X(2)+0.46D+3)**2))
     F    +PRESS*E2/(V*((X(2)+0.46D+3)**2))
      A22=XK2+X(1)/V
      A23=PRESS*E2/(V*((X(2)+0.46D+3)**2))
      A31=-H1*XK1/CPP
      A32=-H2*XK2/CPP
      A33=X(1)/V+UT*AREA/(V*CPP)-(X(1)*C1F-PRESS)*XK1*E1*H1/((X(1)+V*XK1
     F    )*CPP*((X(2)+0.46D+3)**2))-PRESS*E2*H2/(V*CPP*((X(2)
     F    +0.46D+3)**2))
      TEMP1=A11+A22+A33
      PHI(5)=TEMP1
      TEMP2=A11*A22+A22*A33+A33*A11-A13*A31-A23*A32
      PHI(6)=TEMP2
      TEMP3=A11*A22*A33+A12*A23*A31-A13*A31*A22-A23*A32*A11
      PHI(7)=TEMP3
      PHI(8)=TEMP1*TEMP2-TEMP3
      PHI(9)=HEAT
      DO 7 I=1,9
    7 IF (INDEX1(I)) G(I)=PHI(I)
      RETURN
      END
