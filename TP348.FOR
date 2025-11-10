      SUBROUTINE TP348 (MODE)
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
      COMMON     /D348/AF,AT,AC,GI,RE,XMDOT,DELP,H1,COSTM,COSTT,COSTF,
     /       HO,XVAL,ETAF,ETAS,HEF,Q
      DOUBLEPRECISION AF,AT,AC,GI,RE,XMDOT,DELP,H1,COSTM,COSTT,COSTF,
     /       HO,XVAL,ETAF,ETAS,HEF,Q,XX
      DOUBLEPRECISION RHO,XMU,CP,PR,PI,D,TIN,TSURF,H,W,RHOC,RHOA
      DATA RHO,XMU,CP,PR,PI,D,TIN,TSURF,H,W,RHOC,RHOA/.747D-1,
     / .443D-1,.240D+0,.709D+0,3.14159D+0,.525D+0,75.D+0,45.D+0,
     / 13.13D+0,3.166D+0,559.D+0,169.D+0/  
      INTEGER I
      GOTO (1,2,3,4,5),MODE
 1    N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=1
      DO 6 I=1,3	   
      LXL(I)=.FALSE.
 6    LXU(I)=.TRUE.
      LXL(2)=.TRUE.
      XL(2)=.1313D+2
      XU(1)=.44D-1
      XU(2)=.24D+2
      XU(3)=.6D+3
      X(1)=.04D0
      X(2)=.18D+2
      X(3)=.144D+3
      LEX=.FALSE.
      NEX=1
      XEX(1)=0.44D-01
      XEX(2)=0.24D+02
      XEX(3)=0.85607576D+02
      FEX=0.36970840D+02
      RETURN
2     CONTINUE
      IF (X(1).LT.XL(1)) X(2)=XL(1)
      IF (X(2).LT.XL(2)) X(2)=XL(2)
      IF (X(3).LT.XL(3)) X(2)=XL(3)
      AF=X(2)/X(1)*0.2D+1*(W*H-0.3D+2*PI*D**2/.4D+1)/0.144D+3
      AT=0.3D+2*PI*D*X(2)/0.144D+3
      AC=(H*X(2)-0.1D+2*D*X(2)-X(2)/X(1)*0.6D-2*H)/0.144D+3
      IF (AC.EQ.0.0D0) AC=1.0D-20
      GI=(RHO*X(3)*(H*X(2))/(AC*0.144D+3))*0.6D+2
      RE=GI*.1083D+1/(.12D+2*XMU)
      IF (RE.LT..1D-9) RE=.1D-9
      HO=(.195D+0*GI*CP)/(PR**.67*RE**.35)
      XMDOT=RHO*X(3)*H*X(2)/0.144D+3*0.6D+2
      DELP=0.1833D-5/RHO*GI**2*0.3D+1*(AF/AC*RE**(-0.5)+0.1D+0*AT/AC)
      IF (HO.LT..1D-9) HO=.1D-9
      XVAL=.732D-1*DSQRT(HO)
      ETAF=DTANH(XVAL)/XVAL
      ETAS=.1D+1-AF/(AF+AT)*(0.1D+1-ETAF)
      XX=XMDOT*CP
      HEF=0.1D+1-DEXP(DMAX1(-ETAS*HO*(AF+AT)/XX,-1.0D2))
      Q=HEF*(TIN-TSURF)*XMDOT*CP
      IF (MODE .EQ. 4) GOTO 7
      H1=DELP/RHO*XMDOT/.198D+7
      IF (H1.LT.0.1D-9) H1=.1D-9
      COSTM=DSQRT(H1)/.718D-1+.4D+1
      COSTT=.101D+1*.3D+2*X(2)*PI/.4D+1*(D**2-(D-.36D-1)**2)
      COSTF=.47D+0*H*W*.6D-2*RHOA/.1728D+4*X(2)/X(1)
      COSTT=COSTT*RHOC/.1728D+4
      FX=COSTM+COSTT+COSTF
 3    RETURN
 4    IF (.NOT.INDEX1(1)) GOTO 5
      GOTO 2
 7    G(1)=6.D+3-Q
 5    RETURN
      END
