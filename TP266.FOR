      SUBROUTINE TP266(MODE) 
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
     /           /L15/ F(200)    
     /           /L16/ DF(200,200)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I,K,L
      DOUBLEPRECISION F,DF,A(10),D(10),C(10,5),B(5,5),HF 
      DATA A /0.426149D-1,0.352053D-1,0.878058D-1,0.330812D-1,
     /        0.580924D-1,
     F        0.649704D0,0.344144D0,-0.627443D0,0.1828D-2,-0.224783D0/
      DATA D /0.234659D+1,0.284048D+1,0.113888D+1,0.302286D+1,
     F        0.172139D+1,0.153917D+0,0.290577D+0,-0.159378D+0,
     F        0.546910D+2,-0.444873D+0/
      DATA C /-0.564255D+0,0.535493D+0,0.586387D+0,0.608734D+0,
     /    0.774227D+0,
     F   -0.435033D+0,0.759468D+0,-0.152448D+0,-0.821772D+0,0.819831D+0,
     F   .0392417D+0,0.658799D+0,0.289826D+0,0.984915D+0,0.325421D+0,
     F   -0.688583D+0,-0.627795D+0,-0.546437D+0,-0.53412D0,-0.910632D0,
     F   -0.404979D0,-0.0636666D0,0.854402D0,0.375699D0,-0.151719D0,
     F   .0222278D+0,0.0403142D+0,0.484134D+0,-0.798498D+0,-0.480344D+0,
     F   0.927589D+0,-0.681091D+0,0.789312D+0,0.239547D+0,0.448051D+0,
     F   -0.524653D+0,0.724666D+0,0.353951D+0,-0.658572D+0,-0.871758D+0,
     F   -0.0735083D+0,-0.869487D+0,0.949721D+0,0.463136D+0,0.149926D+0,
     F   0.413248D+0,-0.0182537D+0,0.887866D+0,0.662362D+0,-0.978666D+0/
      DATA B /.354033D+0,-0.0230349D+0,-0.211938D+0,-0.0554288D+0,
     F   0.220429D+0,-0.0230349D+0,0.29135D+0,-0.00180333D0,-0.111141D0,
     F   0.0485461D+0,-0.211938D0,-0.00180333D0,-0.815808D0,-0.133538D0,
     F   -0.38067D+0,-0.0554288D+0,-0.111141D+0,-0.133538D+0,0.389198D0,
     F   -0.131586D+0,0.220429D+0,0.0485461D+0,-0.38067D+0,-0.131586D0, 
     F   0.534706D+0/
      GOTO (1,2,2,4,4),MODE 
 1    N=5 
      NILI=0 
      NINL=0 
      NELI=0 
      NENL=0 
      DO 6 I=1,5 
      X(I)=0.1D+0 
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
      XL(I)=0.0D0
 6    XEX(I)=0.D+0 
      XEX(3)=0.29297857D-1
      NEX=1 
      LEX=.TRUE.
      NEX=1
      FEX= 0.99597447D+0 
      RETURN 
 2    DO 20 I=1,10
      CALL TP266A(A(I),B,C,D(I),I,X,F(I))
 20   CONTINUE
      IF (MODE.EQ.3) GOTO 3 
      FX=0.D+0 
      DO 7 I=1,10 
 7    FX=FX+F(I)**2
      RETURN 
 3    DO 8 K=1,5 
      GF(K)=0.D+0 
      DO 8 I=1,10 
      HF=0.D+0 
      DO 10 L=1,5 
10    HF=HF+(B(K,L)+B(L,K))*X(L) 
      DF(I,K)=C(I,K)+.5D+0*D(I)*HF 
      GF(K)=GF(K)+0.2D+1*F(I)*DF(I,K) 
 8    CONTINUE
 4    RETURN 
      END 
