      SUBROUTINE TP204(MODE)
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
      DOUBLEPRECISION PROD,A(3),D(3),H(3,2),F,DF
      DATA A/0.13294D+0,-0.244378D+0,0.325895D+0/
      DATA D/2.5074D+0,-1.36401D+0,1.02282D+0/
      DATA H/-0.564255D+0,-0.404979D+0,-0.0735084D+0,0.392417D+0,
     1       0.927589D+0,0.535493D+0/
C      DATA B/5.66598D+0,2.77141D+0,2.77141D+01,2.12413D+0/
      INTEGER I
      GOTO(1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0 
      NENL=0
      X(1)=0.1D+0
      X(2)=0.1D+0
      DO 6 I=1,2 
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.183601D+0
      XEX(1)=0.D+0
      XEX(2)=0.D+0
      RETURN
    2 DO 10 I=1,3
      PROD=H(I,1)*X(1)+H(I,2)*X(2)
   10 F(I)=A(I)+PROD+.5D+0*PROD**2*D(I)
      FX=F(1)**2+F(2)**2+F(3)**2
      RETURN
    3 DO 11 I=1,3
      PROD=H(I,1)*X(1)+H(I,2)*X(2)
   11 F(I)=A(I)+PROD+.5D+0*PROD**2*D(I)
      DO 7 I=1,3 
    7 DF(I,1)=H(I,1)+(H(I,1)*X(1)+H(I,2)*X(2))*H(I,1)*D(I)
      DO 8 I=1,3
    8 DF(I,2)=H(I,2)+(H(I,1)*X(1)+H(I,2)*X(2))*H(I,2)*D(I)
      DO 9 I=1,2
    9 GF(I)=2.D+0*(F(1)*DF(1,I)+F(2)*DF(2,I)+F(3)*DF(3,I))
    4 RETURN
      END
