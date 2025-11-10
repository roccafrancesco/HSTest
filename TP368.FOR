      SUBROUTINE TP368(MODE)
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
      INTEGER I
      COMMON     /D368/S2,S3,S4
      DOUBLEPRECISION S2,S3,S4
      GOTO (1,2,2,4,4), MODE
    1 N=8
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 11 I=1,8
      X(I)=1.0-1.0/DBLE(I)
      LXL(I)=.TRUE.
      LXU(I)=.TRUE.
      XL(I)=0.D+0
   11 XU(I)=1.D+0
      X(7)=0.7D0
      X(8)=0.7D0
      NEX=3
      LEX=.TRUE.
      XEX(1)=1.0D+0
      XEX(2)=0.5D+0
      XEX(3)=0.5D+0
      XEX(4)=1.0D+0
      XEX(5)=1.0D+0
      XEX(6)=1.0D+0
      XEX(7)=0.5D+0
      XEX(8)=0.5D+0     
      FEX=-0.74997564D+0
c      XEX(9)=0.49834105D+0
c      XEX(10)=0.49977950D+0
c      XEX(11)=0.50201378D+0
c      XEX(12)=0.50378302D+0
c      XEX(13)=0.50263008D+0
c      XEX(14)=0.50232579D+0
c      XEX(15)=0.10000000D+1
c      XEX(16)=0.10000000D+1        
c      FEX=-0.75
c      XEX(17)=0.5
c      XEX(18)=0.5
c      XEX(19)=0.5
c      XEX(20)=0.5
c      XEX(21)=0.5
c      XEX(22)=0.5
c      XEX(23)=0.10000000D+1
c      XEX(24)=0.10000000D+1
c      FEX=-1.0D0
      RETURN
    2 S2=0.D+0    
      S3=0.D+0
      S4=0.D+0
      DO 10 I=1,8
      S2=S2+X(I)**2
      S3=S3+X(I)**3     
   10 S4=S4+X(I)**4
      IF (MODE .EQ.3) GOTO 3
      FX=-S2*S4+S3**2
      RETURN
    3 DO 31 I=1,8
   31 GF(I)=-2.D+0*X(I)*S4-4.D+0*X(I)**3*S2+6.D+0*X(I)**2*S3
    4 RETURN
      END
