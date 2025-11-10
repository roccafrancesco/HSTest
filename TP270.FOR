      SUBROUTINE TP270(MODE) 
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
      GOTO (1,2,3,4,5),MODE 
 1    N=5 
      NILI=0 
      NINL=1 
      NELI=0 
      NENL=0 
      DO 6 I=1,4 
      X(I)=DBLE(I)+0.1D+0 
      XEX(I)=DBLE(I) 
      LXL(I)=.TRUE. 
      LXU(I)=.FALSE. 
 6    XL(I)=DBLE(I) 
      LXL(5)=.FALSE. 
      LXU(5)=.FALSE. 
      X(5)=-0.1D+1 
      XEX(5)=0.2D+1 
      NEX=1 
      LEX=.TRUE. 
      FEX= -0.1D+1 
      RETURN 
 2    FX=X(1)*X(2)*X(3)*X(4)-3.D+0*X(1)*X(2)*X(4)
     F -4.D+0*X(1)*X(2)*X(3)+12.D+0*X(1)*X(2)-X(2)*X(3)*X(4)
     F +3.D+0*X(2)*X(4)+4.D+0*X(2)*X(3)-12.D+0*X(2)
     F -2.D+0*X(1)*X(3)*X(4) +6.D+0*X(1)*X(4)+8.D+0*X(1)*X(3)
     F -24.D+0*X(1)+2.D+0*X(3)*X(4)-6.D+0*X(4)-8.D+0*X(3)
     F +24.D+0+1.5D+0*X(5)**4-5.75D+0*X(5)**3+5.25D+0*X(5)**2
      RETURN 
 3    GF(1)=X(2)*X(3)*X(4)-3.D+0*X(2)*X(4)-4.D+0*X(2)*X(3)   
     F +12.D+0*X(2)-2.D+0*X(3)*X(4)+6.D+0*X(4)+8.D+0*X(3)-24.D+0  
      GF(2)=X(1)*X(3)*X(4)-3.D+0*X(1)*X(4)-4.D+0*X(1)*X(3) 
     F +12.D+0*X(1)-X(3)*X(4)+3.D+0*X(4)+4.D+0*X(3)-12.D+0
      GF(3)=X(1)*X(2)*X(4)-4.D+0*X(1)*X(2)-X(2)*X(4)+4.D+0*X(2)
     F -2.D+0*X(1)*X(4)+8.D+0*X(1)+2.D+0*X(4)-8.D+0  
      GF(4)=X(1)*X(2)*X(3)-3.D+0*X(1)*X(2)-X(2)*X(3)+3.D+0*X(2)
     F -2.D+0*X(1)*X(3)+6.D+0*X(1)+2.D+0*X(3)-6.D+0 
      GF(5)=10.5D+0*X(5)-17.25D+0*X(5)**2+6.D+0*X(5)**3 
      RETURN 
 4    IF (INDEX1(1)) G(1)=34.D+0-X(1)**2-X(2)**2-X(3)**2
     F             -X(4)**2-X(5)**2 
      RETURN 
 5    IF (.NOT.INDEX2(1)) GOTO 7 
      DO 8 I=1,5 
 8    GG(1,I)=-0.2D+1*X(I) 
 7    RETURN 
      END 
