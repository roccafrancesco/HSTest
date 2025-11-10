      SUBROUTINE TP72(MODE)     
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
1     N=4       
      NILI=0    
      NINL=2    
      NELI=0    
      NENL=0    
      DO 6 I=1,4
      X(I)=1.D0 
      XL(I)=1.D-3       
      XU(I)=1.D+5*(5.D0-DBLE(I))      
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      DO 30 I=1,4       
30    GF(I)=1.D0
      LEX=.FALSE.
      NEX=1       
      XEX(1)=0.193407050141D+03     
      XEX(2)=0.179547504555D+03     
      XEX(3)=0.185018587841D+03     
      XEX(4)=0.168706233485D+03     
C      FEX=0.727679376021D+03 
      FEX=0.72767936D+3
      RETURN    
2     FX=1.D0+X(1)+X(2)+X(3)+X(4)       
3     RETURN    
4     IF (INDEX1(1)) G(1)=-4.D0/X(1)-2.25D0/X(2)-1.D0/X(3)-0.25D0/X(4)  
     1     +0.0401D0    
      IF (INDEX1(2)) G(2)=-0.16D0/X(1)-0.36D0/X(2)-0.64D0*(1.D0/X(3)    
     1     +1.D0/X(4))+0.010085D0       
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=4.D0/X(1)**2      
      GG(1,2)=2.25D0/X(2)**2    
      GG(1,3)=1.D0/X(3)**2      
      GG(1,4)=0.25D0/X(4)**2    
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=0.16D0/X(1)**2    
      GG(2,2)=0.36D0/X(2)**2    
      GG(2,3)=0.64D0/X(3)**2    
      GG(2,4)=0.64D0/X(4)**2    
8     RETURN    
      END       
