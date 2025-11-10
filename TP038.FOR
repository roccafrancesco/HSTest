      SUBROUTINE TP38(MODE)     
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
      GOTO (1,2,3,4,4),MODE     
1     N=4       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=0    
      X(1)=-3.D0
      X(2)=-1.D0
      X(3)=-3.D0
      X(4)=-1.D0
      DO 6 I=1,4
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=-10.D0      
6     XU(I)=10.D0       
      LEX=.TRUE.
      NEX=1     
      DO 30 I=1,4       
30    XEX(I)=1.D0       
      FEX=0.D0  
      RETURN    
2     FX=(100.D0*(X(2)-X(1)**2)**2+(1.D0-X(1))**2
     /   + 90.D0*(X(4)-X(3)**2)**2
     /   + (1.D0-X(3))**2+10.1D0*((X(2)-1.D0)**2+(X(4)-1.D0)**2)
     /   + 19.8D0*(X(2)-1.D0)*(X(4)-1.D0))
      RETURN    
3     GF(1)=(-400.D0*X(1)*(X(2)-X(1)**2)-2.D0*(1.D0-X(1)))
      GF(2)=(200.D0*(X(2)-X(1)**2)+20.2D0*(X(2)-1.D0)+19.8D0
     /                 *(X(4)-1.D0))
      GF(3)=(-360.D0*X(3)*(X(4)-X(3)**2)-2.D0*(1.D0-X(3)))
      GF(4)=(180.D0*(X(4)-X(3)**2)+20.2D0*(X(4)-1.D0)+19.8D0
     /                 *(X(2)-1.D0))
4     RETURN    
      END       
