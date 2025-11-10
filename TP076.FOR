      SUBROUTINE TP76(MODE)     
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
      NILI=3    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 6 I=1,4
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
      X(I)=0.5D0
6     XL(I)=0.D0
      GG(1,1)=-1.D0     
      GG(1,2)=-2.D0     
      GG(1,3)=-1.D0     
      GG(1,4)=-1.D0     
      GG(2,1)=-3.D0     
      GG(2,2)=-1.D0     
      GG(2,3)=-2.D0     
      GG(2,4)=1.D0      
      GG(3,1)=0.D0      
      GG(3,2)=1.D0      
      GG(3,3)=4.D0      
      GG(3,4)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.272727272717D+00     
      XEX(2)=0.209090909094D+01     
      XEX(3)=-0.263371889808D-10     
      XEX(4)=0.545454545496D+00     
      FEX=-0.468181818182D+01 
      RETURN    
2     FX=X(1)**2+X(3)**2+0.5D0*(X(2)**2+X(4)**2)-X(1)*X(3)+X(3)*X(4)-   
     -X(1)-3.D0*X(2)+X(3)-X(4)  
      RETURN    
3     GF(1)=2.D0*X(1)-X(3)-1.D0 
      GF(2)=X(2)-3.D0   
      GF(3)=2.D0*X(3)-X(1)+X(4)+1.D0    
      GF(4)=X(4)+X(3)-1.D0      
      RETURN    
4     IF (INDEX1(1)) G(1)=-X(1)-2.D0*X(2)-X(3)-X(4)+5.D0
      IF (INDEX1(2)) G(2)=-3.D0*X(1)-X(2)-2.D0*X(3)+X(4)+4.D0   
      IF (INDEX1(3)) G(3)=X(2)+4.D0*X(3)-1.5D0  
5     RETURN    
      END       
