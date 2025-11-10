      SUBROUTINE TP63(MODE)     
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
1     N=3       
      NILI=0    
      NINL=0    
      NELI=1    
      NENL=1    
      DO 6 I=1,3
      X(I)=2.D0 
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
6     XL(I)=0.D0
      GG(1,1)=8.D0      
      GG(1,2)=14.D0     
      GG(1,3)=7.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.351211841492D+01     
      XEX(2)=0.216988174172D+00     
      XEX(3)=0.355217403459D+01     
      FEX=0.961715172127D+03 
      RETURN    
2     FX=1.D+3-X(1)**2-2.D0*X(2)**2-X(3)**2-X(1)*X(2)-X(1)*X(3) 
      RETURN    
3     GF(1)=-2.D0*X(1)-X(2)-X(3)
      GF(2)=-4.D0*X(2)-X(1)     
      GF(3)=-2.D0*X(3)-X(1)     
      RETURN    
4     IF (INDEX1(1)) G(1)=8.D0*X(1)+14.D0*X(2)+7.D0*X(3)-56.D0  
      IF (INDEX1(2)) G(2)=X(1)**2+X(2)**2+X(3)**2-25.D0 
      RETURN    
5     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=2.D0*X(1) 
      GG(2,2)=2.D0*X(2) 
      GG(2,3)=2.D0*X(3) 
8     RETURN    
      END       
