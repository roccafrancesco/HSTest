      SUBROUTINE TP79(MODE)     
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
      DOUBLEPRECISION V1,V2,V3,V4
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=3    
      DO 6 I=1,5
      X(I)=2.D0 
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      GG(1,1)=1.D0      
      GG(1,4)=0.D0      
      GG(1,5)=0.D0      
      GG(2,1)=0.D0      
      GG(2,2)=1.D0      
      GG(2,4)=1.D0      
      GG(2,5)=0.D0      
      GG(3,2)=0.D0      
      GG(3,3)=0.D0      
      GG(3,4)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.119112745626D+01     
      XEX(2)=0.136260316492D+01     
      XEX(3)=0.147281793150D+01     
      XEX(4)=0.163501661894D+01     
      XEX(5)=0.167908143619D+01     
      FEX=0.787768208538D-01 
      RETURN    
2     FX=(X(1)-1.D0)**2+(X(1)-X(2))**2+(X(2)-X(3))**2+(X(3)-X(4))**4    
     1     +(X(4)-X(5))**4      
      RETURN    
3     V1=X(1)-X(2)      
      V2=X(2)-X(3)      
      V3=X(3)-X(4)      
      V4=X(4)-X(5)      
      GF(1)=2.D0*(X(1)-1.D0+V1) 
      GF(2)=2.D0*(V2-V1)
      GF(3)=-2.D0*V2+4.D0*V3**3 
      GF(4)=4.D0*(V4**3-V3**3)  
      GF(5)=-4.D0*V4**3 
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)+X(2)**2+X(3)**3-2.D0-3.D0*DSQRT(2.D0)    
      IF (INDEX1(2)) G(2)=X(2)-X(3)**2+X(4)+2.D0-2.D0*DSQRT(2.D0)       
      IF (INDEX1(3)) G(3)=X(1)*X(5)-2.D0
      RETURN    
5     IF(.NOT.INDEX2(1)) GOTO 7 
      GG(1,2)=2.D0*X(2) 
      GG(1,3)=3.D0*X(3)**2      
7     IF (INDEX2(2)) GG(2,3)=-2.D0*X(3) 
      IF (.NOT.INDEX2(3)) GOTO 9
      GG(3,1)=X(5)      
      GG(3,5)=X(1)      
9     RETURN    
      END       
