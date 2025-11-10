      SUBROUTINE TP74(MODE)     
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
      COMMON     /L74/ A(2)    
      DOUBLEPRECISION A,V1,V2  
      INTEGER I,KN1
      XEX(1)=0.679945319802D+03     
      XEX(2)=0.102606713256D+04     
      XEX(3)=0.118876364490D+00     
      XEX(4)=-0.396233553180D+00     
      FEX=0.512649810934D+04 
      KN1=1     
      GOTO 7    
      ENTRY TP75(MODE)  
      KN1=2     
      XEX(1)=0.776159220293D+03     
      XEX(2)=0.925194939196D+03     
      XEX(3)=0.511087936804D-01     
      XEX(4)=-0.428891137432D+00     
      FEX=0.517441288686D+04 
7     GOTO (1,2,3,4,5),MODE    
1     N=4       
      NILI=2    
      NINL=0    
      NELI=0    
      NENL=3    
      A(1)=0.55D0       
      A(2)=0.48D0       
      DO 6 I=1,4
      X(I)=0.D0 
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=0.D0
      XL(2)=0.D0
      XL(3)=-A(KN1)     
      XL(4)=-A(KN1)     
      XU(1)=1.2D+3      
      XU(2)=1.2D+3      
      XU(3)=A(KN1)      
      XU(4)=A(KN1)      
      GF(3)=0.D0
      GF(4)=0.D0
      GG(1,1)=0.D0      
      GG(1,2)=0.D0      
      GG(1,3)=-1.D0     
      GG(1,4)=1.D0      
      GG(2,1)=0.D0      
      GG(2,2)=0.D0      
      GG(2,3)=1.D0      
      GG(2,4)=-1.D0     
      GG(3,1)=-1.D0     
      GG(3,2)=0.D0      
      GG(4,1)=0.D0      
      GG(4,2)=-1.D0     
      GG(5,1)=0.D0      
      GG(5,2)=0.D0      
      LEX=.FALSE.
      NEX=0       
      RETURN    
2     FX=3.D0*X(1)+X(1)**3*1.D-6+2.D0*X(2)+2.D0*1.D-6/3.D0*X(2)**3      
      RETURN    
3     GF(1)=3.D0+3.D-6*X(1)**2  
      GF(2)=2.D0+2.D-6*X(2)**2  
      RETURN    
4     IF (INDEX1(1)) G(1)=X(4)-X(3)+A(KN1)      
      IF (INDEX1(2)) G(2)=X(3)-X(4)+A(KN1)      
      IF (INDEX1(3)) G(3)=1.D+3*(DSIN(-X(3)-0.25D0)+DSIN(-X(4)-0.25D0))+
     -894.8D0-X(1)      
      IF (INDEX1(4)) G(4)=1.D+3*(DSIN(X(3)-0.25D0)+DSIN(X(3)-X(4)       
     --0.25D0))+
     -894.8D0-X(2)      
      IF(INDEX1(5)) G(5)=1.D+3*(DSIN(X(4)-0.25D0)+DSIN(X(4)-X(3)
     --0.25D0))+
     -1.2948D+3 
      RETURN    
5     IF (.NOT.INDEX2(3)) GOTO 9
      GG(3,3)=-1.D+3*DCOS(-X(3)-0.25D0) 
      GG(3,4)=-1.D+3*DCOS(-X(4)-0.25D0) 
9     IF (.NOT.INDEX2(4)) GOTO 10       
      V1=DCOS(X(3)-X(4)-0.25D0) 
      GG(4,3)=1.D+3*(DCOS(X(3)-0.25D0)+V1)      
      GG(4,4)=-1.D+3*V1 
10    IF (.NOT.INDEX2(5)) GOTO 11       
      V2=DCOS(X(4)-X(3)-0.25D0) 
      GG(5,3)=-1.D+3*V2 
      GG(5,4)=1.D+3*(DCOS(X(4)-0.25D0)+V2)      
11    RETURN    
      END       
