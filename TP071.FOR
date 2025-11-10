      SUBROUTINE TP71(MODE)     
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
      NINL=1    
      NELI=0    
      NENL=1    
      DO 6 I=1,4
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=1.D0
6     XU(I)=5.D0
      X(1)=1.D0 
      X(2)=5.D0 
      X(3)=5.D0 
      X(4)=1.D0 
      XEX(1)=0.100000000000D+01     
      XEX(2)=0.474299937545D+01     
      XEX(3)=0.382115032617D+01     
      XEX(4)=0.137940824585D+01     
      FEX=0.170140172895D+02 
      LEX=.FALSE.       
      NEX=0
      RETURN    
2     FX=X(1)*X(4)*(X(1)+X(2)+X(3))+X(3)
      RETURN    
3     GF(1)=X(4)*(2.D0*X(1)+X(2)+X(3))  
      GF(2)=X(1)*X(4)   
      GF(3)=GF(2)+1.D0  
      GF(4)=X(1)*(X(1)+X(2)+X(3))       
      RETURN    
4     IF (INDEX1(1)) G(1)=(X(1)*X(2)*X(3)*X(4)-25.D0)/25.0D0
      IF (INDEX1(2)) G(2)=(X(1)**2+X(2)**2+X(3)**2+X(4)**2-40.D0)/40.0D0
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=X(2)*X(3)*X(4)/25.0D0    
      GG(1,2)=X(1)*X(3)*X(4)/25.0D0    
      GG(1,3)=X(1)*X(2)*X(4)/25.0D0    
      GG(1,4)=X(1)*X(2)*X(3)/25.0D0    
7     IF (.NOT.INDEX2(2)) GOTO 8
      DO 20 I=1,4       
20    GG(2,I)=2.D0*X(I)/40.0D0  
8     RETURN    
      END       
