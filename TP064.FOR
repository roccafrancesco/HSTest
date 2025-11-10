      SUBROUTINE TP64(MODE)     
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
      NINL=1    
      NELI=0    
      NENL=0    
      DO 6 I=1,3
      X(I)=1.D0 
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
6     XL(I)=1.D-5       
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.108734717597D+03     
      XEX(2)=0.851261394257D+02     
      XEX(3)=0.204324707858D+03     
      FEX=0.629984242821D+04 
      RETURN    
2     FX=5.D0*X(1)+5.D+4/X(1)+20.D0*X(2)+7.2D+4/X(2)+10.D0*X(3) 
     1     +1.44D+5/X(3)
      RETURN    
3     GF(1)=5.D0-5.D+4/X(1)**2  
      GF(2)=20.D0-7.2D+4/X(2)**2
      GF(3)=10.D0-1.44D+5/X(3)**2       
      RETURN    
4     IF (INDEX1(1)) G(1)=1.D0-4.D0/X(1)-32.D0/X(2)-120.D0/X(3) 
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=4.D0/X(1)**2      
      GG(1,2)=32.D0/X(2)**2     
      GG(1,3)=120.D0/X(3)**2    
7     RETURN    
      END       
