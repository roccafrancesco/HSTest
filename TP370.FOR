      SUBROUTINE TP370(MODE)
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
     /           /L15/ F(200)    
     /           /L16/ DF(200,200)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I,J,LSUM
      DOUBLEPRECISION F,DF,SUM,SUM1,SUM2(31),BASIS
      N=6
      FEX=.228767005355D-2
      XEX(1)=-0.15663881D-01
      XEX(2)=0.10124222D+01
      XEX(3)=-0.23290211D+00
      XEX(4)=0.12604426D+01
      XEX(5)=-0.15138534D+01
      XEX(6)=0.99314584D+00
      GOTO 10
      ENTRY TP371(MODE)
      N=9
C      FEX=0.1399766D-5
      FEX=0.13997601D-5
      XEX(1)=-0.10630204D-03
      XEX(2)=0.99902770D+00
      XEX(3)=0.30998203D-01 
      XEX(4)=0.61696369D-01
      XEX(5)=0.11466839D+01
      XEX(6)=-0.25891812D+01
      XEX(7)=0.37452496D+01
      XEX(8)=-0.27632595D+01
      XEX(9)=0.92600597D+00
   10 GOTO (1,2,4,4,4),MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 11 I=1,N
      X(I)=0.D+0
      LXL(I)=.FALSE.
   11 LXU(I)=.FALSE.
      LSUM=31
      LEX=.FALSE.
      NEX=1
      RETURN
    2 F(1)=X(1)
      F(2)=X(2)-X(1)**2-1.D+0
      DO 20 I=2,30
      BASIS=DBLE(I-1)/29.D+0
      SUM1=0.D+0
      DO 21 J=2,N
   21 SUM1=SUM1+X(J)*DBLE(J-1)*BASIS**(J-2)
      SUM=0.D+0
      DO 23 J=1,N
   23 SUM=SUM+X(J)*BASIS**(J-1)
      SUM2(I+1)=SUM
   20 F(I+1)=SUM1-SUM2(I+1)**2-1.D+0
      FX=0.D+0
      DO 22 I=1,31
   22 FX=FX+F(I)**2
    4 RETURN
      END
