      SUBROUTINE TP394(MODE)
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
    1 N=20
      NILI=0
      NINL=0
      NELI=0
      NENL=1
      DO 6 I=1,20
      X(I)=2.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=1.9166667D+0
      XEX(1)=0.91287160D+0
      XEX(2)=0.40824680D+0
      XEX(3)=-0.16746493D-4
      XEX(4)=-0.54074613D-5
      XEX(5)=0.19606096D-5
      XEX(6)=-0.88626385D-5
      XEX(7)=0.81697576D-5
      XEX(8)=-0.14386551D-4
      XEX(9)=0.21831200D-4
      XEX(10)=-0.13873341D-4
      XEX(11)=0.13498048D-4
      XEX(12)=-0.39814429D-5
      XEX(13)=-0.11023953D-4
      XEX(14)=-0.12809830D-4
      XEX(15)=0.79408513D-5
      XEX(16)=0.20458900D-4
      XEX(17)=0.45644559D-5
      XEX(18)=-0.94429887D-5
      XEX(19)=-0.10142804D-4
      XEX(20)=-0.13788343D-5
      RETURN
    2 FX=0.D+0
      DO 8 I=1,20
    8 FX=FX+DBLE(I)*(X(I)**2+X(I)**4)
      RETURN
    3 DO 9 I=1,20
    9 GF(I)=DBLE(I)*(2.D+0*X(I)+4.D+0*X(I)**3)
      RETURN
    4 IF(.NOT.INDEX1(1))GOTO 10
      G(1)=0.D+0
      DO 11 I=1,20
   11 G(1)=G(1)+X(I)**2
      G(1)=G(1)-1.D+0
   10 RETURN
    5 IF(.NOT.INDEX2(1))GOTO 12
      DO 13 I=1,20
   13 GG(1,I)=2.D+0*X(I)
   12 RETURN
      END
