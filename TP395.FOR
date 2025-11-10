      SUBROUTINE TP395(MODE)
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
    1 N=50
      NILI=0
      NINL=0
      NELI=0
      NENL=1
      DO 6 I=1,50
      X(I)=2.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=0.19166668D+1
      XEX(1)=0.91285206D+0
      XEX(2)=0.40829045D+0
      XEX(3)=-0.64969989D-5
      XEX(4)=-0.99096716D-4
      XEX(5)=0.11891290D-3
      XEX(6)=-0.46486687D-4
      XEX(7)=0.57605078D-4
      XEX(8)=-0.48016383D-4
      XEX(9)=0.25691371D-4
      XEX(10)=0.11670144D-4
      XEX(11)=-0.30881321D-4
      XEX(12)=0.87202482D-5
      XEX(13)=0.19980370D-4
      XEX(14)=-0.12338706D-4
      XEX(15)=-0.16390153D-4
      XEX(16)=0.73383634D-5
      XEX(17)=0.16862980D-4
      XEX(18)=0.43922807D-5
      XEX(19)=-0.58623189D-5
      XEX(20)=-0.25188987D-5
      XEX(21)=0.45980202D-5
      XEX(22)=0.32507205D-5
      XEX(23)=-0.66596023D-5
      XEX(24)=-0.14419491D-4
      XEX(25)=-0.12164937D-4
      XEX(26)=-0.39129061D-5
      XEX(27)=0.98985037D-6
      XEX(28)=0.14776535D-6
      XEX(29)=-0.68312704D-6
      XEX(30)=0.24242977D-5
      XEX(31)=0.53892372D-5
      XEX(32)=0.26662956D-5
      XEX(33)=-0.29282090D-5
      XEX(34)=-0.38338271D-5
      XEX(35)=0.61198364D-6
      XEX(36)=0.43671860D-5
      XEX(37)=0.41104627D-5
      XEX(38)=0.14549012D-5
      XEX(39)=-0.12562117D-5
      XEX(40)=-0.30092086D-5
      XEX(41)=-0.38620459D-5
      XEX(42)=-0.42627256D-5
      XEX(43)=-0.45080325D-5
      XEX(44)=-0.44852099D-5
      XEX(45)=-0.37953194D-5
      XEX(46)=-0.23440318D-5
      XEX(47)=-0.74816106D-6
      XEX(48)=-0.54626804D-7
      XEX(49)=-0.10972677D-5
      XEX(50)=-0.21312770D-5
      RETURN
    2 FX=0.D+0
      DO 8 I=1,50
    8 FX=FX+DBLE(I)*(X(I)**2+X(I)**4)
      RETURN
    3 DO 9 I=1,50
      GF(I)=DBLE(I)*(2.D+0*X(I)+4.D+0*X(I)**3)
    9 GF(I)=GF(I)
      RETURN
    4 IF(.NOT.INDEX1(1))GOTO 10
      G(1)=0.D+0
      DO 11 I=1,50
   11 G(1)=G(1)+X(I)**2
      G(1)=G(1)-1.D+0
   10 RETURN
    5 IF(.NOT.INDEX2(1))GOTO 12
      DO 13 I=1,50
   13 GG(1,I)=2.D+0*X(I)
   12 RETURN
      END
