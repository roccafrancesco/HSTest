      SUBROUTINE TP380(MODE)
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
      DOUBLEPRECISION A(11),TEMP,C(30)
      DATA A/-0.00133172D+0,-0.002270927D+0,-0.00248546D+0,
     1 -0.467D+1,-0.4671973D+1,-0.00814D+0,-0.008092D+0,
     2 -0.005D+0,-0.000909D+0,-0.00088D+0,-0.00119D+0/
      DATA C/5.367373D-2,2.1863746D-2,9.7733533D-2,6.6940803D-3,
     1 1.0D-6,1.0D-5,1.0D-6,1.0D-10,1.0D-8,1.0D-2,1.0D-4,
     2 1.0898645D-1,1.6108052D-4,1.0D-23,1.9304541D-6,1.0D-3,
     3 1.0D-6,1.0D-5,1.0D-6,1.0D-9,1.0D-9,1.0D-3,1.0D-3,
     4 1.0898645D-1,1.6108052D-5,1.0D-23,1.9304541D-8,1.0D-5,
     5 1.1184059D-4,1.0D-4/
      GOTO (1,2,3,4,5),MODE
    1 N=12
      NILI=0
      NINL=3
      NELI=0
      NENL=0
      DO 6 I=1,12
      X(I)=0.4D+1
      LXU(I)=.TRUE.
      LXL(I)=.TRUE.
      XL(I)=.1D+0
    6 XU(I)=100.D+0
      GG(1,1)=-C(1)   
      GG(1,2)=-C(2)
      GG(1,3)=-C(3)
      DO 7 I=6,12
    7 GG(1,I)=0.D+0
      GG(2,1)=-C(5)
      GG(2,3)=-C(7)
      GG(2,8)=0.D+0
      GG(2,9)=0.D+0
      GG(2,11)=0.D+0
      GG(3,3)=-C(19)
      GG(3,6)=-C(22)
      GG(3,7)=0.D+0
      GG(3,8)=-C(23)
      GG(3,10)=0.D+0
      GG(3,11)=-C(30)
      GG(3,12)=0.D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.31682215D+01
c      FEX=FEX*1.0D-6
      XEX(1)=0.26631947068D+1
      XEX(2)=0.4517277762D+1
      XEX(3)=0.7133802907D+1
      XEX(4)=0.2237268448D+1
      XEX(5)=0.407840382657D+1
      XEX(6)=0.131827569D+1
      XEX(7)=0.4125187034D+1
      XEX(8)=0.2856195978D+1
      XEX(9)=0.16765929748D+1
      XEX(10)=0.21789111052D+1
      XEX(11)=0.512343515D+1
      XEX(12)=0.6659338016D+1
      RETURN
c    2 FX=0.1D+6
    2 FX=0.1D+1
      DO 20 I=1,11
      TEMP=X(I)
      IF(X(I).LT.0.1D-14) TEMP=0.1D-14
   20 FX=FX*TEMP**A(I)
      FX=1.0D5*FX
      RETURN
    3 DO 10 I=1,11
      TEMP=X(I)
      IF(X(I).LT.0.1D-14) TEMP=0.1D-14
      GF(I)=1.0D5*FX*(A(I)/TEMP)
   10 CONTINUE   
      GF(12)=0.D+0
      RETURN
    4 IF(INDEX1(1)) G(1)=1.D+0-C(1)*X(1)-C(2)*X(2)-C(3)*X(3)-C(4)*X(4)
     1 *X(5)
      IF(INDEX1(2)) G(2)=1.D+0-C(5)*X(1)-C(6)*X(2)-C(7)*X(3)-C(8)
     1 *X(4)*X(12)-C(9)*X(5)/X(12)-C(10)*X(6)/X(12)-C(11)*X(7)
     2 *X(12)-C(12)*X(4)*X(5)-C(13)*X(2)*X(5)/X(12)-C(14)*X(2)
     3 *X(4)*X(5)-C(15)*X(2)/X(4)*X(5)/X(12)**2-C(16)*X(10)/X(12)
      IF(INDEX1(3)) G(3)=1.0D+0-C(17)*X(1)-C(18)*X(2)-C(19)*X(3)
     1 -C(20)*X(4)-C(21)*X(5)-C(22)*X(6)-C(23)*X(8)-C(24)*X(4)*X(5)
     2 -C(25)*X(2)*X(5)-C(26)*X(2)*X(4)*X(5)-C(27)*X(2)*X(5)/X(4)
     3 -C(28)*X(9)-C(29)*X(1)*X(9)-C(30)*X(11)
      RETURN
    5 IF(.NOT.INDEX2(1)) GOTO 50
      GG(1,4)=-C(4)*X(5)
      GG(1,5)=-C(4)*X(4)
   50 IF(.NOT.INDEX2(2)) GOTO 51
      GG(2,2)=-C(6)-C(13)*X(5)/X(12)-C(14)*X(4)*X(5)-C(15)/X(4)*X(5)/
     1 X(12)**2
      GG(2,4)=-C(8)*X(12)-C(12)*X(5)-C(14)*X(2)*X(5)+C(15)*X(2)/X(4)
     1 **2*X(5)/X(12)**2
      GG(2,5)=-C(9)/X(12)-C(12)*X(4)-C(13)*X(2)/X(12)-C(14)*X(2)*X(4)-
     1 C(15)*X(2)/X(4)/X(12)**2
      GG(2,6)=-C(10)/X(12)
      GG(2,7)=-C(11)*X(12)
      GG(2,10)=-C(16)/X(12)
      GG(2,12)=-C(8)*X(4)+C(9)*X(5)/X(12)**2+C(10)*X(6)/X(12)**2
     1 -C(11)*X(7)+C(13)*X(2)*X(5)/X(12)**2+2*C(15)*X(2)/X(4)
     2 *X(5)/X(12)**3.D+0+C(16)*X(10)/X(12)**2
   51 IF(.NOT.INDEX2(3)) GOTO 52
      GG(3,1)=-C(17)-C(29)*X(9)
      GG(3,2)=-C(18)-C(25)*X(5)-C(26)*X(4)*X(5)-C(27)*X(5)/X(4)
      GG(3,4)=-C(20)-C(24)*X(5)-C(26)*X(2)*X(5)+C(27)*X(2)*X(5)/
     1 X(4)**2
      GG(3,5)=-C(21)-C(24)*X(4)-C(25)*X(2)-C(26)*X(2)*X(4)-C(27)*X(2)
     1 /X(4)
      GG(3,9)=-C(28)-C(29)*X(1)
   52 RETURN
      END
