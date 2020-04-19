!-----------------------------------------------------------------------
!     Translation of
!     <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
!     by Jim Storer from FOCAL to Fortran 95.
!-----------------------------------------------------------------------

      program lunar

      implicit none
      common  A, G, I, J, K, L, M, N, S, T, V, Z
      real :: A, G, I, J, K, L, M, N, S, T, V, Z
      real :: W

   10 format (A)
   20 format (A//)
   30 format (A, F8.2, A)

  104 write(*,10) "CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY"
  106 write(*,10) "YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE"
  108 write(*,10) "BETWEEN 8 & 200 LBS/SEC. YOU'VE 16000 LBS FUEL. ESTIMATED"
  111 write(*,20) "FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS"
  120 write(*,20) "FIRST RADAR CHECK COMING UP"
  130 write(*,10) "COMMENCE LANDING PROCEDURE"
  140 write(*,10) "TIME,SECS   ALTITUDE,MILES+FEET   VELOCITY,MPH   FUEL,LBS   FUEL RATE"
  150 A=120; V=1; M=32500; N=16500; G=.001; Z=1.8

  210 write(*, 211, advance="no") nint(L), int(A), nint(5280*(A-int(A))), 3600*V, M-N, "K=:"
  211 format (i7, i16, i7, F15.2, F12.1, A8)
      stop
! 220 T %6.02,"       ",3600*V,"    ",%6.01,M-N,"      K=";A K;S T=10
! 270 T %7.02;I (200-K)2.72;I (8-K)3.1,3.1;I (K)2.72,3.1
! 272 T "NOT POSSIBLE";F X=1,51;T "."
! 273 T "K=";A K;G 2.7

  310 if((M-N-.001).lt.0) goto 410; if((T-.001).lt.0) goto 210; S=T
  340 if(((N+S*K)-M).le.0) goto 350; S=(M-N)/K
  350 call sub9(); if(I.le.0) goto 710; if(V.le.0) goto 380; if(J.lt.0) goto 810
  380 call sub6(); goto 310

  410 write(*,10) "FUEL OUT AT",L," SECS"!
  440 S=(SQRT(V*V+2*A*G)-V)/G; V=V+G*S; L=L+S

  510 write(*,30) "ON THE MOON AT",L," SECS"; W=3600*V
  520 write(*,30) "IMPACT VELOCITY OF",W,"M.P.H."; write(*,30) "FUEL LEFT:",M-N," LBS"
  540 if(W.gt.1)  goto 550; write(*,10) "PERFECT LANDING !-(LUCKY)"; goto 590
  550 if(W.gt.10) goto 560; write(*,10) "GOOD LANDING-(COULD BE BETTER)"; goto 590
  560 if(W.gt.22) goto 570; write(*,10) "CONGRATULATIONS ON A POOR LANDING"; goto 590
  570 if(W.gt.40) goto 581; write(*,10) "CRAFT DAMAGE. GOOD LUCK"; goto 590
  581 if(W.gt.60) goto 582; write(*,10) "CRASH LANDING-YOU'VE 5 HRS OXYGEN"; goto 590
  582 write(*,10) "SORRY,BUT THERE WERE NO SURVIVORS-YOU BLEW IT!"
  583 write(*,30) "IN FACT YOU BLASTED A NEW LUNAR CRATER",W*.277777," FT.DEEP."
  590 write(*,"(////A)") "TRY AGAIN?"
! 592 A "(ANS. YES OR NO)"P;I (P-0NO)5.94,5.98
! 594 I (P-0YES)5.92,1.2,5.92
  598 write(*,10) "CONTROL OUT"; print*; print*; stop

  710 if(S.lt.0.005) goto 510; S=2*A/(V+SQRT(V*V+2*A*(G-Z*K/M)))
  730 call sub9(); call sub6(); goto 710

  810 W=(1-M*G/(Z*K))/2; S=M*V/(Z*K*(W+SQRT(W*W+V/Z)))+.05; call sub9()
  830 if(I.le.0) goto 710; call sub6(); if(J.ge.0) goto 310; if(V.le.0) goto 310; goto 810

      end program lunar

!-----------------------------------------------------------------------
!     Subroutine in section 06 of original FOCAL source
!-----------------------------------------------------------------------

      subroutine sub6()
        implicit none
        common  A, I, J, K, L, M, N, S, T, V
        real :: A, I, J, K, L, M, N, S, T, V

  610   L=L+S; T=T-S; M=M-S*K; A=I; V=J

        return
      end subroutine sub6

!-----------------------------------------------------------------------
!     Subroutine in section 09 of original FOCAL source
!-----------------------------------------------------------------------

      subroutine sub9()
        implicit none
        common  A, G, I, J, K, M, S, V, Z
        real :: A, G, I, J, K, M, S, V, Z
        real :: Q

  910   Q=S*K/M; J=V+G*S+Z*(-Q-Q**2/2-Q**3/3-Q**4/4-Q**5/5)
  940   I=A-G*S*S/2-V*S+Z*S*(Q/2+Q**2/6+Q**3/12+Q**4/20+Q**5/30)

        return
      end subroutine sub9

