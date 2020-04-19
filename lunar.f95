! Translation of
! <http://www.cs.brandeis.edu/~storer/LunarLander/LunarLander/LunarLanderListing.jpg>
! by Jim Storer from FOCAL to Fortran 95.

program lunar

    implicit none

    common A, G, I, J, K, L, M, N, Q, S, T, V, W, Z
    real :: A, G, I, J, K, L, M, N, Q, S, T, V, W, Z

 10 format (A)
 20 format (A//)
 30 format (A, F9.2, A)

    write(*,10) "CONTROL CALLING LUNAR MODULE. MANUAL CONTROL IS NECESSARY"
    write(*,10) "YOU MAY RESET FUEL RATE K EACH 10 SECS TO 0 OR ANY VALUE"
    write(*,10) "BETWEEN 8 & 200 LBS/SEC. YOU'VE 16000 LBS FUEL. ESTIMATED"
    write(*,20) "FREE FALL IMPACT TIME-120 SECS. CAPSULE WEIGHT-32500 LBS"
120 write(*,20) "FIRST RADAR CHECK COMING UP"
    write(*,10) "COMMENCE LANDING PROCEDURE"
    write(*,10) "TIME,SECS   ALTITUDE,MILES+FEET   VELOCITY,MPH   FUEL,LBS   FUEL RATE"
    A = 120
    V = 1
    M = 32500
    N = 16500
    G = 0.001
    Z = 1.8

210 write(*,211,advance="no") nint(L), int(A), nint(5280*(A-int(A))), 3600*V, M-N, "K=:"
211 format (i7, i16, i7, F15.2, F12.1, A9)

    read*, K
    T=10
270 if(K.gt.200) goto 272
    if(K.lt.0) goto 272
    if(K.ge.8)goto 310
    if(K.gt.0) goto 272
    goto 310
272 write(*,10,advance="no") "NOT POSSIBLE...................................................K=:"
    read*,K
    goto 270

310 if((M-N).lt.0.001) goto 410
    if(T.lt.0.001) goto 210
    S = T
    if((N+S*K).le.M) goto 350
    S = (M-N)/K
350 call sub9()
    if(I.le.0) goto 710
    if(V.le.0) goto 380
    if(J.lt.0) goto 810
380 call sub6()
    goto 310

410 write(*,30) "FUEL OUT AT",L," SECS"!
    S = (sqrt(V*V + 2*A*G)-V) / G
    V = V+G*S
    L = L+S

510 write(*,30) "ON THE MOON AT", L, " SECS"
    W = 3600*V
    write(*,30) "IMPACT VELOCITY OF", W, " M.P.H."
    write(*,30) "FUEL LEFT:", M-N, " LBS"
    if(W.gt.1) goto 550
    write(*,10) "PERFECT LANDING !-(LUCKY)"
    goto 590
550 if(W.gt.10) goto 560
    write(*,10) "GOOD LANDING-(COULD BE BETTER)"
    goto 590
560 if(W.gt.22) goto 570
    write(*,10) "CONGRATULATIONS ON A POOR LANDING"
    goto 590
570 if(W.gt.40) goto 581
    write(*,10) "CRAFT DAMAGE. GOOD LUCK"
    goto 590
581 if(W.gt.60) goto 582
    write(*,10) "CRASH LANDING-YOU'VE 5 HRS OXYGEN"
    goto 590
582 write(*,10) "SORRY,BUT THERE WERE NO SURVIVORS-YOU BLEW IT!"
    write(*,30) "IN FACT YOU BLASTED A NEW LUNAR CRATER", W*0.277777, " FT.DEEP."
590 write(*,'(///A)') "TRY AGAIN?"
! 592 A "(ANS. YES OR NO)"P;I (P-0NO)5.94,5.98
! 594 I (P-0YES)5.92,1.2,5.92
598 write(*,20) "CONTROL OUT"
    stop

710 if(S.lt.0.005) goto 510
    S = 2*A/(V + sqrt(V*V + 2*A*(G - Z*K/M)))
    call sub9()
    call sub6()
    goto 710

810 W = (1 - M*G/(Z*K))/2
    S = M*V/(Z*K*(W+sqrt(W*W + V/Z)))+.05
    call sub9()
    if(I.le.0) goto 710
    call sub6()
    if(J.ge.0) goto 310
    if(V.le.0) goto 310
    goto 810

end program lunar

! (Subroutine in section 06 of original FOCAL source)
subroutine sub6()
    implicit none
    common A, G, I, J, K, L, M, N, Q, S, T, V, W, Z
    real :: A, G, I, J, K, L, M, N, Q, S, T, V, W, Z

    L = L+S
    T = T-S
    M = M-S*K
    A = I
    V = J
    return
end subroutine sub6

! (Subroutine in section 09 of original FOCAL source)
subroutine sub9()
    implicit none
    common A, G, I, J, K, L, M, N, Q, S, T, V, W, Z
    real :: A, G, I, J, K, L, M, N, Q, S, T, V, W, Z

    Q = S*K/M
    J = V + G*S + Z*(-Q - Q**2/2 - Q**3/3 - Q**4/4 - Q**5/5)
    I = A - G*S * S/2 - V*S + Z*S*(Q/2 + Q**2/6 + Q**3/12 + Q**4/20 + Q**5/30)
    return
end subroutine sub9
