!===============================================================
	subroutine euler(n,x,f)
	
	use params
    use states
    use arrays
    use prices
	implicit none
    
    integer,  intent(in) :: n
    real(dp), intent(in), dimension(n) :: x,f
    real(dp) :: rh, xhst,rst,pist,lam
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    lam = (alphast2*pst*z*(bigd)**alphast1/wf(nt))**(one/(one-alphast2))
    xhst = lam*xk**((one-alphast1-alphast2)/(one-alphast2))
    rst = pst*(z*(bigd)**alphast1)*(xhst**alphast2)*(xk**(one-alphast1-alphast2))
    pist = rst - wf(nt)*xhst
    
    rh = (one-taust(nt))*pist + taust(nt)*delta*pm(nt)*xk + (one-delta)*pm(nt)*xk
    
    solve_xk = const_xk - rh

    return
    end function solve_xk
!===============================================================