!**********************************************************************!
!                                                                      !
! June 5, 2015                                                         !
!                                                                      !
!**********************************************************************!  
    program multisectors
    
    use params
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
    
    integer, parameter :: n = 11
    integer, parameter :: m = n
	real(dp), dimension(m) :: f
	real(dp), dimension(n) :: x
	real(dp) :: xkinc,btime,etime
	integer  :: i,j
    
    include 'mpif.h'

    call MPI_INIT(mpiierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,mpiierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,mpiierr)
    
    call cpu_time(btime)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    xkpts(1) = 0.00015_dp
    xkinc = zero
    do j = 1,60
       xkinc = xkinc+0.0065_dp*dble(real(j))
       do i = (j-1)*8+2,j*8+1
          xkpts(i) = xkpts(i-1)+xkinc
       enddo
    enddo
    
    xk2pts(1) = xkpts(1)
    xkinc = (xkpts(nkpts)-xkpts(1))/dble(real(nk2pts-1))
    do i = 2,nk2pts
       xk2pts(i) = xk2pts(i-1)+xkinc
    enddo

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    call mpirange(nzpts,nprocs,myrank,jsta,jend)
    ipack = nkpts*(jend-jsta+1)
    iget  = nkpts*nzpts
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if ( myrank .eq. root) then
        write (*,'(A21,I3)') ' Transition Period = ', ntran
    endif
    
    call calfun(m,n,f,x)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (myrank .eq. root) then   
       open(1,file='eprices_out.txt',form='formatted')
          do i = 1,ntran
             write(1,100) (eprice(i,j), j=1,5)
          enddo
       100   format(5f16.10)
       close(1)
    endif  

    call cpu_time(etime)

    if (myrank .eq. root) then 
       print*, 'Time ',(etime-btime)/60.0_dp
       print*, 'finished'
    endif
    
    call MPI_FINALIZE(mpiierr)  
    end program multisectors
    