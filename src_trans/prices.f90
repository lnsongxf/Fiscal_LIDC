    module prices
      
       use params
       implicit none
       
       real(dp), dimension(ntran,5)    :: eprice
       real(dp), dimension(ntran_in,5) :: eprice_in
       real(dp), dimension(ntran)      :: ps
       real(dp), dimension(ntran)      :: pa
       real(dp), dimension(ntran)      :: w
       real(dp), dimension(ntran)      :: wf
       real(dp), dimension(ntran)      :: re
    end module prices