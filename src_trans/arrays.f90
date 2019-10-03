    module arrays
    
       use params
       implicit none

       real(dp), dimension(nkpts)   :: xkpts
       real(dp), dimension(nk2pts)  :: xk2pts
       
       real(dp), dimension(ntran) :: taua
       real(dp), dimension(ntran) :: taum
       real(dp), dimension(ntran) :: tauw
       real(dp), dimension(ntran) :: taust
       real(dp), dimension(ntran) :: taur
       real(dp), dimension(ntran) :: taui
   
       real(dp), dimension(nzpts)  :: xzptsu,xzptsr,xzptsue,pinvu,pinvr,zpts
       real(dp), dimension(nzpts)  :: xhptsu,xhptsr
       real(dp), dimension(nzpts,nzpts) :: pizu,pizr
       
       real(dp), allocatable, dimension(:,:) :: y2,optk2,v,vp,y,optk,opth
       real(dp), allocatable, dimension(:,:) :: optcs2,optcs,optcm,optca
  
       real(dp), allocatable, dimension(:,:,:) :: trank,tranh,tranv
       real(dp), allocatable, dimension(:,:,:) :: trancs,trancm,tranca

       real(dp), allocatable, dimension(:) :: xc2pts,xy2pts,xca2pts
       real(dp), allocatable, dimension(:,:) :: distc,distk,disty
       real(dp), allocatable, dimension(:,:) :: aggfc,aggfk,aggfy,aggfca
       

    end module arrays
