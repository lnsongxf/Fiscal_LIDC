! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of Auxilliary Variables Used in Computation.
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    12/26/2016                 Legacy Code
!
! ===============================================================================
    
    module arrays
    
       use params
       implicit none
       
       real(dp), dimension(nval)   :: eprice
       real(dp), dimension(11)     :: weight
       real(dp), dimension(nkpts)  :: xkpts
       real(dp), dimension(nk2pts) :: xk2pts
   
       real(dp), dimension(nzpts)  :: xzptsu,xzptsr,pinvu,pinvr,zpts,xzptsue
       real(dp), dimension(nzpts)  :: xhptsu,xhptsr
       real(dp), dimension(nzpts,nzpts) :: pizu,pizr
       
       real(dp), allocatable, dimension(:,:) :: y2u,optk2u,vu,vpu,yu,optku,opthu
       real(dp), allocatable, dimension(:,:) :: optcs2u,optcsu,optcmu,optcau
  
       real(dp), allocatable, dimension(:,:) :: y2r,optk2r,vr,vpr,yr,optkr,opthr
       real(dp), allocatable, dimension(:,:) :: optcs2r,optcsr,optcmr,optcar
       
       real(dp), allocatable, dimension(:) :: distc,distk,disty
       real(dp), allocatable, dimension(:) :: xc2pts,xy2pts,xca2pts
       real(dp), allocatable, dimension(:) :: aggfc,aggfk,aggfy,aggfca
       real(dp), allocatable, dimension(:) :: shareca
  
       REAL(DP), DIMENSION(11) :: calidata
       REAL(DP), DIMENSION(11) :: calimodel
    end module arrays
