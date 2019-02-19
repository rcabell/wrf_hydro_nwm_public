module flux_aggregator
    implicit none
    public :: init_flux_aggregator, FluxAggregator
    private

    type :: FluxAggregator_
        real, allocatable :: overland_in(:, :)
        real, allocatable :: landmodel_in(:, :)
        real, allocatable :: groundwater_out(:, :)
        integer :: version
    contains
    end type FluxAggregator_

    interface FluxAggregator_
        module procedure FluxAggregator_init            ! constructor
    end interface FluxAggregator_

    type(FluxAggregator_), allocatable :: FluxAggregator        ! singleton

contains

    subroutine init_flux_aggregator(xx, yy)
        integer, intent(in) :: xx, yy
        if (.not. allocated(FluxAggregator)) then
            allocate(FluxAggregator)
            FluxAggregator = FluxAggregator_(xx, yy)
        end if
        ! else reinit with new dimensions or warn??
    end subroutine init_flux_aggregator


    type(FluxAggregator_) function FluxAggregator_init(xx, yy)
        implicit none

        integer, intent(in) :: xx, yy
        integer :: dx, dy

        allocate(FluxAggregator_init%overland_in(xx, yy))
        allocate(FluxAggregator_init%landmodel_in(xx, yy))
        allocate(FluxAggregator_init%groundwater_out(xx, yy))

        FluxAggregator_init%version = 1

        FluxAggregator_init%overland_in = 0
        FluxAggregator_init%landmodel_in = 0
        FluxAggregator_init%groundwater_out = 0

    end function FluxAggregator_init

end module flux_aggregator