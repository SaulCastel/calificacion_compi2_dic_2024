module sandwiches_suite
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parser
    implicit none
    private
    character(len=*), parameter :: filePrefix = 'test/inputs/'

    public :: collect_sandwiches_suite

    contains

    !> Collect all exported unit tests
    subroutine collect_sandwiches_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [&
            new_unittest("Pan BLT", test_blt),&
            new_unittest("Pan Pollo", test_pollo),&
            new_unittest("Pan Shuco", test_shuco),&
            new_unittest("Pan Desconocido", test_desconocido)&
        ]

    end subroutine collect_sandwiches_suite

    subroutine test_blt(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'blt.txt')

        result = parse(input)
        call check(error, result, 'Un sandwich BLT')
        if (ALLOCATED(error)) return
    end subroutine test_blt

    subroutine test_pollo(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'pollo.txt')

        result = parse(input)
        call check(error, result, 'Un sandwich de pollo')
        if (ALLOCATED(error)) return
    end subroutine test_pollo

    subroutine test_shuco(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'shuco.txt')

        result = parse(input)
        call check(error, result, 'Un shuco')
        if (ALLOCATED(error)) return
    end subroutine test_shuco

    subroutine test_desconocido(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'desconocido.txt')

        result = parse(input)
        call check(error, result, 'Pan desconocido')
        if (ALLOCATED(error)) return
    end subroutine test_desconocido

    function getFileContents(filename) result(contents)
        character(len=*) :: filename
        character(:), allocatable :: contents
        integer :: inputLen

        inquire(file=filename, size=inputLen)
        allocate(character(len=inputLen) :: contents)
        open (1, file=filename, status='old', action='read', access='stream', form='unformatted')
        read (1) contents
    end function getFileContents
end module sandwiches_suite
