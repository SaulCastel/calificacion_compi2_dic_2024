module actions_suite
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parser
    implicit none
    private
    character(len=*), parameter :: filePrefix = 'test/inputs/'

    public :: collect_actions_suite

    contains

    !> Collect all exported unit tests
    subroutine collect_actions_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [&
            new_unittest("Parentesis con accion", test_paren_action),&
            new_unittest("Conteo con accion", test_count_action),&
            new_unittest("Conteo con accion y delimitador", test_count_delimiter_action),&
            new_unittest("Repeticion min-max con acciones", test_min_max_action),&
            new_unittest("Repeticion min-max con acciones y delimitador", test_min_max_delimiter_action),&
            new_unittest("Asersion negativa con accion", test_neg_assertion_action),&
            new_unittest("Asersion positiva con accion", test_assertion_action)&
        ]

    end subroutine collect_actions_suite

    subroutine test_paren_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'parentesis_accion.txt')

        result = parse(input)
        call check(error, result, 'a.5.')
        if (ALLOCATED(error)) return
    end subroutine test_paren_action

    subroutine test_count_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'conteo_accion.txt')

        result = parse(input)
        call check(error, result, 'bbb')
        if (ALLOCATED(error)) return
    end subroutine test_count_action

    subroutine test_count_delimiter_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'conteo_delimitador_accion.txt')

        result = parse(input)
        call check(error, result, 'ccc')
        if (ALLOCATED(error)) return
    end subroutine test_count_delimiter_action

    subroutine test_min_max_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'min_max_accion.txt')

        result = parse(input)
        call check(error, result, 'dd')
        if (ALLOCATED(error)) return
    end subroutine test_min_max_action

    subroutine test_min_max_delimiter_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'min_max_delimitador_accion.txt')

        result = parse(input)
        call check(error, result, 'ee')
        if (ALLOCATED(error)) return
    end subroutine test_min_max_delimiter_action

    subroutine test_neg_assertion_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'asersion_negativa_accion.txt')

        result = parse(input)
        call check(error, result, 'f789')
        if (ALLOCATED(error)) return
    end subroutine test_neg_assertion_action

    subroutine test_assertion_action(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'asersion_accion.txt')

        result = parse(input)
        call check(error, result, 'g101')
        if (ALLOCATED(error)) return
    end subroutine test_assertion_action

    function getFileContents(filename) result(contents)
        character(len=*) :: filename
        character(:), allocatable :: contents
        integer :: inputLen

        inquire(file=filename, size=inputLen)
        allocate(character(len=inputLen) :: contents)
        open (1, file=filename, status='old', action='read', access='stream', form='unformatted')
        read (1) contents
    end function getFileContents
end module actions_suite
