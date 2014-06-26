program gui_main
        use iso_c_binding
    use gtk, only: gtk_init, gtk_main
    use gui_functions
    implicit none

    type(window) :: demo_win

    call gtk_init()

    call create_window(demo_win,"test.glade"//c_null_char)

    call show_window(demo_win)

    call gtk_main()
end program gui_main
