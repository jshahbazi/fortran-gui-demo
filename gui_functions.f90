module gui_functions
    use iso_c_binding
    use gtk
    use gtk_sup
    use g
    implicit none

    type window
        private
        type(c_ptr) :: window_ptr
        type(c_ptr) :: textbox_ptr
    end type

    private
    public :: window, create_window, show_window

contains

    subroutine create_window(this, glade_file_name)
        type(window), target :: this

        type(c_ptr)    :: builder
        type(c_ptr)    :: error
        integer(c_int) :: guint
        character(kind=c_char), dimension(*) :: glade_file_name


        ! load GUI into builder
        builder = gtk_builder_new()
        error = c_null_ptr
        guint = gtk_builder_add_from_file(builder, glade_file_name, error)
        if (guint == 0) then
            print *, "Could not open .glade file."
            stop "Program terminated"
        end if

        ! get references to GUI elements
        ! The name passed to the gtk_builder_get_object function has to match the name
        ! of the objects in Glade
        this%window_ptr  = gtk_builder_get_object(builder,"demo_window"//c_null_char)
        this%textbox_ptr = gtk_builder_get_object(builder,"textbox_enter_name"//c_null_char)

        ! connect signal handlers
        call gtk_builder_connect_signals_full(builder, c_funloc(connect_signals), c_loc(this))

        ! free memory
        call g_object_unref(builder)

    end subroutine

    !-----------------------------------------

    subroutine show_window(this)
        type(window), intent(in) :: this

        call gtk_widget_show(this%window_ptr)
    end subroutine

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !Window and object creation above
    !==================================================================================
    !
    !
    !
    !==================================================================================
    !Connect signals to objects below
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

    subroutine connect_signals (builder, object, signal_name, handler_name, connect_object, flags, c_this) bind(c)
        use iso_c_binding, only: c_ptr, c_char, c_int
        type(c_ptr), value                     :: builder        !a GtkBuilder
        type(c_ptr), value                     :: object         !object to connect a signal to
        character(kind=c_char), dimension(*)   :: signal_name    !name of the signal
        character(kind=c_char), dimension(*), target   :: handler_name   !name of the handler
        type(c_ptr), value                     :: connect_object !a GObject, if non-NULL, use g_signal_connect_object()
        integer(c_int), value                  :: flags          !GConnectFlags to use
        type(c_ptr), value                     :: c_this         !user data

        character(len=32)                      :: h_name


        call c_f_string_chars(handler_name, h_name)
        print *, "Connect signal for = ", h_name

        select case (h_name)  !Add event handlers created in Glade below, otherwise the widgets won't connect to functions
                              !The names in the case have to match the names of the *signals* in Glade and the
                              !text in c_funloc(...) has to match the name of the actual function in the code.
        case ("button_hello_clicked")
            call g_signal_connect (object, signal_name, c_funloc(button_hello_clicked), c_this)
        case ("button_quit_clicked")
            call g_signal_connect (object, signal_name, c_funloc(button_quit_clicked), c_this)
        case ("signal_quit_window")
            call g_signal_connect (object, signal_name, c_funloc(signal_quit_window), c_this)
        case default
            print *, "Unknown handler = "//h_name
            stop "Program terminated"
        end select

    end subroutine

    !==================================================================================
    !Create functions for the gui below.  Then attach them to the gui's code using event handlers in the above
    !function.  Attach the event handlers to the actual buttons or whatever in the gui using Glade
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    function button_hello_clicked(widget, gdata) result(ret) bind(c)
        use gtk_hl_dialog

        type(c_ptr), value :: widget, gdata
        integer(c_int) :: ret
        integer(c_int) :: resp

        type(window), pointer :: this
        integer(c_int16_t) :: elen
        type(c_ptr) :: cptxt
        character(kind=c_char,len=100), pointer :: cname
        character(kind=c_char,len=30)  :: name
        character(len=80) :: msg

        call c_f_pointer(gdata, this)

        print *, "Hello was clicked"

        elen = gtk_entry_get_text_length(this%textbox_ptr)
        print *, 'elen=', elen
        if (elen == 0) then
            msg = "Write your name first, please."
        else
            cptxt = gtk_entry_get_text(this%textbox_ptr)
            call c_f_string(cptxt,name)
            msg  = "Hello "//trim(name)//"!"
        end if

        resp = hl_gtk_message_dialog_show([msg], GTK_BUTTONS_OK, "Message"//c_null_char, parent=this%window_ptr)

        ret = FALSE
    end function

    !-----------------------------------------
    function button_quit_clicked(widget, gdata) result(ret) bind(c)
        type(c_ptr) :: widget, gdata
        integer(c_int) :: ret

        print *, "Quit was clicked"
        call gtk_main_quit()
        ret = FALSE
    end function

    !-----------------------------------------
    function signal_quit_window(widget, gdata) result(ret) bind(c)
        type(c_ptr) :: widget, gdata
        integer(c_int) :: ret

        print *, "Quit window"
        call gtk_main_quit()
        ret = FALSE
    end function

end module


!Don't modify the c_f_string_chars subroutine
subroutine c_f_string_chars(c_string, f_string)
    ! Helper function
    use iso_c_binding
    implicit none
    character(len=1,kind=c_char), intent(in) :: c_string(*)
    character(len=*), intent(out) :: f_string
    integer :: i
    i=1
    do while(c_string(i)/=c_null_char .and. i<=len(f_string))
        f_string(i:i) = c_string(i)
        i=i+1
    end do
    if (i<=len(f_string)) f_string(i:) = ' '
end subroutine c_f_string_chars
