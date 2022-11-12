program main
    use :: types
    implicit none

    integer,    parameter   :: Length = 20
    integer                 :: int

    character(len=Length)   :: firstName
    character(len=Length)   :: secondName
    character(len=Length)   :: transaction
    character(len=Length)   :: preposition
    real                    :: amount

    type(a_tree_node), pointer  :: root
    
    do
        read (*, *, iostat = int) firstName, transaction, amount, preposition, secondName
        if (int < 0) exit        !iostat is negative when nothing is read
        if ((transaction == "borrowed") .or. (transaction == "lent")) then
            block
            end block
        else
            print *,"Wrong transaction '",trim(transaction),"'."
        end if
    end do

end program main