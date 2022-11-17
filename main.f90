program main
    use :: modules
    implicit none

    integer,    parameter   :: LENGTH = 15
    integer                 :: int          ! Takes account of read's input

    character(len=LENGTH)   :: firstDeity
    character(len=LENGTH)   :: transaction
    real(kind=8)            :: amount       ! kind=8 for necessary precision in net results
    character(len=LENGTH)   :: preposition
    character(len=LENGTH)   :: secondDeity

    type(a_tree_node), pointer  :: root
    
    do
        read ( *, *, iostat = int ) firstDeity, transaction, amount, preposition, secondDeity
        if ( int < 0 ) exit        ! iostat is negative when nothing is read
        if ( ( transaction == "borrowed" ) .or. ( transaction == "lent" ) ) then
            block
                ! First we add the Deities to the trees
                character( len=: ), allocatable     :: Deity1
                character( len=: ), allocatable     :: Deity2
                Deity1 = trim ( firstDeity )
                Deity2 = trim ( secondDeity )
                call Insert_Deity( root, Deity1 )
                call Insert_Deity( root, Deity2 )
                
                ! Now we add the transactions
                if ( transaction == "borrowed" ) then
                    call Insert_Credit(root, Deity1, Deity2, amount)
                    call Insert_Debit(root, Deity2, Deity1, amount)
    
                else if ( transaction == "lent" ) then
                    call Insert_Debit(root, Deity1, Deity2, amount)
                    call Insert_Credit(root, Deity2, Deity1, amount)

                end if

            end block

            
        else
            print '(a,a,a)',"Wrong transaction '",trim( transaction ),"'."
        end if
    end do

    call Print_BST(root)
end program main