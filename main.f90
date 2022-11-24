program main
    use :: modules
    implicit none

    integer,    parameter       :: MAXLENGTH = 15   ! We set a max length of 15 characters for the input strings
    integer                     :: int              ! Takes account of read's input

    character(len=MAXLENGTH)    :: firstDeity
    character(len=MAXLENGTH)    :: transaction
    real(kind=8)                :: amount           ! kind=8 for necessary precision in net results
    character(len=MAXLENGTH)    :: preposition
    character(len=MAXLENGTH)    :: secondDeity

    type(a_tree_node), pointer  :: root
    
    do
        read (*, *, iostat = int) firstDeity, transaction, amount, preposition, secondDeity
        if (int < 0) exit        ! iostat is negative when nothing is read
        if ( (transaction == "borrowed") .or. (transaction == "lent") ) then
            block
                ! First we add the Deities to the trees
                character(len=:), allocatable   :: Deity1
                character(len=:), allocatable   :: Deity2
                character(len=:), allocatable   :: credit
                character(len=:), allocatable   :: debit
                credit = 'credit'
                debit = 'debit'
                credit = trim(credit)
                debit = trim(debit)
                Deity1 = trim (firstDeity)      ! Strings trimmed to their real size
                Deity2 = trim (secondDeity)     
                call Insert_Deity(root, Deity1)
                call Insert_Deity(root, Deity2)
                
                ! Now we add the transactions
                if ( transaction == "borrowed" ) then
                    call Insert_Transaction(root, Deity1, Deity2, amount, credit)
                    call Insert_Transaction(root, Deity2, Deity1, amount, debit)
    
                else if ( transaction == "lent" ) then
                    call Insert_Transaction(root, Deity1, Deity2, amount, debit)
                    call Insert_Transaction(root, Deity2, Deity1, amount, credit)

                end if

            end block
            
        else    ! We add an error message that pops-up with wrong transactions 
            print '(3a)',"Wrong transaction '",trim(transaction),"'."
        end if
    end do
    print '(a)'
    call Print_All(root)
    call Destroy_BST(root)
end program main