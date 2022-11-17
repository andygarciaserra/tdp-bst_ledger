
module modules

    implicit none

    type :: a_list_item
        type(a_tree_node), pointer  :: deity
        real(kind=8)                :: amount
        type(a_list_item), pointer  :: next
    end type a_list_item

    type :: a_tree_node
        character(len=:), allocatable   :: name
        type(a_list_item), pointer      :: debit
        type(a_list_item), pointer      :: credit
        type(a_tree_node), pointer      :: left
        type(a_tree_node), pointer      :: right
    end type a_tree_node

contains

    ! Print BST and Total Credit/Debit
    recursive subroutine Print_All(root)
        type(a_tree_node), pointer, intent(in) :: root
        real(kind=8)                           :: debitsum
        real(kind=8)                           :: creditsum
        debitsum = 0
        creditsum = 0

        call Print_BST(root)
        call Sum_Transactions(root, debitsum, creditsum)
        print '(a)', ''
        print '(a,f0.2)', 'Net debit:  ', debitsum
        print '(a,f0.2)', 'Net credit: ', creditsum

    end subroutine Print_All

    ! Summing transactions
    recursive subroutine Sum_Transactions(root, debitsum, creditsum)
        type(a_tree_node), pointer, intent(in)      :: root
        real(kind=8), intent(in out)                :: debitsum
        real(kind=8), intent(in out)                :: creditsum

        if (associated(root)) then
            call Sum_Transactions(root%left, debitsum, creditsum)
            call Sum_List(root%credit,creditsum)
            call Sum_List(root%debit,debitsum)
            call Sum_Transactions(root%right, debitsum, creditsum)
        end if

    end subroutine Sum_Transactions

    ! Summing list elements
    recursive subroutine Sum_List(head,total)
        type(a_list_item), pointer, intent(in)      :: head
        real(kind=8), intent(in out)                :: total
        if(associated(head)) then
            call Sum_List(head%next,total)
            total = total + head%amount
        end if

    end subroutine Sum_List

    ! Printing BST
    recursive subroutine Print_BST(root)
        type(a_tree_node), pointer, intent(in)  :: root
        block
            if(associated(root)) then
                call Print_BST(root%left)
                print '(a,a)', root%name, ':'
                if(associated(root%debit)) then
                    print '(a)', '    debit'
                    call Print_List(root%debit)
                end if
                if(associated(root%credit)) then
                    print '(a)', '    credit'
                    call Print_List(root%credit)
                end if
                call Print_BST(root%right)
            end if
        end block
    end subroutine Print_BST

    ! Printing a List
    recursive subroutine Print_List(head)
        type(a_list_item), pointer, intent(in)  :: head

        if(associated(head)) then
            print '(a,a,a,f0.2)', '        ', head%deity%name, ' ', head%amount 
            call Print_List(head%next)
        end if
    end subroutine Print_List

    ! Inserting element in the credit or debit linked to node lists
    recursive subroutine Insert_in_List(head, amount, secondNode)    
        type(a_list_item), pointer, intent(in out)  :: head
        real(kind=8), intent(in)                    :: amount
        type(a_tree_node), pointer, intent(in)      :: secondNode
        type(a_list_item), pointer                  :: newHead

        if(.not.associated(head)) then
            allocate(newHead)
            newHead = a_list_item(deity = secondNode, amount = amount, next = null())
            head => newHead
        else if(llt(secondNode%name,head%deity%name)) then
            allocate(newHead)
            newHead = a_list_item(deity = secondNode, amount = amount, next = head)
            head => newHead
        else if(secondNode%name == head%deity%name) then
            head%amount = head%amount + amount
        else if(lgt(secondNode%name,head%deity%name)) then 
            call Insert_in_List(head%next,amount,secondNode)
        end if
    end subroutine Insert_in_List

    ! Finding the node of a given Deity given string name
    recursive function Find_Node(root, name) result(node)
        character(len=:), allocatable, intent(in)   :: name
        type(a_tree_node), pointer, intent(in)      :: root
        type(a_tree_node), pointer                  :: node

        if (.not.associated(root)) then
            node => null()
            print *, "'", name, "' hasn't been added yet."
        else if (lgt(name, root%name)) then
            node => Find_Node(root%right, name)
        else if (llt(name, root%name)) then
            node => Find_Node(root%left, name)
        else
            node => root
        end if
    end function Find_Node

    ! Checking if a Deity is already in the BST, if not, adding it
    recursive subroutine Insert_Deity(node, name)
        type(a_tree_node), pointer, intent(in out)  :: node
        character (len=:), allocatable, intent(in)  :: name 

        if (.not.associated(node)) then
            allocate(node)
            node = a_tree_node(name=name,debit=null(),credit=null(),left=null(),right=null())
            print '(a,a)', name," has been added."
        else if (lgt(name,node%name)) then       ! If the name is Lexically Greater Than the actual one, repeat with right branch
            call Insert_Deity(node%right,name)
        else if(llt(name,node%name)) then        ! If the name is Lexically Less Than the actual one, repeat with left branch
            call Insert_Deity(node%left,name)
        end if
    end subroutine Insert_Deity

    ! Inserting Credit element to a Deity list
    recursive subroutine Insert_Credit(root, Deity1, Deity2, amount)
        type(a_tree_node), pointer, intent(in out)  :: root
        character (len=:), allocatable, intent(in)  :: Deity1
        character (len=:), allocatable, intent(in)  :: Deity2
        real(kind=8), intent (in)                   :: amount
        type(a_tree_node), pointer                  :: firstNode
        type(a_tree_node), pointer                  :: secondNode

        firstNode => Find_Node(root, Deity1)
        secondNode => Find_Node(root, Deity2)
        call Insert_in_List(firstNode%credit, amount, secondNode)
    end subroutine Insert_Credit

    ! Inserting Debit element to a Deity list
    recursive subroutine Insert_Debit(root, Deity1, Deity2, amount)
        type(a_tree_node), pointer, intent(in out)  :: root
        character (len=:), allocatable, intent(in)  :: Deity1
        character (len=:), allocatable, intent(in)  :: Deity2
        real(kind=8), intent(in)                    :: amount
        type(a_tree_node), pointer                  :: firstNode
        type(a_tree_node), pointer                  :: secondNode

        firstNode => Find_Node(root, Deity1)
        secondNode => Find_Node(root, Deity2)
        call Insert_in_List(firstNode%debit, amount, secondNode)
    end subroutine Insert_Debit

    ! Destroying the tree
    recursive subroutine Destroy_BST(root)
        type(a_tree_node), pointer, intent(in out)      :: root

        if(associated(root)) then
            call Destroy_BST(root%left)
            call Destroy_BST(root%right)
            call Destroy_List(root%credit)
            call Destroy_List(root%debit)
            deallocate(root)
        end if
    end subroutine

    ! Destroying a list
    recursive subroutine Destroy_List(head)
        type(a_list_item), pointer, intent(in out)      :: head

        if(associated(head)) then
            call Destroy_List(head%next)
            deallocate(head)
        end if
    end subroutine
end module