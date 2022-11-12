
module types

    implicit none

    type :: a_list_item
        type(a_tree_node), pointer  :: deity
        real                        :: amount
        type(a_list_item), pointer  :: next
    end type a_list_item

    type :: a_tree_node
        character(len=:), allocatable   :: name
        type(a_list_item), pointer      :: debit
        type(a_list_item), pointer      :: credit
        type(a_tree_node), pointer      :: left
        type(a_tree_node), pointer      :: right
    end type a_tree_node

end module
