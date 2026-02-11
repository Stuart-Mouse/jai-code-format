
make_identifier :: (name: string) -> *Code_Ident {
    ident := New(Code_Ident);
    ident.name = name;
    return ident;
}

make_binary_operator :: (operator_type: Operator_Type, lhs: *Code_Node, rhs: *Code_Node) -> *Code_Binary_Operator {
    code_operator := New(Code_Binary_Operator);
    code_operator.operator_type = operator_type;
    code_operator.left  = lhs;
    code_operator.right = rhs;
    return code_operator;
}

make_unary_operator :: (operator_type: Operator_Type, expression: *Code_Node) -> *Code_Unary_Operator {
    code_operator := New(Code_Unary_Operator);
    code_operator.operator_type = operator_type;
    code_operator.subexpression = expression;
    return code_operator;
}

make_procedure_call :: (procedure_name: string, arguments: ..Code_Argument) -> *Code_Procedure_Call {
    call := New(Code_Procedure_Call);
    call.procedure_expression = make_identifier(procedure_name);
    call.arguments_unsorted   = array_copy(arguments);
    return call;
}

make_procedure_call :: (procedure_expression: *Code_Node, arguments: ..Code_Argument) -> *Code_Procedure_Call {
    call := New(Code_Procedure_Call);
    call.procedure_expression = procedure_expression;
    call.arguments_unsorted   = array_copy(arguments);
    return call;
}

// make_declaration :: (name: string, type_inst: *Code_Type_Instantiation = null, expression: *Code_Node = null)

make_if :: (condition: *Code_Node, then_block: *Code_Block, else_block: *Code_Block, flags: type_of(Code_If.if_flags)) -> *Code_If {
    code := New(Code_If);
    code.condition  = condition;
    code.then_block = then_block;
    code.else_block = else_block;
    code.if_flags   = flags;
    return code;
}

make_if_case :: (condition: *Code_Node, cases: [] *Code_Case) -> *Code_If {
    code_if := New(Code_If);
    code_if.if_flags = .IS_SWITCH_STATEMENT;
    code_if.condition = condition;
    code_if.then_block = ifx 1 {
        block := New(Code_Block);
        block.block_type       = .IMPERATIVE;
        block.owning_statement = code_if;
        block.statements       = cast,force([]*Code_Node) cases;
        for cases {
            // NOTE: I have no idea why, but it seems to be the case that the then blocks of case statements point to the first statement of the then block as their owning statement.
            // it.then_block.owning_statement = it.then_block.statements[0];
            it.then_block.parent = block;
        }
        block;
    };
    return code_if;
}

make_array_literal :: (element_type: *Code_Type_Instantiation, array_members: [] *Code_Node) -> *Code_Literal {
    literal := New(Code_Literal);
    literal.value_type = .ARRAY;
    literal.array_literal_info = New(Code_Array_Literal_Info);
    literal.array_literal_info.element_type  = element_type;
    literal.array_literal_info.array_members = array_members;
    return literal;
}

get_location :: (code: *Code_Node) -> Source_Code_Location {
    return .{
        fully_pathed_filename = ifx code.enclosing_load then code.enclosing_load.fully_pathed_filename,
        line_number = code.l0,
        character_number = code.c0,
    };
}
