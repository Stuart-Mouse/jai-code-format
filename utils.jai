
make_identifier :: (name: string) -> *Code_Ident {
    ident := New(Code_Ident);
    ident.name = name;
    return ident;
}

make_binary_operator :: (operator_type: Operator_Type, lhs: *Code_Node, rhs: *Code_Node) -> *Code_Binary_Operator {
    code_operator := New(Code_Binary_Operator);
    code_operator.operator_type = #char ".";
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
