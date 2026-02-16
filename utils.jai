/*
    Most of this file is just procedures to create different kinds of nodes more conveniently.
    At the bottom there are a few other useful procedures. Maybe those should be moved up top.
*/

make_ident :: (name: string, flags: type_of(Code_Ident.flags) = xx 0) -> *Code_Ident {
    ident := New(Code_Ident);
    ident.name = name;
    return ident;
}

make_binary_operator :: (operator_type: Operator_Type, lhs: *Code_Node, rhs: *Code_Node, flags: Code_Binary_Operator.Flags = xx 0) -> *Code_Binary_Operator {
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
    call.procedure_expression = make_ident(procedure_name);
    call.arguments_unsorted   = array_copy(arguments);
    return call;
}

make_procedure_call :: (procedure_expression: *Code_Node, arguments: ..Code_Argument) -> *Code_Procedure_Call {
    call := New(Code_Procedure_Call);
    call.procedure_expression = procedure_expression;
    call.arguments_unsorted   = array_copy(arguments);
    return call;
}


make_declaration :: (name: string, type_inst: *Code_Type_Instantiation = null, expression: *Code_Node = null, flags: type_of(Code_Declaration.flags) = xx 0) -> *Code_Declaration {
    declaration := New(Code_Declaration);
    declaration.name        = name;
    declaration.type_inst   = type_inst;
    declaration.expression  = expression;
    declaration.flags       = flags;
    return declaration;
}

make_declaration :: (name: string, type_name: string, expression: *Code_Node = null, flags: type_of(Code_Declaration.flags) = xx 0) -> *Code_Declaration {
    return make_declaration(name, make_type_inst_from_ident(type_name), expression, flags);
}

make_declaration_from_code :: (name: string, type_inst := #code,null, expression := #code,null, flags: type_of(Code_Declaration.flags) = xx 0) -> *Code_Declaration {
    declaration := New(Code_Declaration);
    declaration.name        = name;
    declaration.type_inst   = ifx type_inst then make_type_inst_from_code(type_inst);
    declaration.expression  = ifx expression then compiler_get_nodes(expression);
    declaration.flags       = flags;
    return declaration;
}


make_type_inst_from_code :: (type_valued_expression: Code) -> *Code_Type_Instantiation {
    type_inst := New(Code_Type_Instantiation);
    type_inst.type_valued_expression = compiler_get_nodes(type_valued_expression);
    return type_inst;
}

make_type_inst :: (type_valued_expression: *Code_Node) -> *Code_Type_Instantiation {
    type_inst := New(Code_Type_Instantiation);
    type_inst.type_valued_expression = type_valued_expression;
    return type_inst;
}

make_type_inst_from_ident :: (type_name: string) -> *Code_Type_Instantiation {
    type_inst := New(Code_Type_Instantiation);
    type_inst.type_valued_expression = make_ident(type_name);
    return type_inst;
}

make_type_inst_fixed_array :: (array_element_type: *Code_Type_Instantiation, array_dimension: int) {
    type_inst := New(Code_Type_Instantiation);
    type_inst.array_element_type = array_element_type;
    type_inst.array_dimension = array_dimension;
    return type_inst;
}

make_type_inst_array_view :: (array_element_type: *Code_Type_Instantiation) {
    type_inst := New(Code_Type_Instantiation);
    type_inst.array_element_type = array_element_type;
    type_inst.inst_flags = .ARRAY_VIEW;
    return type_inst;
}

make_type_inst_resizable_array :: (array_element_type: *Code_Type_Instantiation) {
    type_inst := New(Code_Type_Instantiation);
    type_inst.array_element_type = array_element_type;
    type_inst.inst_flags = .RESIZABLE;
    return type_inst;
}

make_type_inst_pointer_to :: (pointer_to: *Code_Type_Instantiation) {
    type_inst := New(Code_Type_Instantiation);
    type_inst.pointer_to = pointer_to;
    return type_inst;
}


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

make_type_of :: (expression: *Code_Node) {
    code_expression_query := New(Code_Expression_Query);
    code_expression_query.query_kind = .TYPE_OF;
    code_expression_query.expression_to_query = expression;
    return code_expression_query;
}





get_location :: (code: *Code_Node) -> Source_Code_Location {
    return .{
        fully_pathed_filename = ifx code.enclosing_load then code.enclosing_load.fully_pathed_filename,
        line_number = code.l0,
        character_number = code.c0,
    };
}

print_code :: (code: *Code_Node) {
    builder: String_Builder;
    Program_Print.print_expression(*builder, code);
    append(*builder, "\n");
    write_string(builder_to_string(*builder));
}