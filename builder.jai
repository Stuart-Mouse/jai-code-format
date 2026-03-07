
Code_Builder :: struct {
    allocator:                  Allocator;
    node_stack:                 [..] *Code_Node;
    index_of_current_block:     int = -1;
}

// NOTE: We use the temp allocator by default because we don't want to have to think about individually freeing code nodes.
//       In most cases, the user will probably want to provide their own allocator (usually a pool of some sort) which is being used in a borader scope than the code builder.
init :: (builder: *Code_Builder, allocator := temp) {
    builder.allocator = allocator;
}

deinit :: (builder: *Code_Builder) {
    fini(*builder.memory_pool);
}

get_allocator :: (builder: *Code_Builder) -> Allocator {
    return builder.allocator;
}

push_node_stack ::  (builder: *Code_Builder, node: *Code_Node) {
    array_add(*builder.node_stack, node);
}

push_node_stack_block ::  (builder: *Code_Builder, code_block: *Code_Block) {
    builder.index_of_current_block = builder.node_stack.count;
    array_add(*builder.node_stack, code_block);
}

// pop_node_stack :: (builder: *Code_Builder, count: int) -> [] *Code_Node {
//     assert(count <= builder.node_stack.count, "Error: attempted to pop % nodes from node stack, but the stack only contains % nodes.", count, builder.node_stack.count);
//     node := builder.node_stack[builder.node_stack.count-1];
// }

get_current_block :: (builder: *Code_Builder) -> *Code_Block {
    return ifx builder.index_of_current_block >= 0 
        then builder.node_stack[builder.index_of_current_block].(*Code_Block);
}

pop_current_block :: (builder: *Code_Builder) -> *Code_Block, [] *Code_Node {
    contents: [] *Code_Node;
    if builder.index_of_current_block != builder.node_stack.count {
        contents = array_view(builder.node_stack, builder.index_of_current_block + 1);
    }
    code_block := builder.node_stack[builder.index_of_current_block].(*Code_Block);
    assert(code_block.kind == .BLOCK);
    
    builder.node_stack.count = builder.index_of_current_block;
    
    // TODO: try to figure out a way to store index of parent block directly on the current block.
    //       We could either use parent *Code_Block to store this index (which would be very hacky),
    //       or we could find somewhere to put this data on the side. (maybe on the node stack before block itself?)
    
    builder.index_of_current_block -= 1;
    while builder.index_of_current_block >= 0
       && builder.node_stack[builder.index_of_current_block].kind != .BLOCK {
        builder.index_of_current_block -= 1;
    }
    
    return code_block, contents;
}

pop_node_stack :: (builder: *Code_Builder, $kind: Code_Node.Kind) -> *$R 
#modify {
    R = get_type_for_node_kind(kind);
    return R.(*Type_Info) != null, "Invalid code node kind.";
} {
    node := builder.node_stack[builder.node_stack.count-1];
    builder.node_stack.count -= 1;
    return xx ifx node.kind == kind then node;
}

peek_node_stack :: (builder: *Code_Builder, $kind: Code_Node.Kind) -> *$R 
#modify {
    R = get_type_for_node_kind(kind);
    return R.(*Type_Info) != null, "Invalid code node kind.";
} {
    node := builder.node_stack[builder.node_stack.count-1];
    return xx ifx node.kind == kind then node;
}


begin_block :: (builder: *Code_Builder, block_type: type_of(Code_Block.block_type), block_flags: type_of(Code_Block.block_flags) = xx 0) -> *Code_Block {
    code_block := New(Code_Block);
    code_block.block_type = block_type;
    code_block.block_flags = block_flags;
    code_block.node_flags = .IS_PARENTHESIZED;
    code_block.parent = get_current_block(builder); // NOTE: may be null
    push_node_stack_block(builder, code_block);
    return code_block;
}

end_block :: (builder: *Code_Builder) -> *Code_Block {
    code_block, contents := pop_current_block(builder);
    code_block.statements = array_copy(contents,, get_allocator(builder));
    return code_block;
}

append_statement :: (builder: *Code_Builder, statement: *Code_Node) {
    // TODO: should check that parent block type accepts arbitrary statements...
    // TODO: should check the node kind to make sure it is something that can validly be used at statement level.
    push_node_stack(builder, statement);
}


append_statement_from_code :: (builder: *Code_Builder, code: Code) {
    append_statement(builder, compiler_get_nodes(code));
}


// append_declaration :: (builder: *Code_Builder, declaration: *Code_Declaration) {
//     assert(builder.index_of_current_block >= 0);
    
//     // TODO: should check the node kind to make sure it is something that can validly be used at statement level.
//     push_node_stack(builder, declaration);
// }





begin_struct :: (builder: *Code_Builder) -> *Code_Struct {
    code_struct := New(Code_Struct,, get_allocator(builder));
    push_node_stack(builder, code_struct);
    
    code_block := begin_block(builder, .DATA_DECLARATIONS);
    code_block.belongs_to_struct = code_struct;
    
    code_struct.block = code_block;
    return code_struct;
}

end_struct :: (builder: *Code_Builder) -> *Code_Struct {
    code_block := end_block(builder);
    code_struct := pop_node_stack(builder, .STRUCT);
    
    assert(code_block.belongs_to_struct == code_struct);
    assert(code_struct != null && code_struct.block == code_block);
    
    return code_struct;
}

append_struct_member :: (builder: *Code_Builder, entry: *Code_Scope_Entry, loc := #caller_location) {
    code_block := get_current_block(builder);
    assert(code_block.belongs_to_struct != null, "Attempted to append a struct member into a block which does not belong to a struct.", loc = loc);
    
    if entry.kind == {
      case .DECLARATION;
        push_node_stack(builder, entry);
        
      case;
        assert(false, "Invalid code node kind passed to append_struct_member: %", entry.kind, loc = loc);
    }
}

append_struct_member :: (builder: *Code_Builder, name: string, type_inst: *Code_Type_Instantiation = null, expression: *Code_Node = null, flags: type_of(Code_Declaration.flags) = xx 0, loc := #caller_location) {
    code_block := get_current_block(builder);
    assert(code_block.belongs_to_struct != null, "Attempted to append a struct member into a block which does not belong to a struct.", loc = loc);
    
    entry := make_declaration(name, type_inst, expression, flags);
    push_node_stack(builder, entry);
}

build_struct :: (builder: *Code_Builder, $body: Code, loc := #caller_location) -> *Code_Struct #expand {
    code_struct := begin_struct(builder);
    
    #insert body;
    
    assert(code_struct == end_struct(builder), "end_code_struct returned a different node than begin_code_struct! Did you open a child struct and forget to close it?", loc = loc);
    return code_struct;
}




/*
    Building Procedures
    
    push header
    
    in any order:
        begin/end arguments
        begin/end returns
        begin/end body
    
    end procedure
    
    TODO: refactor node stack so that we can make a proper interface for procedure arguments, returns, and constants
    
    we may need better infrastructure wrt node stack in order to handle procedures
    something more like a state stack, onto which we can also push some structure representing the current action in a richer way
    maybe we can just create faux code nodes for these state structs and use special values in code_node.kind to signify these
*/

begin_procedure_header :: (builder: *Code_Builder) -> *Code_Procedure_Header {
    header := New(Code_Procedure_Header);
    push_node_stack(builder, header);
    return header;
}

end_procedure_header :: (builder: *Code_Builder) -> *Code_Procedure_Header {
    header := pop_node_stack(builder, .PROCEDURE_HEADER);
    assert(header != null);
    return header;
}

// begin_procedure_arguments :: (builder: *Code_Builder) -> *Code_Block {
    
// }

// end_procedure_arguments :: (builder: *Code_Builder) -> *Code_Block {
    
// }

// begin_procedure_returns :: (builder: *Code_Builder) -> *Code_Block {
    
// }

// end_procedure_returns :: (builder: *Code_Builder) -> *Code_Block {
    
// }

// begin_procedure_constants :: (builder: *Code_Builder) -> *Code_Block {
    
// }

// end_procedure_constants :: (builder: *Code_Builder) -> *Code_Block {
    
// }

begin_procedure_body :: (builder: *Code_Builder, loc := #caller_location) -> *Code_Procedure_Body {
    header := peek_node_stack(builder, .PROCEDURE_HEADER);
    assert(header != null, "begin_procedure_body must be called while building a procedure header.", loc = loc);
    
    procedure_body := New(Code_Procedure_Body,, get_allocator(builder));
    procedure_body.header = header;
    push_node_stack(builder, procedure_body);
    
    header.body_or_null = procedure_body;
    
    code_block := begin_block(builder, .IMPERATIVE);
    procedure_body.block = code_block;
    return procedure_body;
}

end_procedure_body :: (builder: *Code_Builder) -> *Code_Procedure_Body {
    code_block := end_block(builder);
    procedure_body := pop_node_stack(builder, .PROCEDURE_BODY);
    assert(procedure_body != null);
    return procedure_body;
}

