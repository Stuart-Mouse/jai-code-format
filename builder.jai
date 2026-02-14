
Code_Builder :: struct {
    node_stack:                 [..] *Code_Node;
    index_of_current_block:     int = -1;
    memory_pool:                Flat_Pool;
}

get_allocator :: (builder: *Code_Builder) -> Allocator {
    return .{ flat_pool_allocator_proc, *builder.memory_pool };
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
    
    builder.index_of_current_block -= 1;
    while builder.node_stack[builder.index_of_current_block].kind != .BLOCK {
        builder.index_of_current_block -= 1;
        // assert(builder.index_of_current_block > 0, "Error: could not find parent block on node stack!");
        if builder.index_of_current_block == -1  break;
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

begin_code_struct :: (builder: *Code_Builder) -> *Code_Struct {
    code_struct := New(Code_Struct,, get_allocator(builder));
    code_block  := New(Code_Block,,  get_allocator(builder));
    
    code_struct.block = code_block;
    
    code_block.belongs_to_struct = code_struct;
    code_block.block_type = .DATA_DECLARATIONS;
    
    push_node_stack(builder, code_struct);
    push_node_stack_block(builder, code_block);
    
    return code_struct;
}

end_code_struct :: (builder: *Code_Builder) -> *Code_Struct {
    code_block, contents := pop_current_block(builder);
    code_struct := pop_node_stack(builder, .STRUCT);
    
    assert(code_block.belongs_to_struct == code_struct);
    assert(code_struct != null && code_struct.block == code_block);
    
    // TODO: should maybe validate contents?
    code_block.statements = array_copy(contents,, get_allocator(builder));
    
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

append_statement :: (builder: *Code_Builder, statement: *Code_Node) {
    assert(builder.index_of_current_block >= 0);
    
    // TODO: should check the node kind to make sure it is something that can validly be used at statement level.
    push_node_stack(builder, entry);
}

build_code_struct :: (builder: *Code_Builder, $body: Code, loc := #caller_location) -> *Code_Struct #expand {
    code_struct := begin_code_struct(builder);
    
    #insert body;
    
    assert(code_struct == end_code_struct(builder), "end_code_struct returned a different node than begin_code_struct! Did you open a child struct and forget to close it?", loc = loc);
    return code_struct;
}




