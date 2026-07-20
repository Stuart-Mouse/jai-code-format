/*
    Code_Format attempts to provide a simple interface for formatting code in a printf-like way.
    Because wildcards aren't yet a thing in the lagnuage, I am using identifiers as placeholder values.
    (I sort of assume the plan for wildcards is to allow users to do something like this.)
    The idea is that you will pass an un-typechecked Code to format_code (so that identifiers are not yet resolved)
    and then format_code will search for those identifiers and replace then with the desired replacement expressions.
    
    
    TODO: in the future, we should be able to do things like provide a search expression `a + b`
    and then replace all subexpressions that match this form with some replacement expression, which may use the same subexpressions from the original expression.
    For example:
    
    output := format_code(#code sin(a + (b * d)), { #code `x + `y, #code x - y });
    print_expression(output); // prints `sin(a - (b * d))`
    
    1. walk input search expressions to replace the backticked identifiers with Extract_Expressioes
    2. add separate logic in format_code for matching rather than formatting (maybe this should be its own procedure and we just interleave as necessary)
    3. run format_code on the insert expression to poke back in the extracted subexpressions
    
    So when we perform the match, we grab subexpression `a` as x and `(b * d)` as y.
    And in the replacement code we substitute `a` and `(b * d)` back in.
    Maybe we also would need some parameter to control whether this reformat occurs or not. Or perhaps we can mix and match by using backticked identifers to indicate that the resubsitition should occur for that particular identifier.
    
    TODO: we should design this to where formats can be done recursively, and we can specify the form of subexpressions to extract, somewhat like a where clause on a sql subquery
    maybe eventually I will even add some metaprogram stuff to transform a query DSL into a set of format rules
    
    TODO: allow user to pass a polymorphic procedure or quick lambda for the filtering function / "where clause"
          all arguments will be of type *Code_Node, 
          
    TODO: we could use `_  to indicate a wildcard that we don't actually want to extract.
    
    TODO: consider using ..`a for situations where we specifically want to extract a variable number of expressions into a wildcard, e.g. procedure arguments and compound declarations
          could also use to match compound declaration of unknown number of LHS items
          
          in where clause code, ..`a extractions should be [] *Code_Node
    
    TODO: we need to actually copy every single AST node we modify for our output, so that we don't modify the source AST 
          (otherwise, we would invalidate any extracted expressions!)
    
    TODO: once we have the more dynamic version working with exopression extracting and such, we could also create baked formatter procedures using code generation.
          then we have our fun little DSL that compiles down into a more efficient procedure that does only our specific code transformation
    
    specialized_formatter := create_formatter("replace (`c * `x) where (is_literal(`c, .NUMBER) && is_identifier(`x)) with (x / sqrt(c)");
    
    TODO: maybe instead of using a pool for memory, we should just use temp and reset the high water mark before format_code returns.

    
    NOTE:
    
    For many differnet types of AST nodes, there's a bit of amiguity as to how the match expressions should match to the input code.
    There are many nodes where child nodes are optional (e.g. names for it and it_index), or flags that aren't really matchable code nodes at all.
    For optionl nodes, the general rule is that, if a source node is null and the match is non-null, 
    
    source_code:
    ```
    for some_array { ... }
    ```
    
    match expression:
    ```
    for element, index: some_array {...}
    ```
    
    This case would not match because the match expression is more specific than the source code.
    But if we flipped the two, we would not prevent matching on the basis that the source code names the iterators and the match expression does not.
    We treat the null nodes on the match expression as an unnamed wildcard in this case.
    Perhaps we will decide later on that we should be more rigorous and enfore use of `_ in these situations, but for now that is not the case.
    
    find (for `it, `it_index: `iterated `block;)
    where (type_of(iterated) == string)
    
    
    // reverse the order of elements in compound declaration
    
    find (`..left: `type = `..right)
    where (left.count > 1 && right.count > 1)
    modify {
        left  = array_reverse(left);
        right = array_reverse(right);
    }
    replace (..left: type = ..right)
    
    
    TODO: create a code builder
        append statements within block, open block, close block etc.
    
    TODO: create procedure to format the same code with an array of insert expresisons
        this can probably just be part of the code builder
    
    TODO: before adding more stuff for formatting/replacing we should make a functional and useful code_search
    
    code search will do largely the same thing as format_code, 
    except it will collect all of the matched expressions into an array
    and we will need to implement the filter lambda stuff
    
    for now I am doing the basic version of the filter which is just to give the match expression and match code as-is
    in a perfect world we would be able to just let the user pass a polymorphic quick lambda and automatically generate the calling code to pass the extract expression nodes by name
*/

#module_parameters()(DEBUG := false);

debug_log :: (format: string, args: ..Any) {
    #if DEBUG  log(format, ..args);
}

Code_Formatter :: struct {
    // IMPORTANT NOTE: we assume that the allocator is some kind of pool and that all nodes will be freed at once.
    allocator:      Allocator;
    format_rules:   [] Node_Format_Rule;
}

Node_Format_Rule :: struct {
    match_expression:   Match_Expression;
    insert_expression:  *Code_Node;     
}

Match_Expression :: struct {
    root:                   *Code_Node;
    extracted_expressions:  [..] *Extracted_Expression;
    filter:                 (*Match_Expression, *Code_Node) -> bool;
}

Extracted_Expression :: struct {
    using #as ident:    Code_Ident;
    expression:         *Code_Node;
    // expressions:  [] *Code_Node;
}

Code_Format_Rule :: struct {
    match_expression:   Code;
    insert_expression:  Code;
    filter:             Code;
}

create_node_format_rule :: (formatter: *Code_Formatter, $code_format_rule: Code_Format_Rule, $code_to_copy_scope_from: Code) -> Node_Format_Rule {
    node_rule: Node_Format_Rule;
    
    match_expression_walk_proc :: (formatter: *Code_Formatter, code: *Code_Node, match_expression: *Match_Expression) -> Walk_Nodes_Return_Code, *Code_Node {
        code_ident := is_backticked_identifier(code);
        if code_ident {
            ext_exp := New(Extracted_Expression,, get_allocator(formatter));
            ext_exp.ident = code_ident;
            array_add(*match_expression.extracted_expressions, ext_exp);
            return .RETURN, ext_exp;
        }
        return .CONTINUE, code;
    };
    
    // make extracted expression for root
    {
        ext_exp := New(Extracted_Expression,, get_allocator(formatter));
        ext_exp.ident = make_ident("root", flags = .HAS_SCOPE_MODIFIER,, get_allocator(formatter));
        array_add(*node_rule.match_expression.extracted_expressions, ext_exp);
    }
    
    node_rule.match_expression.root = compiler_get_nodes(code_format_rule.match_expression);
    node_rule.match_expression.root = walk_nodes(
        match_expression_walk_proc, 
        formatter, 
        node_rule.match_expression.root, 
        *node_rule.match_expression,
        modify_in_place = true,
    );
    
    // TODO: We should experiment later with preprocessing the insert expression to optimize subexpression re-insertions.
    node_rule.insert_expression = compiler_get_nodes(code_format_rule.insert_expression);
    
    // NOTE: The call to generate_filter_proc is the reason why code_format_rule must be constant.
    node_rule.match_expression.filter = #ifx #run code_format_rule.filter 
        then generate_filter_proc(#run code_format_rule.filter, code_to_copy_scope_from) else null;
    
    return node_rule;
}

format_code :: ($code: Code, $format_rules: ..Code_Format_Rule, $code_to_copy_scope_from := #caller_code, $location := #caller_location) -> Code {
    memory_pool: Flat_Pool;
    defer fini(*memory_pool);
    allocator := Allocator.{ flat_pool_allocator_proc, *memory_pool };
    
    formatter: Code_Formatter;
    formatter.allocator = allocator;
    
    formatter.format_rules = NewArray(format_rules.count, Node_Format_Rule,, get_allocator(*formatter));
    // TODO: Try and make this into a code insert rather than a string insert.
    #insert -> string {
        builder: String_Builder;
        for 0..format_rules.count-1 {
            print(*builder, "formatter.format_rules[%1] = create_node_format_rule(*formatter, format_rules[%1], code_to_copy_scope_from);", it);
        }
        return builder_to_string(*builder);
    }
    
    nodes := compiler_get_nodes(code);
    nodes = format_code(*formatter, nodes);
    
    #if DEBUG {
        print("%:%,%: format_code returned:\n\n", location.fully_pathed_filename, location.line_number, location.character_number);
        write_string(sprintx(nodes,, temp));
        write_string("\n\n");
    }
    
    return compiler_get_code(nodes, code_to_copy_scope_from);
}

format_code_to_nodes :: ($code: Code, format_rules: ..Node_Format_Rule, allocator: Allocator, $location := #caller_location) -> *Code_Node {
    formatter: Code_Formatter;
    formatter.allocator    = allocator;
    formatter.format_rules = format_rules;
    
    nodes := compiler_get_nodes(code);
    nodes = format_code(*formatter, nodes);
    
    #if DEBUG {
        print("%:%,%: format_code returned:\n\n", location.fully_pathed_filename, location.line_number, location.character_number);
        write_string(sprintx(nodes,, temp));
        write_string("\n\n");
    }
    
    return nodes;
}

format_code :: (formatter: *Code_Formatter, code: *Code_Node) -> *Code_Node {
    return walk_nodes(
        proc      = try_getting_replacement_nodes, 
        formatter = formatter, 
        code      = code,
        user_data = null,
        modify_in_place = false,
        run_proc_on_return = true,
    );
}

try_getting_replacement_nodes :: (formatter: *Code_Formatter, code: *Code_Node, _: *void) -> Walk_Nodes_Return_Code, *Code_Node {
    for *rule: formatter.format_rules {
        if match_code(formatter, *rule.match_expression, code, rule.match_expression.root) {
            // set the root extracted expression before running the filter proc
            if rule.match_expression.extracted_expressions {
                rule.match_expression.extracted_expressions[0].expression = code;
            }
            
            filter := rule.match_expression.filter;
            if filter && !filter(*rule.match_expression, code) {
                return .CONTINUE, code;
            }
            return .RETURN, create_replacement_nodes(formatter, code, rule);
        }
    }
    return .CONTINUE, code;
}

match_code :: (formatter: *Code_Formatter, match_expression: *Match_Expression, code: *Code_Node, code_to_match: *Code_Node) -> bool {
    try_extract_expression :: (code: *Code_Node, code_to_match: *Code_Node) -> bool #expand {
        code_ident := is_backticked_identifier(code_to_match);
        if code_ident { 
            if code_ident.name != "_" {
                code_ident.(*Extracted_Expression).expression = code;
            }
            return true;
        }
        return false;
    }
    
    if code_to_match == null {
        debug_log("Warning: `code_to_match` passed to match_code was null. Stack trace:");
        #if DEBUG  log_stack_trace(context.stack_trace, xx 0);
        return false;
    }
    
    // Check if this expression needs to be extracted.
    // NOTE: This is done before the null check on `code` so that we can match a wildcard to a null node.
    if try_extract_expression(code, code_to_match)  return true;
    
    if code == null {
        debug_log("Warning: `code` passed to match_code was null. Stack trace:");
        #if DEBUG  log_stack_trace(context.stack_trace, xx 0);
        return false;
    }
    
    get_code_and_match_as :: ($T: Type) -> *T, *T #expand {
        if code_to_match.kind != code.kind  `return false;
        return code.(*T), code_to_match.(*T);
    }
    
    match :: (code: *Code_Node, code_to_match: *Code_Node) -> bool #expand {
        return (code == null && code_to_match == null)
            || match_code(formatter, match_expression, code, code_to_match);
    }
    
    if #complete code.kind == {
      case .IDENT;
        // log("checking for match on identifier: %", code.(*Code_Ident).name);
        // log("code_to_match: %", code_to_match.*);
        _code, _match := get_code_and_match_as(Code_Ident);
        // log("comapring identifier: % == % -> %", _code.name, _match.name, _code.name == _match.name);
        return _code.name == _match.name;
        
      case .BINARY_OPERATOR;
        _code, _match := get_code_and_match_as(Code_Binary_Operator);
        
        // TODO: could we somehow indicate a wildcard binary operator? 
        // Maybe we can use the operator keyword in the match expression? (maybe too hacky...)
        if _code.operator_type != _match.operator_type  return false;
        if !match(_code.left,  _match.left )  return false;
        if !match(_code.right, _match.right)  return false;
        return true;
        
      case .UNARY_OPERATOR;
        _code, _match := get_code_and_match_as(Code_Unary_Operator);
        if _code.operator_type != _match.operator_type  return false;
        if !match(_code.subexpression, _match.subexpression)  return false;
        return true;
        
      case .PROCEDURE_CALL;
        // _code  := code.(*Code_Procedure_Call);
        // _match := code_to_match.(*Code_Procedure_Call);
        
        // if !match_code(formatter, match_expression, _code.procedure_expression, _match.procedure_expression)  return false;
        
        // TODO: we can't use arguments_sorted since most likely our _code and _match nodes are not yet typechecked.
        //       Maybe in the future I can revisit this and handle things better if odes are typechecked.
        //       Unfortunately, this means that we can really only match arguments lexically.
        /*
            Considerations:
            
            For now we should probably not permit mixing of named an unnamed arguments in the search expression.
            If we use unnamed arguments, then we just match arguments by index.
            If named arguments are used, then we care only about matching the named arguments.
            We will need to check this in the search expression(s) when we create the formatter.
        */
        
      case .LITERAL;
        _code, _match := get_code_and_match_as(Code_Literal);
        
        if _code.value_type  != _match.value_type   return false;
        if _code.value_flags != _match.value_flags  return false;
        
        if _code.value_type == {
          case .UNINITIALIZED;  assert(false, "Uninitialized Code_Literal in match_code."); // TODO: add source location info.
          case .NUMBER;
            return ifx _code.value_flags & .FLOAT 
                then _code._float64 == _match._float64
                else _code._u64     == _match._u64;
            
          case .STRING;
            return _code._string == _match._string;
            
          case .BOOLEAN;
            return _code._u64 == _match._u64;
            
          case .ARRAY;
            // TODO
            return false;
            
          case .STRUCT;
            // TODO
            return false;
            
          case .POINTER;
            // TODO
            return false;
            
          case .TYPE_INFO;
            return match(_code.type_info_literal_defn, _match.type_info_literal_defn);
        }
        
      case .RETURN;
        _code, _match := get_code_and_match_as(Code_Return);
        if _code.arguments_unsorted.count != _match.arguments_unsorted.count  return false;
        for _code.arguments_unsorted {
            if !match(it.expression, _match.arguments_unsorted[it_index].expression)  return false;
        }
        return true;
        
      case .MAKE_VARARGS;
        _code, _match := get_code_and_match_as(Code_Make_Varargs);
        if _code.element_type && _match.element_type 
        && _code.element_type != _match.element_type {
            return false;
        }
        if _code.expressions.count != _match.expressions.count  return false;
        for _code.expressions {
            if !match(it, _match.expressions[it_index])  return false;
        }
        return true;
        
      case .CASE;
        _code, _match := get_code_and_match_as(Code_Case);
        if !match(_code.condition,  _match.condition )  return false;
        if !match(_code.then_block, _match.then_block)  return false;
        // TODO: compare based on fallthrough?
        return true;
        
      case .CAST;
        _code, _match := get_code_and_match_as(Code_Cast);
        if !match(_code.target_type, _match.target_type)  return false;
        if !match(_code.expression,  _match.expression)   return false;
        return true;
        
      case .LOOP_CONTROL;
        _code, _match := get_code_and_match_as(Code_Loop_Control);
        if _code.control_type != _match.control_type  return false;
        if !match(_code.target_ident,  _match.target_ident)   return false;
        
      case .BLOCK;
        // I am not sure what it would mean to match a block to another block.
        // Seems like you'd typically just extract a block via a wildcard, 
        // and then once we have a filter proc, the user can manually discriminate based on the contents of the block.
        // In theory we could have some special syntax to match block contents oir find blocks containing particular subexpressions,
        // but that's a lower priority right now and I'm not sure how best to implement that at the moment.
        
      case .FOR;
        // _code, _match := get_code_and_match_as(Code_For);
        
        // if !match(_code.want_replacement_for_expansion, _match.want_replacement_for_expansion)  return false;
        
        // add_it       := false;
        // add_it_index := false;
        // if f.ident_it && (f.ident_it.name != "it" || (f.ident_it.flags & (.DEFINES_POLYMORPH_VARIABLE | .HAS_SCOPE_MODIFIER))) {
        //     add_it = true;
        // }
        // if f.ident_it_index && (f.ident_it_index.name != "it_index"  || (f.ident_it_index.flags & (.DEFINES_POLYMORPH_VARIABLE | .HAS_SCOPE_MODIFIER))) {
        //     add_it = true;
        //     add_it_index = true;
        // }
        
        // if _code.iteration_expression_right {
        //     // TODO: special case for when _match.iteration_expression is wildcard
        //     //       will require modifications to Extracted_Expression to handle multi-mode cases...
        //     if !match(_code.iteration_expression,       _match.iteration_expression)        return false;
        //     if !match(_code.iteration_expression_right, _match.iteration_expression_right)  return false;
        // } else {
        //     if !match(_code.iteration_expression, _match.iteration_expression)  return false;
        // }
        
        
      case .WHILE;
        _code, _match := get_code_and_match_as(Code_While);
        // TODO: maybe have special handling here for when condition is a declaration, so that we can still match named while loops based only on the actual condition.
        if !match(_code.condition,  _match.condition)  return false;
        if !match(_code.block,      _match.block)      return false;
        return true;
        
      case .IF;
        _code, _match := get_code_and_match_as(Code_If);
        if _code.if_flags != _match.if_flags  return false; // maybe reconsider this
        if !match(_code.condition,  _match.condition )  return false;
        if !match(_code.then_block, _match.then_block)  return false;
        if !match(_code.else_block, _match.else_block)  return false;
        return true;
        
      case .DEFER;
        _code, _match := get_code_and_match_as(Code_Defer);
        return match(_code.block, _match.block);
        
      case .USING;
        _code, _match := get_code_and_match_as(Code_Using);
        return match(_code.expression, _match.expression)
            && match(_code.filter_expression, _match.filter_expression);
        
      case .DECLARATION;
        _code, _match := get_code_and_match_as(Code_Declaration);
        if _match.flags & .HAS_SCOPE_MODIFIER {
            // TODO: allow extracting the name here... will require extract_expression changes...
            
        } else {
            if _code.name != _match.name  return false;
        }
        if !match(_code.type_inst,  _match.type_inst)   return false;
        if !match(_code.expression, _match.expression)  return false;
        return true;
        
      case .COMPOUND_DECLARATION;
        // _code := code.(*Code_Compound_Declaration);
        // if _match.kind == {
        //   case .COMPOUND_DECLARATION;
        //     _match := code_to_match.(*Code_Compound_Declaration);
        //     if _code.operator_type.count != _match.operator_type.count  return false;
        //     if !match(_code.declaration_properties, _match.declaration_properties)  return false;
        //     // TODO: alignment expression?
        //     // TODO: notes? (user should handle...)
            
        //     if _code.comma_separated_assignment.count != _match.comma_separated_assignment.count  return false;
        //     for _code.comma_separated_assignment {
        //         if !match(it.expression, _match.comma_separated_assignment[it_index].expression)  return false;
        //     }
            
        //   case .DECLARATION;
        //     _match := code_to_match.(*Code_Declaration);
            
        //   case .BINARY_OPERATOR;
        //     _match := code_to_match.(*Code_Binary_Operator);
            
            
        // }
        
      case .CONTEXT;
        _code, _match := get_code_and_match_as(Code_Type_Query);
        return true;
        
      case .COMMA_SEPARATED_ARGUMENTS;
        // NOTE: only show up in compound declaration. 
        
      case .TYPE_INSTANTIATION;
        // Very recursive! Lots to consdier as well! But should ultimately be mostly straightforward... (edge cases will probably abound though)
        _code, _match := get_code_and_match_as(Code_Type_Instantiation);
        
        // TODO: consider whether this is actually a good idea or not! Maybe we need flags for this!
        // Take a shortcut and compare the type infos directly if we have type info for both _code and _match.
        if _code.result && _match.result && (_code.result == _match.result)  return true;
        
        FLAGS_THAT_MUST_MATCH :: Code_Type_Instantiation.inst_flags.VARARGS | .TYPE_OF_PROCEDURE | .INTERFACE | .TYPE_DIRECTIVE;
        if (_code.inst_flags & FLAGS_THAT_MUST_MATCH) != (_match.inst_flags & FLAGS_THAT_MUST_MATCH)  return false;
        
        if _code.pointer_to {
            return match(_code.pointer_to, _match.pointer_to);
        }
        
        if _code.array_element_type {
            // TODO: special case for wildcard _match.array_dimension
            ARRAY_FLAGS_THAT_MUST_MATCH :: Code_Type_Instantiation.inst_flags.RESIZABLE | .ARRAY_VIEW;
            if (_code.inst_flags & ARRAY_FLAGS_THAT_MUST_MATCH) != (_code.inst_flags & ARRAY_FLAGS_THAT_MUST_MATCH)  return false;
            if _code.array_dimension != _match.array_dimension  return false;
            return match(_code.array_element_type, _match.array_element_type);
        }
        
        if _code.type_directive_target {
            return match(_code.type_directive_target, _match.type_directive_target);
        }
        
        if _code.must_implement {
            return match(_code.must_implement, _match.must_implement);
        }
        
        // TODO: if type_value_expression is an ident, we want to return the type inst node as the matched node rather than the identifier?
        
        return match(_code.type_valued_expression, _match.type_valued_expression);
        
      case .STRUCT;
        // Probably more useful to allow recursive search to match particular subsexpressions. Same can be said for blocks, I suppose.
        
      case .ENUM;
        // See above.
        
      case .PROCEDURE_HEADER;
        // Many different way we may want to match procedure header. Should write notes about that here and also consider what kind of substitutions we will want to do. I think I left some notes in walk_nodes that are relevant.
        
      case .TYPE_QUERY;
        _code, _match := get_code_and_match_as(Code_Type_Query);
        if _code.query_kind != _match.query_kind  return false;
        if !match(_code.type_to_query, _match.type_to_query)  return false;
        return true;
        
      case .EXPRESSION_QUERY;
        _code, _match := get_code_and_match_as(Code_Expression_Query);
        if _code.query_kind != _match.query_kind  return false;
        if !match(_code.expression_to_query, _match.expression_to_query)  return false;
        return true;
        
      case .DIRECTIVE_EXISTS;
        _code, _match := get_code_and_match_as(Code_Directive_Exists);
        if !match(_code.query_expression, _match.query_expression)  return false;
        if !match(_code.sync_expression, _match.sync_expression)  return false;
        return true;
        
      case .DIRECTIVE_ADD_CONTEXT;
        _code, _match := get_code_and_match_as(Code_Directive_Add_Context);
        return match(_code.expression, _match.expression);
        
      case .DIRECTIVE_OVERLAY;
        _code, _match := get_code_and_match_as(Code_Directive_Overlay);
        if !match(_code.target_expression, _match.target_expression)  return false;
        if !match(_code.declaration_or_using, _match.declaration_or_using)  return false;
        return true;
        
      case .DIRECTIVE_CODE;
        _code, _match := get_code_and_match_as(Code_Directive_Code);
        if _code.code_flags != _match.code_flags  return false;
        if !match(_code.expression, _match.expression)  return false;
        return true;
        
      case .DIRECTIVE_PROCEDURE_NAME;
        _code, _match := get_code_and_match_as(Code_Directive_Procedure_Name);
        return match(_code.argument, _match.argument);
        
      case .DIRECTIVE_BYTES;
        _code, _match := get_code_and_match_as(Code_Directive_Bytes);
        return match(_code.expression, _match.expression);
        
      case .DIRECTIVE_LOCATION;
        _code, _match := get_code_and_match_as(Code_Directive_Location);
        if _code.is_caller_location != _match.is_caller_location  return false;
        if !match(_code.expression, _match.expression)  return false;
        return true;
        
      case .DIRECTIVE_BAKE;
        _code, _match := get_code_and_match_as(Code_Directive_Bake);
        if _code.bake_type !=  _match.bake_type  return false;
        if !match(_code.procedure_call, _match.procedure_call)  return false;
        return true;
        
      // Match based only on node kind:
        
      case .DIRECTIVE_THIS;
        return code_to_match.kind == .DIRECTIVE_THIS;
        
      case .DIRECTIVE_CONTEXT_TYPE;
        return code_to_match.kind == .DIRECTIVE_CONTEXT_TYPE;
        
      case .DIRECTIVE_THROUGH;
        return code_to_match.kind == .DIRECTIVE_THROUGH;
        
      case .DIRECTIVE_COMPILE_TIME;
        return code_to_match.kind == .DIRECTIVE_COMPILE_TIME;
        
      // Need some thinking:
        
      case .DIRECTIVE_INSERT;       // Lots of things to consider...
      case .RESOLVED_OVERLOAD;      // May have to just strip this up top for both code and code_to_match, sort of like what we do for variant types in Convert
      case .DIRECTIVE_RUN;          // Will have to match based on procedure header..? Not intuitive when just an expression...
      case .DIRECTIVE_IMPORT;       // No reasonable way to match name at the moment.
      case .DIRECTIVE_LOAD;         // No reasonable way to match name at the moment.
      case .DIRECTIVE_LIBRARY;      // No reasonable way to match name at the moment.
      case .DIRECTIVE_POKE_NAME;    // May get removed form language, supposedly.
      case .DIRECTIVE_MODIFY;
      case .EXTRACT;
      case .NOTE;
      case .PUSH_CONTEXT;
      case .ASM;
      
      // Unmatchable: 
      
      case .DIRECTIVE_MODULE_PARAMETERS;
      case .DIRECTIVE_WILDCARD; // Not implemented in the language.
      case .DIRECTIVE_SCOPE;    
      case .DIRECTIVE_PLACE;    // Will be removed soon.
      case .TYPE_DEFINITION;
      case .PLACEHOLDER;        // We aren't given the identifier.
      case .PROCEDURE_BODY;
      
      case .UNINITIALIZED;               #through;
      case;
        assert(false, "Invalid code node kind: %", code.kind);
    }
    
    return false;
}


#scope_module

#load "utils.jai";
#load "builder.jai";

#import "Compiler";
#import "Basic";
#import "String";
#import "Flat_Pool";    // TODO: probably just replace with Pool or temp allocator.

Program_Print :: #import "Program_Print";
Code_Visit    :: #import "Code_Visit";

// TODO: module/program parameters for quick lambda namespace struct




is_backticked_identifier :: (node: *Code_Node) -> *Code_Ident {
    ident := node.(*Code_Ident);
    return ifx ident.kind == .IDENT && (ident.flags & .HAS_SCOPE_MODIFIER) then ident;
}

create_replacement_nodes :: (formatter: *Code_Formatter, expression: *Code_Node, rule: Node_Format_Rule) -> *Code_Node {
    result: *Code_Node;
    if rule.match_expression.extracted_expressions {
        // We need to override the current format rules so that we can run the formatter on the insert expression.
        // So we back up the original rules and restore them  when we return.
        insert_formatter: Code_Formatter;
        insert_formatter.allocator = formatter.allocator;
        insert_formatter.format_rules = create_format_rules_from_extract_expressions(*insert_formatter, rule.match_expression.extracted_expressions);
        result = format_code(*insert_formatter, rule.insert_expression);
        
        // TODO: Improve contextual awareness so we can provide better logs. 
        //       I would like to log the replacements for even simple formats when modifying the input code, but the result can be very spammy.
        //       And if we are replacing subexpressions in the insert expression, that should be logged differently.
        //       Perhaps we should just collect all of the subexpressions that get matched/replaced and log them at the end.
        debug_log("Replacing subexpression (%) with (%)", sprintx(expression), sprintx(result));
        debug_log("    Rule matched: (%) -> (%)", sprintx(rule.match_expression.root), sprintx(rule.insert_expression));
        for rule.match_expression.extracted_expressions {
            if it.expression {
                debug_log("% -> %", it.ident.name, sprintx(it.expression));
            }
        }
    } else {
        result = rule.insert_expression;
    }
    return result;
}

create_format_rules_from_extract_expressions :: (formatter: *Code_Formatter, extracted_expressions: [] *Extracted_Expression) -> [] Node_Format_Rule {
    // TODO: maybe allocate rules using temp instead of node memory pool?
    rules := NewArray(extracted_expressions.count, Node_Format_Rule,, get_allocator(formatter));
    for extracted_expressions {
        rules[it_index] = .{
            match_expression = .{
                // We can't use the existing identifier because it is backticked, 
                // so we just make a new one for the match expression.
                root = make_ident(it.name,, get_allocator(formatter)),
            },
            insert_expression = it.expression,
        };
    }
    return rules;
}

get_type_for_node_kind :: (kind: Code_Node.Kind) -> Type {
    type: Type;
    if #complete kind == {
      case .LITERAL;                     type = Code_Literal;
      case .BLOCK;                       type = Code_Block;
      case .IDENT;                       type = Code_Ident;
      case .BINARY_OPERATOR;             type = Code_Binary_Operator;
      case .UNARY_OPERATOR;              type = Code_Unary_Operator;
      case .PROCEDURE_CALL;              type = Code_Procedure_Call;
      case .WHILE;                       type = Code_While;
      case .FOR;                         type = Code_For;
      case .LOOP_CONTROL;                type = Code_Loop_Control;
      case .IF;                          type = Code_If;
      case .CASE;                        type = Code_Case;
      case .COMMA_SEPARATED_ARGUMENTS;   type = Code_Comma_Separated_Arguments;
      case .RESOLVED_OVERLOAD;           type = Code_Resolved_Overload;
      case .CAST;                        type = Code_Cast;
      case .TYPE_INSTANTIATION;          type = Code_Type_Instantiation;
      case .RETURN;                      type = Code_Return;
      case .DIRECTIVE_THIS;              type = Code_Node; //  There is no Code_Directive_This
      case .DIRECTIVE_ADD_CONTEXT;       type = Code_Directive_Add_Context;
      case .DIRECTIVE_CONTEXT_TYPE;      type = Code_Directive_Context_Type;
      case .DIRECTIVE_OVERLAY;           type = Code_Directive_Overlay;
      case .DIRECTIVE_PROCEDURE_NAME;    type = Code_Directive_Procedure_Name;
      case .DIRECTIVE_COMPILE_TIME;      type = Code_Node; //  There is no Code_Directive_Compile_Time
      case .DIRECTIVE_CODE;              type = Code_Directive_Code;
      case .DIRECTIVE_BYTES;             type = Code_Directive_Bytes;
      case .STRUCT;                      type = Code_Struct;
      case .ENUM;                        type = Code_Enum;
      case .CONTEXT;                     type = Code_Context;
      case .PUSH_CONTEXT;                type = Code_Push_Context;
      case .TYPE_QUERY;                  type = Code_Type_Query;
      case .EXPRESSION_QUERY;            type = Code_Expression_Query;
      case .DIRECTIVE_LOCATION;          type = Code_Directive_Location;
      case .DIRECTIVE_THROUGH;           type = Code_Directive_Through;
      case .DIRECTIVE_RUN;               type = Code_Directive_Run;
      case .DIRECTIVE_LIBRARY;           type = Code_Directive_Library;
      case .DIRECTIVE_BAKE;              type = Code_Directive_Bake;
      case .PROCEDURE_HEADER;            type = Code_Procedure_Header;
      case .TYPE_DEFINITION;             type = Code_Type_Definition;
      case .DIRECTIVE_IMPORT;            type = Code_Directive_Import;
      case .DEFER;                       type = Code_Defer;
      case .USING;                       type = Code_Using;
      case .COMPOUND_DECLARATION;        type = Code_Compound_Declaration;
      case .ASM;                         type = Code_Asm;
      case .DIRECTIVE_INSERT;            type = Code_Directive_Insert;
      case .DIRECTIVE_PLACE;             //type = Code_Directive_Place;
      case .DIRECTIVE_MODULE_PARAMETERS; type = Code_Directive_Module_Parameters;
      case .NOTE;                        type = Code_Note;
      case .DIRECTIVE_WILDCARD;          type = Code_Directive_Wildcard;
      case .DIRECTIVE_EXISTS;            type = Code_Directive_Exists;
      case .PROCEDURE_BODY;              type = Code_Procedure_Body;
      case .DECLARATION;                 type = Code_Declaration;
      case .DIRECTIVE_MODIFY;            type = Code_Directive_Modify;
      case .EXTRACT;                     type = Code_Extract;
      case .DIRECTIVE_SCOPE;             type = Code_Directive_Scope;
      case .MAKE_VARARGS;                type = Code_Make_Varargs;
      case .DIRECTIVE_POKE_NAME;         type = Code_Directive_Poke_Name;
      case .DIRECTIVE_LOAD;              type = Code_Directive_Load;
      case .PLACEHOLDER;                 type = Code_Placeholder;
      
      case .UNINITIALIZED;               #through;
      case;
        assert(false, "Invalid code node kind: %", kind);
    }
    return type;
}

get_node_type_info :: (node: *Code_Node) -> *Type_Info {
    type := get_type_for_node_kind(node.kind).(*Type_Info);
    return type;
}

// TODO: what to do about serial numbers?
shallow_copy_node :: (from: *Code_Node) -> *Code_Node {
    node_info := get_node_type_info(from);
    copy := alloc(node_info.runtime_size);
    memcpy(copy, from, node_info.runtime_size);
    return copy;
}

get_allocator :: (formatter: *Code_Formatter) -> Allocator {
    return formatter.allocator;
}

is_this_yours :: (formatter: *Code_Formatter, memory: *void) -> bool {
    return formatter.allocator.proc(.IS_THIS_YOURS, 0, 0, memory, formatter.allocator.data).(bool);
}

copy_to_pool :: (formatter: *Code_Formatter, node: *Code_Node) -> *Code_Node {
    if is_this_yours(formatter, node)  return node;
    return shallow_copy_node(node,, get_allocator(formatter));
}

copy_to_pool :: (formatter: *Code_Formatter, data: *$T) -> *T {
    if is_this_yours(*formatter, data)  return data;
    
    SIZE :: size_of(T);
    copy := alloc(SIZE,, get_allocator(formatter));
    memcpy(copy, data, SIZE);
    return copy;
}

// TODO: should we call shallow_copy_node when T is a Code Node type?
copy_to_pool :: inline (formatter: *Code_Formatter, array: []$T) -> []T {
    if is_this_yours(formatter, array.data)  return array;
    
    ELEMENT_SIZE :: size_of(T);
    copy := alloc(array.count * ELEMENT_SIZE,, get_allocator(formatter));
    memcpy(copy, array.data, array.count * ELEMENT_SIZE);
    return ([]T).{ count = array.count, data = copy };
}

sprintx :: (root: *Code_Node) -> string {
    debug_builder: String_Builder;
    Program_Print.print_expression(*debug_builder, root);
    return builder_to_string(*debug_builder);
}

Walk_Nodes_Return_Code :: enum {
    ERROR       :: -1;
    CONTINUE    ::  0;
    RETURN      ::  1;
}

// NOTE: Restricting this to #scope_module because it's very much specific to the way we walk n this module, probably not so useful for generas.
// The only reason I even have this in the first place is because we have similar needs between format_code and create_match_expression, insofar as we need to walk odes in the same way and conditionally replacs.
walk_nodes :: (
    $proc:              (*Code_Formatter, *Code_Node, *$T) -> (Walk_Nodes_Return_Code, *Code_Node), 
    formatter:          *Code_Formatter, 
    code:               *Code_Node, 
    user_data:          *T,
    $modify_in_place    := false,
    $run_proc_on_return := false,
) -> (
    replacement_code:   *Code_Node
) {
    // So we don't have to pass proc at the call every site.
    walk :: (code: *Code_Node) -> *Code_Node #expand {
        return walk_nodes(proc, formatter, code, user_data, modify_in_place, run_proc_on_return);
    }
    
    if code == null {
        debug_log("Warning: node passed to walk_nodes. Stack trace:");
        #if DEBUG  log_stack_trace(context.stack_trace, xx 0);
        return null;
    }
    
    
    
    // NOTE: `code` is replaced with return value of proc unconditionally!
    #if !run_proc_on_return {
        return_code:, code = proc(formatter, code, user_data);
        if #complete return_code == {
          case .CONTINUE;   // no op
          case .ERROR;      return null;
          case .RETURN;     return code;
        }
    }
    
    __get_as :: (c: *Code_Node, $T: Type) -> *T #expand { 
        #if modify_in_place {
            return c.(*T);
        } else {
            copy := c.(*T).*;
            return *copy;
        }
    }
    
    __return :: (result: *Code_Node) #expand {
        #if run_proc_on_return {
            return_code, new := proc(formatter, result, user_data);
            if #complete return_code == {
              case .CONTINUE;   // no op
              case .ERROR;      `return null;
              case .RETURN;
                result = xx new;
            }
        }
        
        #if modify_in_place {
            ensure_correctness(result);
            `return result;
        } else {
            // debug_log("Checking result % for modifications on return:", T);
            // debug_log("   original: %", code.(*T).*);
            // debug_log("   returned: %", result.*);
            // If we have modified the node at all, we will copy the modified node from the stack to the formatter's memory pool and return the copy.
            node_size := get_node_type_info(result).runtime_size;
            if result.kind != code.kind 
            || memcmp(result, code, node_size) != 0 {
                // debug_log("   returning new code...");
                result = copy_to_pool(formatter, result);
                ensure_correctness(result);
                `return result;
            }
            // Otherwise, return the original code node.
            `return code;
        }
    }
    
    // TODO: clean up this since we now modify in place as well
    // macro to help with arrays of nodes, where we only want to create a copy of the array if we are actually going to modify it.
    copy_on_write :: (array: *[]$T, body: Code, flags: For_Flags) #expand {
        #if modify_in_place {
            for index: 0..array.count-1 {
                `it := array.*[index];
                `it_index := index;
                
                defer array.*[index] = it;
                
                #insert body;
            }
        } else {
            working, original := array.*;
            for index: 0..array.count-1 {
                `it := working[index];
                `it_index := index;
                
                defer if memcmp(*it, *working[index], size_of(T)) {
                    if working.data == original.data {
                        working = copy_to_pool(formatter, original);
                    }
                    working[index] = it;
                }
                
                #insert body;
            }
            if working.data != original.data {
                array.* = working;
            }
        }
    }
    
    if #complete code.kind == {
      case .BLOCK;
        code_block := __get_as(code, Code_Block);
        assert(code_block.block_type == .IMPERATIVE);
        for:copy_on_write code_block.statements {
            it = walk(it);
        }
        __return(code_block);
        
      case .BINARY_OPERATOR;
        code_binary_operator := __get_as(code, Code_Binary_Operator);
        code_binary_operator.left  = walk(code_binary_operator.left);
        code_binary_operator.right = walk(code_binary_operator.right);
        __return(code_binary_operator);
        
      case .UNARY_OPERATOR;
        code_unary_operator := __get_as(code, Code_Unary_Operator);
        code_unary_operator.subexpression = walk(code_unary_operator.subexpression);
        __return(code_unary_operator);
        
      case .PROCEDURE_CALL;
        code_procedure_call := __get_as(code, Code_Procedure_Call);
        for:copy_on_write code_procedure_call.arguments_unsorted {
            if it.name  it.name = xx walk(it.name);
            it.expression = walk(it.expression);
        }
        __return(code_procedure_call);
        
      case .WHILE;
        code_while := __get_as(code, Code_While);
        code_while.condition = walk(code_while.condition);
        code_while.block = xx walk(code_while.block);
        __return(code_while);
        
      case .FOR;
        code_for := __get_as(code, Code_For);
        add_it       := false;
        add_it_index := false;
        if code_for.ident_it && (code_for.ident_it.name != "it" || (code_for.ident_it.flags & (.DEFINES_POLYMORPH_VARIABLE | .HAS_SCOPE_MODIFIER))) {
            add_it = true;
        }
        if code_for.ident_it_index && (code_for.ident_it_index.name != "it_index" || (code_for.ident_it_index.flags & (.DEFINES_POLYMORPH_VARIABLE | .HAS_SCOPE_MODIFIER))) {
            add_it = true;
            add_it_index = true;
        }
        
        if add_it {
            code_for.ident_it = xx walk(code_for.ident_it);
            if add_it_index {
                code_for.ident_it_index = xx walk(code_for.ident_it_index);
            }
        }
        code_for.block = xx walk(code_for.block);
        __return(code_for);
        
      case .LOOP_CONTROL;
        code_loop_control := __get_as(code, Code_Loop_Control);
        if code_loop_control.target_ident {
            code_loop_control.target_ident = xx walk(code_loop_control.target_ident);
        }
        __return(code_loop_control);
        
      case .IF;
        code_if := __get_as(code, Code_If);
        // TODO: check if flags as part of matching to search expression
        code_if.condition = walk(code_if.condition);
        if code_if.then_block {
            code_if.then_block = xx walk(code_if.then_block);
        }
        if code_if.else_block {
            code_if.else_block = xx walk(code_if.else_block);
        }
        __return(code_if);
        
      case .CASE;
        code_case := __get_as(code, Code_Case);
        if code_case.condition {
            code_case.condition = xx walk(code_case.condition);
        }
        code_case.then_block = xx walk(code_case.then_block);
        __return(code_case);
        
      case .COMMA_SEPARATED_ARGUMENTS;
        code_arguments := __get_as(code, Code_Comma_Separated_Arguments);
        for:copy_on_write code_arguments.arguments {
            it.node = walk(it.node);
        }
        __return(code_arguments);
        
      case .CAST;
        code_cast := __get_as(code, Code_Cast);
        code_cast.target_type = xx walk(code_cast.target_type);
        code_cast.expression  =    walk(code_cast.expression);
        __return(code_cast);
        
      case .TYPE_INSTANTIATION;
        code_type_inst := __get_as(code, Code_Type_Instantiation);
        
        if code_type_inst.pointer_to {
            code_type_inst.pointer_to = xx walk(code_type_inst.pointer_to);
        } else if code_type_inst.array_element_type {
            code_type_inst.array_element_type = xx walk(code_type_inst.array_element_type);
        } else {
            code_type_inst.type_valued_expression = walk(code_type_inst.type_valued_expression);
        }
        __return(code_type_inst);
        
      case .DIRECTIVE_ADD_CONTEXT;
        code_add_context := __get_as(code, Code_Directive_Add_Context);
        code_add_context.expression = walk(code_add_context.expression);
        __return(code_add_context);
        
      case .DIRECTIVE_OVERLAY;
        code_overlay := __get_as(code, Code_Directive_Overlay);
        code_overlay.target_expression    = walk(code_overlay.target_expression);
        code_overlay.declaration_or_using = walk(code_overlay.declaration_or_using);
        __return(code_overlay);
        
      case .DIRECTIVE_PROCEDURE_NAME;
        code_procedure_name := __get_as(code, Code_Directive_Procedure_Name);
        if code_procedure_name.argument  walk(code_procedure_name.argument);
        __return(code_procedure_name);
        
      case .PROCEDURE_HEADER;
        code_procedure_header := __get_as(code, Code_Procedure_Header);
        for:copy_on_write code_procedure_header.arguments {
            it = xx walk(it);
        }
        for:copy_on_write code_procedure_header.returns {
            it = xx walk(it);
        }
        if code_procedure_header.body_or_null {
            code_procedure_header.body_or_null = xx walk(code_procedure_header.body_or_null);
        }
        __return(code_procedure_header);
        
      case .PROCEDURE_BODY;
        code_procedure_body := __get_as(code, Code_Procedure_Body);
        code_procedure_body.block = xx walk(code_procedure_body.block);
        __return(code_procedure_body);
        
      case .DECLARATION;
        code_declaration := __get_as(code, Code_Declaration);
        if code_declaration.type_inst {
            code_declaration.type_inst = xx walk(code_declaration.type_inst);
        }
        if code_declaration.expression {
            code_declaration.expression = walk(code_declaration.expression);
        }
        __return(code_declaration);
        
      case .RESOLVED_OVERLOAD;
        code_resolved_overload := __get_as(code, Code_Resolved_Overload);
        code_resolved_overload.source_expression = walk(code_resolved_overload.source_expression);
        __return(code_resolved_overload);
        
      case .RETURN;
        code_return := __get_as(code, Code_Return);
        for:copy_on_write code_return.arguments_unsorted {
            if it.name  it.name = xx walk(it.name);
            it.expression = walk(it.expression);
        }
        __return(code_return);
        
      case .DEFER;
        code_defer := __get_as(code, Code_Defer);
        code_defer.block = xx walk(code_defer.block);
        __return(code_defer);
        
      case .USING;
        code_using := __get_as(code, Code_Using);
        code_using.expression = walk(code_using.expression);
        code_using.filter_expression = walk(code_using.filter_expression);
        __return(code_using);
        
      case .MAKE_VARARGS;
        code_make_varargs := __get_as(code, Code_Make_Varargs);
        for:copy_on_write code_make_varargs.expressions {
            it = walk(it);
        }
        __return(code_make_varargs);
        
      case .PUSH_CONTEXT;
        code_push_context := __get_as(code, Code_Push_Context);
        code_push_context.to_push = walk(code_push_context.to_push);
        code_push_context.block = xx walk(code_push_context.block);
        __return(code_push_context);
        
      case .COMPOUND_DECLARATION;
        _code := __get_as(code, Code_Compound_Declaration);
        _code.comma_separated_assignment = xx walk(_code.comma_separated_assignment);
        _code.declaration_properties = xx walk(_code.declaration_properties);
        _code.alignment_expression = xx walk(_code.alignment_expression);
        // TODO: will require special handling for operator type...
        //       If operator type is nonzero here, we probably want to treat this node type as interchangeable with a binary operator, since they are syntactically the same.
        //       And for example, we would probably want a search expresison like (`a += `b) to match to a compound declaration with the same operator type.
        
      case .STRUCT;
        _code := __get_as(code, Code_Struct);
        for:copy_on_write _code.modify_directives {
            it = xx walk(it);
        }
        _code.block           = xx walk(_code.block);
        _code.arguments_block = xx walk(_code.arguments_block);
        _code.constants_block = xx walk(_code.constants_block);
        // TODO: Should we format notes? I dunno about that...
        // NOTE: I am assuming defined_type is ignored on import. If not, then we would need to zero it out if the node's been modified.
        __return(_code);
        
      case .ENUM;
        _code := __get_as(code, Code_Enum);
        _code.internal_type_inst = xx walk(_code.internal_type_inst);
        // TODO: how to modify internal type? Does this get set even for untyped nodes since, presumably, the internal type should be a primitive?
        // _code.internal_type      = xx walk(_code.internal_type);
        // _code.external_type      = xx walk(_code.external_type);
        _code.block              = xx walk(_code.block);
        // TODO: Should we format notes? I dunno about that...
        __return(_code);
        
      case .TYPE_QUERY;
        _code := __get_as(code, Code_Type_Query);
        _code.type_to_query = xx walk(_code.type_to_query);
        __return(_code);
        
      case .EXPRESSION_QUERY;
        _code := __get_as(code, Code_Expression_Query);
        _code.expression_to_query = walk(_code.expression_to_query);
        __return(_code);
        
      case .EXTRACT;
        code_extract := __get_as(code, Code_Extract);
        code_extract.from = walk(code_extract.from);
        // TODO: need special handling for index?
        // code_extract.from = walk(code_extract.from);
        __return(code_extract);
        
      case .DIRECTIVE_CODE;
        code_code := __get_as(code, Code_Directive_Code);
        code_code.expression = walk(code_code.expression);
        __return(code_code);
        
      case .DIRECTIVE_BYTES;
        code_bytes := __get_as(code, Code_Directive_Bytes);
        code_bytes.expression = walk(code_bytes.expression);
        __return(code_bytes);
        
      case .DIRECTIVE_MODIFY;
        code_modify := __get_as(code, Code_Directive_Modify);
        code_modify.block = xx walk(code_modify.block);
        __return(code_modify);
        
      case .DIRECTIVE_BAKE;
        _code := __get_as(code, Code_Directive_Bake);
        _code.procedure_call  = xx walk(_code.procedure_call);
        __return(_code);
        
      case .DIRECTIVE_INSERT;
        _code := __get_as(code, Code_Directive_Insert);
        _code.expression           = xx walk(_code.expression);
        _code.scope_redirection    = xx walk(_code.scope_redirection);
        _code.break_replacement    = xx walk(_code.break_replacement);
        _code.continue_replacement = xx walk(_code.continue_replacement);
        _code.remove_replacement   = xx walk(_code.remove_replacement);
        __return(_code);
        
      case .DIRECTIVE_MODULE_PARAMETERS;
        _code := __get_as(code, Code_Directive_Module_Parameters);
        _code.module_parameters  = xx walk(_code.module_parameters);
        _code.program_parameters = xx walk(_code.program_parameters);
        _code.common_code        = xx walk(_code.common_code);
        __return(_code);
        
      case .DIRECTIVE_LOCATION;
        _code := __get_as(code, Code_Directive_Location);
        _code.expression  = xx walk(_code.expression);
        __return(_code);
        
      case .DIRECTIVE_RUN;
        _code := __get_as(code, Code_Directive_Run);
        _code.procedure  = xx walk(_code.procedure);
        __return(_code);
        
      case .DIRECTIVE_EXISTS;
        _code := __get_as(code, Code_Directive_Exists);
        _code.query_expression = walk(_code.query_expression);
        _code.sync_expression  = walk(_code.sync_expression);
        __return(_code);
        
      // nodes I don't know how to handle at the moment:
        
      case .DIRECTIVE_PLACE; // Will be removed soon, probably.
      case .DIRECTIVE_WILDCARD;
      case .DIRECTIVE_LIBRARY;
      case .DIRECTIVE_SCOPE;
        
      case .PLACEHOLDER;    
        // We are not given the code ident for some reason?
        
      case .TYPE_DEFINITION;
        // only has *Type_Info, not sure what to do with that.
        
      case .DIRECTIVE_IMPORT;
      case .DIRECTIVE_LOAD;
        // Should we allow user to replace the file path?
        
      case .DIRECTIVE_POKE_NAME;
        _code := __get_as(code, Code_Directive_Poke_Name);
        _code.module_struct  = xx walk(_code.module_struct);
        // TODO: What to do about name here? We need special handling because it's not an ident!
        __return(_code);
        
      // nodes which always get returned as-is:
        
      case .IDENT;
      case .LITERAL;
      case .NOTE;
      case .ASM;
        
      // node kinds without corresponding Code_Node_X structs:
        
      case .CONTEXT;
      case .DIRECTIVE_CONTEXT_TYPE;
        
      case .DIRECTIVE_THROUGH;
      case .DIRECTIVE_COMPILE_TIME;
      case .DIRECTIVE_THIS;
        
      case .UNINITIALIZED; #through;
      case;
        assert(false, "Invalid code node kind in walk_nodes: %", code.kind);
    }
    
    __return(code);
}


ensure_correctness :: (node: *Code_Node) {
    if node.kind == {
      case .BINARY_OPERATOR;
        /*
            For now, we explicitly parenthesize all binary operators, 
            due to the fact that the insert expression could swap the right and left operands in some nested binary operation, 
            and we currently have no way of knowing whether the transformation can be done validly without parentheses.
            
            For example:
                format_code(#code log("%", 1 + 2 + 3),
                    { (#code `a + `b), (#code b - a) }, 
                );
            Substituting expression depth-first:
                Step 1:  1 + 2 + 3   -> (2 - 1) + 3
                Step 2:  (2 - 1) + 3 -> 3 - (2 - 1)
            If we did not insert the parentheses on the subexpression (2 - 1), then Program_Print would print `3 - 2 - 1`, which is incorrect!
            (The generated code is correct either way, since of course the parentheses are only relevant lexically.)
            
            If we could check the operator precedence here, then we could do the parenthesis insertion conditionally, 
            but that would involve maintaining a table of all operators and that's just not worth the trouble at the moment.
            I think that for the sake of correctness and robustness, Program_Print should check the precedence order of the LHS and RHS of binary operators.
            But until that day, we need to set this flag.
            This may make some long sequences of binary operators more difficult to read in the debug output, but at least it will be correct!
        */
        node.node_flags |= .IS_PARENTHESIZED;
        
      case .PROCEDURE_HEADER;
        _node := node.(*Code_Procedure_Header);
        _node.body_or_null.header = _node;
    }
}

This_Module :: #this;

/*
    NOTES:
    - Using insert,scope() inside a struct appears to cause a circular dependency error of some sort. Need to test this with a more simple case to see what other factors are involved.
    - Using #import,string inside a procedure appears to result in the compiler trying to just do a regular import rather than importing the string as a module.
    
    
    TODO:
    after testing a simpler example, it seems that the dummy struct idea should actually work out, I will just have to pass only the types/procedures I absolutely need.
    The circular dependency thing was probably due to trying to pass the whole module type in to the bake_arguments or something...
    
    If that doesnt' work another possible trick we could try is using dynamic specialize.
    
    I think there may also be something to the idea of using Types to return polymorphic data back from a procedure like generate_filter_proc.
    And then we could do some kind of bake on the polymorphic procedure inside the type once we are back in the calling scope...
    
    Option number 3 is that we add a helper proc inside generate_filter_proc that performs the first step of doing the Code insert,
    then by returning from that helper procedure, we are able to step back into another scope that has access to the local constants and make use of those again.
    
    Eureka, we did it! And now I am exhausted.
    But I still wanna know why we got that compiler circular dependency error thing when trying to just insert,scope into the dummy struct directly, without using the helper proc.
    It's strange that the minimal example works, but something about this more complex scenario was causing the compiler to give up.
*/

generate_filter_proc :: ($filter_code: Code, scope := #caller_code) -> ((*Match_Expression, *Code_Node) -> bool) {
    #assert filter_code != #code,null;
    
    // By returning a Type here, we can effectively return a polymorphic procedure from a #run. (And that procedure has the scope from filter_code.)
    // This allows us to then specialize the procedure below and insert things that we need from this module's scope.
    // This would not be necessary if not for the scoping issues currently presetn when inserting procedures and structs.
    using #run -> Type {
        #insert,scope(filter_code) #run format_fitler_proc_code(filter_code);
    };
    
    #if #exists(filter_proc) {
        return #bake_arguments filter_proc(__get_parameter__ = get_extracted_expression_by_name, Match_Expression = Match_Expression);
    } else {
        return null;
    }
}

format_fitler_proc_code :: (code: Code) -> Code #compile_time {
    root := compiler_get_nodes(code);
    if root == null || root.kind != .PROCEDURE_HEADER {
        return #code return struct {};
    }
    
    filter_proc_decl := compiler_get_nodes(#code
        filter_proc :: ($__get_parameter__: $T, $Match_Expression: Type, match_expression: *Match_Expression, code: *Code_Node) -> bool { }
    ).(*Code_Declaration);
    
    filter_proc_header := filter_proc_decl.expression.(*Code_Procedure_Header);
    filter_proc_header.body_or_null = New(Code_Procedure_Body);
    filter_proc_header.body_or_null.header = filter_proc_header;
    
    lambda_header := root.(*Code_Procedure_Header);
    assert(lambda_header.procedure_flags & .QUICK == .QUICK);
    
    statements: [..] *Code_Node;
    statements.allocator = temp;
    array_reserve(*statements, lambda_header.arguments.count + 3);
    
    match_expression_ident := make_ident("match_expression");
    
    for lambda_header.arguments {
        expression := make_procedure_call("__get_parameter__", 
            .{ match_expression_ident, null },
            .{ make_string_literal(it.name),   null },
        );
        declaration := make_declaration(it.name, null, expression);
        array_add(*statements, declaration);
    }
    
    lambda_block := lambda_header.body_or_null.block;
    {
        return_statement := lambda_block.statements[lambda_block.statements.count-1].(*Code_Return);
        if return_statement.kind == .RETURN {
            return_statement.return_flags &= ~.AUTO_INSERTED_FOR_QUICK_LAMBDA;
        }
    }
    
    array_add(*statements, lambda_block);
    
    filter_block := New(Code_Block);
    filter_block.block_type = .IMPERATIVE;
    filter_block.statements = statements;
    
    filter_proc_header.body_or_null.block = filter_block;
    
    return_statement := compiler_get_nodes(#code return struct #type_info_none { placeholder: void; }).(*Code_Return);
    struct_def := return_statement.arguments_unsorted[0].expression.(*Code_Struct);
    assert(struct_def.kind == .STRUCT);
    assert(struct_def.block.statements.count == 1);
    struct_def.block.statements[0] = filter_proc_decl;
    
    print_code(return_statement);
    return compiler_get_code(return_statement);
}

get_extracted_expression_by_name :: (match_expression: *Match_Expression, name: string) -> *Code_Node {
    for match_expression.extracted_expressions {
        if it.name == name  return it.expression;
    }
    assert(false, "Invalid identifier used in filter code: %", name);
    return null;
}

/*
    TODO: make the following usage example work
          will require lambda nonsense, and the abilty to attach an Any as the insert expression rather than code nodes
          if an Any is passed, we should just insert the value as a literal
    
    total_offset_in_bytes := parent_offset_in_bytes + member.offset_in_bytes;
    nodes := Code_Format.simple_substitution( 
        (#code render_page(site, (site.(*void) + offset).(*Template) )),
        offset => total_offset_in_bytes,
        allocator = builder.allocator
    );
    
    simple_substitution_to_nodes :: (code: Code) -> *Code_Node {
        
    }
*/



#import "Print_Vars";