
// // Code_Search_Results :: struct {
// //     queries: [..] Code_Query;
// // }

// // Code_Query :: struct {
    
// // }

// /*
//     We would like to not have to pass the code formatter in here manually...
//     It would be nice to have a more high-level version that just returns the results you want directly, sort of like our high-level format_code.
//     But the problem is, we need to know in more detail what's going on with allocations when we call compiler_get_nodes.
//     Is it on us to free the nodes we get somehow? or does the compiler just assume that all these nodes should just live forever (untilt he compiler exits?)
    
    
// */
// search_code :: (formatter: *Code_Formatter, $code: Code, $match_expression: Code) #compile_time {
//     formatter: Code_Formatter;
//     formatter.allocator = allocator;
//     formatter.mode = .SEARCH;
    
//     // formatter.format_rules = NewArray(format_rules.count, Node_Format_Rule,, get_allocator(*formatter));
//     // // TODO: Try and make this into a code insert rather than a string insert.
//     // #insert -> string {
//     //     builder: String_Builder;
//     //     for 0..format_rules.count-1 {
//     //         print(*builder, #string JAI
//     //             formatter.format_rules[%1] = create_node_format_rule(*formatter, format_rules[%1]);
//     //         JAI, it);
//     //     }
//     //     return builder_to_string(*builder);
//     // }
    
//     formatter.match_expression.root = compiler_get_nodes(match_expression);
//     init_extracted_expressions(formatter, *formatter.match_expression);
    
//     nodes := compiler_get_nodes(code);
//     search_code(*formatter, nodes);
// }

// search_code :: (formatter: *Code_Formatter, code: *Code_Node) -> *Code_Node {
//     return walk_nodes(
//         formatter       = formatter, 
//         code            = code,
//         user_data       = null,
//         on_return       = xx find_all_matching_expressions, 
//         modify_in_place = false,
//     );
// }

// find_all_matching_expressions :: (formatter: *Code_Formatter, code: *Code_Node, _: *void) -> Code_Walk_Return_Code, *Code_Node {
//     assert(formatter.mode == .SEARCH);
    
//     if match_code(formatter, *formatter.match_expression, code, formatter.match_expression.root) {
//         // set the root extracted expression before running the filter proc
//         if formatter.match_expression.extracted {
//             formatter.match_expression.extracted[0].expression = code;
//         }
        
//         filter := formatter.match_expression.filter;
//         if filter && !filter(*formatter.match_expression, code) {
//             return .CONTINUE, code;
//         }
        
//         match: Match_Expression.Matched;
//         match.root = code;
//         match.extracted_expressions = NewArray(formatter.match_expression.extracted.count, *Code_Node);
//         for formatter.match_expression.extracted {
//             match.extracted_expressions[it_index] = it.expression;
//         }
//         append(*formatter.match_expression.matched, match);
        
//         return .RETURN, code;
//     }
//     return .CONTINUE, code;
// }
