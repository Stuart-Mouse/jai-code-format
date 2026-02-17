
#import "Basic";
#import "Math";
#import "Compiler";

#import "Utils";
#import "Code_Format";
Program_Print :: #import "Program_Print";

main :: () {
    // Test simple find and replace
    #insert #run,stallable 
        format_code(#code log_eval(a + b),
            { (#code a), 3, true }, 
            { (#code b), 5, true },
        );
    
    {
        sum_floats :: #insert #run,stallable format_code( code_of(sum), { int, float, true }, );
        
        numbers := float.[ 1, 2.5, 3.25 ];
        log("sum_floats(%) -> %", numbers, sum_floats(numbers));
    }
    
    // Test find and replace with extracted/reinserted subexpressions
    #insert #run,stallable 
        format_code(#code log_eval(3 + 7),
            { (#code `a + `b), (#code b - a), true }, 
        );
    
    {
        negative_sum :: #insert #run,stallable format_code( code_of(sum), { (#code `a += `b), (#code a -= b), true } );
        
        numbers := int.[ 1, 2, 3, 4, 5 ];
        log("negative_sum(%) -> %", numbers, negative_sum(numbers));
    }
    
    
    // Test more complex match expressions...
    // Matching against literals
    #insert #run,stallable 
        format_code(#code {
                log_eval(1 + 1);
                log_eval(1 + 2);
                log_eval(2 + 2);
            },
            { (#code `a + 2), (#code a + 3), true }, 
        );
    
    // TODO: should we automatically match both `a + 2 and 2 + `a when the operator is commutative? maybe pass some flag for this?
    
    
    // Matching against particular binary operators
    #insert #run,stallable 
        format_code(#code {
                log_eval(6 + 1);
                log_eval(6 - 1);
            },
            { (#code `a - `b), (#code `a - (`b * 2)), true }, 
        );
    
    // Matching against particular unary operators
    #insert #run,stallable 
        format_code(#code {
                value := 5;
                log_eval(+value);
                log_eval(-value);
            },
            { (#code -`a), (#code ~`a), true }, 
        );
    
    
    // Match a declaration
    // Match a compound declaration
    
    // Match a cast to a particular type (check actual type info if match expression and input code are typed)
    #insert #run,stallable 
        format_code(#code {
                value    := 5;
                as_float := cast(float) value;
                as_u16   := cast(u16) value;
                log_eval(type_of(as_float));
                log_eval(type_of(as_u16));
            },
            { (#code cast(float) `a), (#code cast(float64) `a), true }, 
        );
    
    
    // Test filter expressions
    // {
    //     foo :: () {
    //         apple:      int   = 3;
    //         banana:     int   = 5;
    //         artichoke:  float = 7;
            
    //         log_eval(apple);
    //         log_eval(banana);
    //         log_eval(artichoke);
    //     }
        
    //     bar :: #insert #run,stallable
    //         format_code(code_of(foo),
    //             { (#code `a), (#code `a * 2), a => a.kind == .IDENT && a.(*Code_Ident).name[0] == "a" && a.type.type == .INTEGER }, 
    //         );
        
    //     bar();
    // }
    
    // {
    //     floor_float_to_int :: (x: float, $T: Type) -> T #modify {
    //         return T.(*Type_Info).type == .INTEGER;
    //     } {
    //         return cast(T) floor(x); 
    //     }
        
    //     foo :: () {
    //         {
    //             _int: int;
    //             _float := cast(float) _int;
    //         }
            
    //         {
    //             _float: float;
    //             _int := cast(int) _float;
    //         }
    //     }
        
    //     bar :: #insert #run,stallable
    //         format_code(code_of(foo),
    //             { 
    //                 (#code cast(`T) `a), 
    //                 (#code floor_float_to_int(a, T)), 
    //                 (a, T) => a.type.type == .FLOAT && echo(T.kind)
    //             }, 
    //             code_to_copy_scope_from = echo
    //         );
        
    //     bar();
    // }
    
    // Test recursive replacements...
    // replace a + (b + c) with a - (b - c)
    
    /*
        NOTE: Program_Print improperly formats the resulting expression here, for some reason.
        
        step 1:  1 + 2 + 3   -> (2 - 1) + 3
        step 2:  (2 - 1) + 3 -> 3 - (2 - 1)
        
        But program_Print just prints `3 - 2 - 1` for this expression.
        This is most likely to do with the second swap that occurs, since we are flipping the tree to an orientation that it would not usually be built in.
        So in theory, we should just explicitly parenthesize the `2 - 1` subepxression and be fine.
        But that is also not working!
        So program print must be ignoring the parenthesis incorrectly, for some reason...
        
        In any case, we still get the proper functionality from the code.
        
        TODO: build an operator precedence lookup table and add logic to Program_Print to respect operator precedence by automatically inserting parentheses where required.
    */ 
    #insert #run,stallable 
        format_code(#code log_eval(1 + 2 + 3),
            { (#code `a + `b), (#code b - a), true }, 
        );
    
    
}

echo :: (value: $T) -> T {
    log("%", value);
    return value;
}

log_eval :: ($code: Code) #expand {
    expression := #run,stallable sprintx(compiler_get_nodes(code));
    value := #insert code;
    log("% -> %", expression, value);
}


/*
    There is also a bit of a secondary issue with the way program print inserts parentheses when it comes to printing manually-generated code nodes.
    If a user manually builds some AST like so:
    
    result := make_binary_operator(#char "-",
        make_integer_literal(3),
        make_binary_operator(#char "-",
            make_integer_literal(2),
            make_integer_literal(1),
        )
    );
    
    Then printing result will yeild: `3 - 2 - 1` because Program_Print does not actually consider operator precedence in its output.
    I think that, for the sake of correctness, Program_Print should check if the RHS of a binary operator is also a binary operator, compare precedence, and inject additional parentheses if they are needed.
    Otherwise any user who wants to debug manually constructed code nodes using program_print will have to manually track operator precedence and set the .IS_PARENTHESIZED flag himself.
    
    
*/

// #run,stallable {
//     expression := compiler_get_nodes( 3 - (2 - 1) );
    
//     builder: String_Builder;
//     Program_Print.print_expression(*builder, expression);
//     append(*builder, "\n");
    
//     log(builder_to_string(*builder,, temp));
// }

sum :: (numbers: [] int) -> int {
    s: int;
    for numbers s += it;
    return s;
}
