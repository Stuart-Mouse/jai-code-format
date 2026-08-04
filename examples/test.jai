
#import "Basic";
#import "Math";
#import "String";
#import "Compiler";

#import "Utils";
#import "Code_Format";
Program_Print :: #import "Program_Print";

// NOTE: The use of #run,stallable everywhere is simply for debugging purposes.
//       In principle, there's no reason why most calls to format_code should need to be stallable, but the compiler does not give very good errors when a #run fails which is not stallable.

main :: () {
    // In this most simple of examples, we just replace the identifiers `a` and `b` with integer literals `3` and `5` respectively.
    // NOTE: Please ignore the the `true` member in each of the format rules for now, I will explain the reason for this later on.
    //       I will hopefully be improving the API sooner rather than later and this should become optional.
    print("\nSimple Substitution:\n");
    #insert #run,stallable 
        format_code(#code print_eval(a + b),
            { (#code a), 3, #code,null },
            { (#code b), 5, #code,null },
        );
    
    {
        // Here's a simple sum function that we will modify and reinsert using format_code.
        sum :: (numbers: [] int) -> int {
            s: int;
            for numbers s += it;
            return s;
        }
        
        // In this case, we modify the sum function to replace all instances of the identifier `float` with `int`.
        // This essentially generates a whole new polymorph of the sum function, though sum is not itself polymorphic.
        // One should note that this would not likely be feasible with more complex type expressions, so this is a bit of a contrived case.
        print("\nSum Floats:\n");
        {
            sum_floats :: #insert #run,stallable 
                format_code(code_of(sum), 
                    { int, float, #code,null }, 
                );
            
            numbers := float.[ 1, 2.5, 3.25 ];
            print_eval(sum_floats(numbers));
        }
        
        
        // Up until now we have only been doing simple substitution with identifiers, but this alone is not very powerful.
        // TODO: explain how we really want to be able to match arbitrarily complex subexpressions, and to manipulate those subexpressions.
        
        // Before we return to modifying our sum function, we need to demonstrate a new feature: wildcards.
        // When an identifier in the match expression is backticked, it becomes a wildcard that can match to (almost) any subexpression.
        // Those wildcard identifiers can then be used in the replacement expression to reinsert the matched subexpressions.
        
        // In this case, we use a match expression with wildcards `a` and `b` for the left- and right-hand sides of a binary addition operation respectively.
        // Then in the replacement expression, we swap the operands and change the operation to subtraction.
        
        print("\nWildcards Example 1:\n");
        #insert #run,stallable 
            format_code(#code print_eval(3 + 7),
                { (#code `a + `b), (#code b - a), #code,null }
            );
        
        print("\nNegative Sum:\n");
        {
            negative_sum :: #insert #run,stallable 
                format_code(code_of(sum), 
                    { (#code `a += `b), (#code a -= b), #code,null }
                );
            
            numbers := int.[ 1, 2, 3, 4, 5 ];
            print_eval(negative_sum(numbers));
        }
    }
    
    // Below are a couple of random test cases involving literals and operators. 
    // They're not particularly interesting for the most part...
    
    // NOTE: We don't currently match both (`a + 2) and (2 + `a) when the operator is commutative.
    //       This seems like something people may want to do, so perhaps in the future I will add some logic to make that possible.
    //       If you have any ideas or suggestions about this or anything else in this module, please let me know!
    //       There are a lot of weird little edge cases and API design things that will need to get ironed out over time...
    
    print("\nMatching Against Literals:\n");
    #insert #run,stallable 
        format_code(#code {
                print_eval(1 + 1);
                print_eval(1 + 2);
                print_eval(2 + 2);
            },
            { (#code `a + 2), (#code a + 3), #code,null }, 
        );
    
    print("\nMatching Binary Minus:\n");
    #insert #run,stallable 
        format_code(#code {
                print_eval(6 + 1);
                print_eval(6 - 1);
            },
            { (#code `a - `b), (#code `a - (`b * 2)), #code,null }, 
        );
    
    print("\nMatching Unary Minus:\n");
    #insert #run,stallable 
        format_code(#code {
                value := 5;
                print_eval(+value);
                print_eval(-value);
            },
            { (#code -`a), (#code ~`a), #code,null }, 
        );
    
    
    // TODO: Match a declaration
    // TODO: Match a compound declaration
    
    print("\nMatching a Simple Cast:\n");
    #insert #run,stallable 
        format_code(#code {
                value    := 5;
                as_float := cast(float) value;
                as_u16   := cast(u16) value;
                print_eval(type_of(as_float));
                print_eval(type_of(as_u16));
            },
            { (#code cast(float) `a), (#code cast(float64) `a), #code,null }, 
        );
    
    
    /*
        Now here's an exciting feature: filter expressions!
        
        That `true` member in the format rule struct that I previously asked you to ignore is for a filter expression that will be checked each time a potential match is found.
        This match expression must be passed in the form of a quick lambda, in which the arguments are the wildcard identifiers used in the match expression.
        You don't have to use all of the wildcards in the filter expression's signature, only the ones you want to inspect, so think of it as more of a capture list.
        Inside the filter expression the wildcard identifiers are of type `*Code_Node` so you can recursively inspect them and use whatever criteria you wish to determine if the expression is a match.
        
        Behind the scenes, we do some sort of kooky stuff to make this nice syntax work out, but the end result is quite nice.
        The filter proc has access to local scope, as well as a set of predefined helper functions.
    */
    
    // Test filter expressions
    {
        foo :: () {
            apple:      int   = 3;
            banana:     int   = 5;
            artichoke:  float = 7;
            
            print_eval(apple);
            print_eval(banana);
            print_eval(artichoke);
        }
        
        bar :: #insert #run,stallable
            format_code(code_of(foo),
                { 
                    (#code `a), 
                    (#code `a * 2), 
                    
                    // This will ensure that `apple` is our only match, 
                    // since it is the only identifier starting with `a` that corresponds to an integer.
                    a => is_integer(a) && ident_begins_with(a, "a")
                }, 
            );
        
        bar();
    }
    
    // Replace float-to-int casts with a procedure call to floor_float_to_int.
    {
        floor_float_to_int :: (x: float, $T: Type) -> T #modify {
            return T.(*Type_Info).type == .INTEGER;
        } {
            return cast(T) floor(x); 
        }
        
        foo :: () {
            {
                _int: int;
                _float32 := cast(float32) _int;
            }
            {
                _float32: float32;
                _int := cast(int) _float32;
            }
            {
                _float32: float32;
                _float64 := cast(float64) _float32;
            }
        }
        
        bar :: #insert #run,stallable
            format_code(code_of(foo),
                { 
                    (#code cast(`T) `a), 
                    (#code floor_float_to_int(a, T)), 
                    
                    // In many cases, one may want to root of the extracted expression rather than only the wildcard subexpressions.
                    // For this, you can just use use the reserved identifier 'expr'.
                    // In this case we use expr to check the cast node itself, rather than `T or `a.
                    _ => is_cast(expr, to = .INTEGER, from = .FLOAT)
                }, 
            );
        
        bar();
    }
    #if false {
        // An example of where this could be useful is if you want to do something like the following, to dynamically determine the float rounding mode based on some context member.
        // Using a simple formatting rule we can replace every float-to-int cast with this procedure.
        float_to_int :: (float_value: $Float_Type, $Int_Type: Type, rounding_mode := context.float_rounding_mode) -> Int_Type #modify {
            return Float_Type.(*Type_Info).type == .FLOAT, "Float_Type is not a float type.";
        } {
            int_value: Int_Type;
            if #complete float_rounding_mode == {
              case .DEFAULT;    int_value = cast(Int_Type) float_value;
              case .TRUNCATE;   int_value = cast(Int_Type) float_value;
              case .FLOOR;      int_value = cast(Int_Type) floor(float_value);
              case .CEILING;    int_value = cast(Int_Type) ciel (float_value);
              case;
                assert(false, "Invalid float_rounding_mode in float_to_int: %", float_rounding_mode);
            }
            return int_value;
        }
        // In a more realistic scenario, we would probably want to run this and other code formatter rules over all our code when we get a Message_Typechecked.
        // To that end, what we really want is to set up a code formatter once and then execute it with various inputs.
        // I intend to make this use case more practical and set up an example metaprogam that is doing something like this as an example.
    }
    
    
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
        format_code(#code print_eval(1 + 2 + 3),
            { (#code `a + `b), (#code b - a), #code,null }, 
        );
    
    
}

#run {
    root := compiler_get_nodes(#code 5);
    log("%", is_integer(root));
}

echo :: (value: $T) -> T {
    print("%\n", value);
    return value;
}

print_eval :: ($code: Code) #expand {
    expression := #run,stallable sprintx(compiler_get_nodes(code));
    value := #insert code;
    print("% -> %\n", expression, value);
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
