open OUnit
open Fol_qbf

let test_fixture = "Removing functions tests" >:::
  [
    "Raising a function to a predicate" >:: (fun () ->
      assert_equal
        {pred_name = "p_f_2"; pred_arguments = [Atom(1)]}
        (raise_function_to_predicate
         {func_name = "f_2"; func_arguments = [1]})
    );

    "Raising a function with two arguments to a predicate" >:: (fun () ->
      assert_equal
        {pred_name = "p_f_3"; pred_arguments = [Atom(1); Atom(2)]}
        (raise_function_to_predicate
         {func_name = "f_3"; func_arguments = [1; 2]})
    );

    "Raising a function with no arguments to a predicate" >:: (fun () ->
      assert_equal
        {pred_name = "p_c_1"; pred_arguments = []}
        (raise_function_to_predicate
         {func_name = "c_1"; func_arguments = []})
    );

    "Removing a function from a predicate" >:: (fun () ->
      assert_equal
        {pred_name = "p_f_2"; pred_arguments = [Atom(1)]}
        (remove_function_from_predicate
         {pred_name = "p"; pred_arguments =
           [Func({func_name = "f_2"; func_arguments = [1]})]})
    );
 
    "Removing a function from a predicate without a function" >:: (fun () ->
      assert_equal
        {pred_name = "p"; pred_arguments = [Atom(2)]}
        (remove_function_from_predicate
         {pred_name = "p"; pred_arguments = [Atom(2)]})
    ); 

    "Removing functions from a fol_qbf" >:: (fun () ->
      assert_equal
        {prefix = [{quant_level = 1; quantifier = A; atom = 1}];
         matrix = [
           [{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
            {sign = Pos; pred = {pred_name = "p_f_2"; pred_arguments = [Atom(1)]}}];
           [{sign = Neg; pred ={pred_name = "p_f_2"; pred_arguments = [Atom(1)]}};
            {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}}]]}
        (remove_functions_from_fol_qbf
         {prefix = [{quant_level = 1; quantifier = A; atom = 1}];
          matrix = [
           [{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
            {sign = Pos; pred = {pred_name = "p"; pred_arguments =
              [Func({func_name = "f_2"; func_arguments = [1]})]}}];
           [{sign = Neg; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_2"; func_arguments = [1]})]}};
            {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}}]]})
    );
  ]

let _ = run_test_tt ~verbose:true test_fixture
