open OUnit
open Qbf
open Fol_qbf

let test_fixture = "Removing functions tests" >:::
  [
    "Converting qbf prefix to fol qbf prefix" >:: (fun () ->
      assert_equal
        [{quant_level = 1; quantifier = E; atom = 1};
         {quant_level = 2; quantifier = A; atom = 2};
         {quant_level = 3; quantifier = E; atom = 3}]
        (convert_qbf_prefix_to_fol 
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = A; variable = 2};
          {quant_level = 3; quantifier = E; variable = 3}])
    );

    "Converting pos literal to fol predicate" >:: (fun () ->
      assert_equal
        {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}}
        (convert_literal_to_fol {sign = Pos; atom = 1})
    );

    "Converting neg literal to fol predicate" >:: (fun () ->
      assert_equal
        {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}}
        (convert_literal_to_fol {sign = Neg; atom = 1})
    );

    "Converting matrix to fol" >:: (fun () ->
      assert_equal
        [[{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
          {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(2)]}}]; 
         [{sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
          {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(2)]}}]]
        (convert_matrix_to_fol
         [[{sign = Pos; atom = 1}; {sign = Neg; atom = 2}];
          [{sign = Neg; atom = 1}; {sign = Pos; atom = 2}]])
    );

    "Converting qbf to fol qbf" >:: (fun () ->
      assert_equal
        {prefix = 
          [{quant_level = 1; quantifier = A; atom = 1};
           {quant_level = 2; quantifier = E; atom = 2}];
         matrix =
           [[{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
             {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(2)]}}]; 
            [{sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
             {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(2)]}}];
            [{sign = Pos; pred = {pred_name = "p"; pred_arguments = [True]}}];
            [{sign = Neg; pred = {pred_name = "p"; pred_arguments = [False]}}]]}
        (convert_qbf_to_fol
         {qbf_prefix =
           [{quant_level = 1; quantifier = A; variable = 1};
            {quant_level = 2; quantifier = E; variable = 2}];
          qbf_matrix =
           [[{sign = Pos; atom = 1}; {sign = Neg; atom = 2}];
            [{sign = Neg; atom = 1}; {sign = Pos; atom = 2}]]})
    );
  ]

let _ = run_test_tt ~verbose:true test_fixture
