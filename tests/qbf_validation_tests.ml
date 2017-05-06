open OUnit
open Qbf

let test_fixture = "QBF validation tests" >:::
  [
    "Negative atom fails" >:: (fun () ->
      assert_raises
        (InvalidQDIMACS "Atom list in quantifier set cannot contain negative integers")
        (fun () -> check_if_atoms [-3; 1])
    );

    "Innermost quantifier is existential pass" >:: (fun () ->
      assert_equal true
        (innermost_quantifier_is_existential
         [{quant_level = 0; quantifier = E; variable = 0};
          {quant_level = 1; quantifier = A; variable = 0};
          {quant_level = 2; quantifier = E; variable = 0}]
        )
    );

    "Innermost quantifier is universal fail" >:: (fun () ->
      assert_equal false
        (innermost_quantifier_is_existential
         [{quant_level = 0; quantifier = A; variable = 0};
          {quant_level = 1; quantifier = E; variable = 0};
          {quant_level = 2; quantifier = A; variable = 0}]
        )
    );

    "No innermost quantifier fails" >:: (fun () ->
      assert_equal false (innermost_quantifier_is_existential [])
    );

    "All atoms in prefix are in matrix pass" >:: (fun () ->
      assert_equal true
        (all_atoms_in_prefix_are_in_matrix
         [{quant_level = 0; quantifier = E; variable = 1};
          {quant_level = 1; quantifier = A; variable = 2};
          {quant_level = 2; quantifier = E; variable = 3}]
         [[{sign = Pos; atom = 1}; {sign = Neg; atom = 2}]; 
          [{sign = Neg; atom = 1}; {sign = Pos; atom = 3}];
          [{sign = Pos; atom = 2}; {sign = Neg; atom = 3}]]
        )
    );

    "Atom in prefix not in matrix fail" >:: (fun () ->
      assert_equal false
        (all_atoms_in_prefix_are_in_matrix  
         [{quant_level = 0; quantifier = E; variable = 1};
          {quant_level = 1; quantifier = A; variable = 2};
          {quant_level = 1; quantifier = E; variable = 3};
          {quant_level = 2; quantifier = E; variable = 4}]
         [[{sign = Pos; atom = 1}; {sign = Neg; atom = 2}]; 
          [{sign = Neg; atom = 1}; {sign = Pos; atom = 3}];
          [{sign = Pos; atom = 2}; {sign = Neg; atom = 3}]]
        )
    );

    "No duplicate atoms in prefix passes" >:: (fun () ->
      assert_equal true
        (no_duplicate_atoms_in_prefix
         [{quant_level = 0; quantifier = E; variable = 1};
          {quant_level = 1; quantifier = A; variable = 2};
          {quant_level = 2; quantifier = E; variable = 3}]
        )
    );

    "Duplicate atoms in prefix fails" >:: (fun () ->
      assert_equal false
        (no_duplicate_atoms_in_prefix
         [{quant_level = 0; quantifier = E; variable = 1};
          {quant_level = 1; quantifier = A; variable = 1};
          {quant_level = 2; quantifier = E; variable = 2}]
        )
    );
  ]

let _ = run_test_tt ~verbose:true test_fixture
