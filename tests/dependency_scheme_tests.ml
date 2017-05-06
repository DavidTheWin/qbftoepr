open OUnit
open Fol_qbf
open Qbf

let test_fixture = "Depedency scheme tests" >:::
  [
    "No universal atoms before existential" >:: (fun () ->
      assert_equal
        []
        (universally_quantified_below_quant_level
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = E; variable = 2};
          {quant_level = 3; quantifier = E; variable = 3};]
         3)
    );

    "No atoms before existential" >:: (fun () ->
      assert_equal
        []
        (universally_quantified_below_quant_level
         [{quant_level = 1; quantifier = E; variable = 1}]
         1)
    );

    "All universal atoms before given existential atom" >:: (fun () ->
      assert_equal 
        [{quant_level = 2; quantifier = A; variable = 2}]
        (universally_quantified_below_quant_level
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = A; variable = 2};
          {quant_level = 3; quantifier = E; variable = 3};
          {quant_level = 4; quantifier = A; variable = 4}]
         3)
    );

    "All universal atoms before given existential atom" >:: (fun () ->
      assert_equal 
        []
        (universally_quantified_below_quant_level
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = A; variable = 2};
          {quant_level = 3; quantifier = E; variable = 3};
          {quant_level = 4; quantifier = A; variable = 4}]
         1)
    );

    "Trivial dependency scheme for variable" >:: (fun () ->
      assert_equal
      [3, [2]]
      (trivial_dep_scheme_for_variable
       [{quant_level = 1; quantifier = E; variable = 1};
        {quant_level = 2; quantifier = A; variable = 2};
        {quant_level = 3; quantifier = E; variable = 3};
        {quant_level = 4; quantifier = A; variable = 4}]
       {quant_level = 3; quantifier = E; variable = 3})
    );

    "Standard dependency scheme for variable" >:: (fun () ->
      assert_equal
      [5, [3; 4]]
      (std_dep_scheme_for_variable
       {qbf_prefix =
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = A; variable = 2};
          {quant_level = 3; quantifier = A; variable = 3};
          {quant_level = 4; quantifier = A; variable = 4};
          {quant_level = 5; quantifier = E; variable = 5}];
        qbf_matrix =
          [[{sign = Pos; atom = 1}; {sign = Pos; atom = 2}];
           [{sign = Neg; atom = 3}; {sign = Pos; atom = 4}; {sign = Neg; atom = 5}]]}
       {quant_level = 5; quantifier = E; variable = 5})
    );

    "Trivial dependency scheme for qbf" >:: (fun () ->
      assert_equal
      [1, []; 5, [2; 3; 4]]
      (build_dep_scheme
       {qbf_prefix =
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = A; variable = 2};
          {quant_level = 3; quantifier = A; variable = 3};
          {quant_level = 4; quantifier = A; variable = 4};
          {quant_level = 5; quantifier = E; variable = 5}];
        qbf_matrix =
          [[{sign = Pos; atom = 1}; {sign = Pos; atom = 2}];
           [{sign = Neg; atom = 3}; {sign = Pos; atom = 4}; {sign = Neg; atom = 5}]]}
       Trivial)
    );

    "Standard dependency scheme for qbf" >:: (fun () ->
      assert_equal
      [1, []; 5, [3; 4]]
      (build_dep_scheme
       {qbf_prefix =
         [{quant_level = 1; quantifier = E; variable = 1};
          {quant_level = 2; quantifier = A; variable = 2};
          {quant_level = 3; quantifier = A; variable = 3};
          {quant_level = 4; quantifier = A; variable = 4};
          {quant_level = 5; quantifier = E; variable = 5}];
        qbf_matrix =
          [[{sign = Pos; atom = 1}; {sign = Pos; atom = 2}];
           [{sign = Neg; atom = 3}; {sign = Pos; atom = 4}; {sign = Neg; atom = 5}]]}
       Standard)
    );
  ]

let _ = run_test_tt ~verbose:true test_fixture
