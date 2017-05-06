open OUnit
open Fol_qbf
open Qbf

let test_fixture = "Skolemization tests" >:::
  [
    "Prefix without given existential variable" >:: (fun () ->
      assert_equal
        [{quant_level = 1; quantifier = E; atom = 1};
         {quant_level = 2; quantifier = A; atom = 2};
         {quant_level = 4; quantifier = A; atom = 4}]
        (prefix_without_quantifier
         [{quant_level = 1; quantifier = E; atom = 1};
          {quant_level = 2; quantifier = A; atom = 2};
          {quant_level = 3; quantifier = E; atom = 3};
          {quant_level = 4; quantifier = A; atom = 4}]
         {quant_level = 3; quantifier = E; atom = 3}
        )
    );

    "Prefix without given existential variable" >:: (fun () ->
      assert_equal
        []
        (prefix_without_quantifier
         [{quant_level = 1; quantifier = E; atom = 1}]
         {quant_level = 1; quantifier = E; atom = 1}
        )
    );

    "Prefix without any existential_variables" >:: (fun () ->
      assert_equal
        [{quant_level = 2; quantifier = A; atom = 2};
         {quant_level = 4; quantifier = A; atom = 4}]
        (prefix_without_existential_variables
         [{quant_level = 1; quantifier = E; atom = 1};
          {quant_level = 2; quantifier = A; atom = 2};
          {quant_level = 3; quantifier = E; atom = 3};
          {quant_level = 4; quantifier = A; atom = 4}]
        )
    );

    "Only existential prefix without existential variables" >:: (fun () ->
      assert_equal
        []
        (prefix_without_existential_variables
         [{quant_level = 1; quantifier = E; atom = 1};
          {quant_level = 2; quantifier = E; atom = 2}]
        )
    );

    "Building a skolem function list" >:: (fun () ->
      assert_equal
        [1, {func_name = "c_1"; func_arguments = []};
         3, {func_name = "f_3"; func_arguments = [2]}]
        (build_skolem_func_list
         [{quant_level = 1; quantifier = E; atom = 1};
          {quant_level = 2; quantifier = A; atom = 2};
          {quant_level = 3; quantifier = E; atom = 3};
          {quant_level = 4; quantifier = A; atom = 4}]
         [3, [2]; 1, []]
        )
    );

    "Building a skolem function list with no existential variables" >:: (fun () ->
      assert_equal
        []
        (build_skolem_func_list
         [{quant_level = 1; quantifier = A; atom = 1};
          {quant_level = 2; quantifier = A; atom = 2}]
         []
        )
    );

    "Getting a skolemized predicate" >:: (fun () ->
      assert_equal
        {pred_name = "p";
         pred_arguments = [Func({func_name = "c_1"; func_arguments = []})]}
        (get_skolemized_predicate
         [1, {func_name = "c_1"; func_arguments = []};
          3, {func_name = "f_3"; func_arguments = [2]}]
         {pred_name = "p"; pred_arguments = [Atom(1)]}
        )
    );

    "Getting a skolemized predicate without replacement" >:: (fun () ->
      assert_equal
        {pred_name = "p"; pred_arguments = [Atom(2)]}
        (get_skolemized_predicate
         [1, {func_name = "c_1"; func_arguments = []};
          3, {func_name = "f_3"; func_arguments = [2]}]
         {pred_name = "p"; pred_arguments = [Atom(2)]}
        )
    );

    "Skolemization" >:: (fun () ->
      assert_equal
       {prefix =
         [{quant_level = 1; quantifier = A; atom = 1};
          {quant_level = 3; quantifier = A; atom = 3}];
        matrix =
         [[{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_2"; func_arguments = [1]})]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}}];
          [{sign = Neg; pred = {pred_name = "p"; pred_arguments =
            [Func({func_name = "f_2"; func_arguments = [1]})]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_4"; func_arguments = [1; 3]})]}}]]}
      (skolemization
       {prefix =
         [{quant_level = 1; quantifier = A; atom = 1};
          {quant_level = 2; quantifier = E; atom = 2};
          {quant_level = 3; quantifier = A; atom = 3};
          {quant_level = 4; quantifier = E; atom = 4}];
        matrix =
         [[{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(2)]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}}];
          [{sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(2)]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(4)]}}]]}
       [4, [1; 3]; 2, [1]])
    );

    "Skolemization with no existential variables" >:: (fun () ->
      assert_equal
       ({prefix =
         [{quant_level = 1; quantifier = A; atom = 1};
          {quant_level = 3; quantifier = A; atom = 3}];
        matrix =
         [[{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_2"; func_arguments = [1]})]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}}];
          [{sign = Neg; pred = {pred_name = "p"; pred_arguments =
            [Func({func_name = "f_2"; func_arguments = [1]})]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_4"; func_arguments = [1; 3]})]}}]]})
      (skolemization
       {prefix =
         [{quant_level = 1; quantifier = A; atom = 1};
          {quant_level = 3; quantifier = A; atom = 3}];
        matrix =
         [[{sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(1)]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_2"; func_arguments = [1]})]}};
           {sign = Neg; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}}];
          [{sign = Neg; pred = {pred_name = "p"; pred_arguments =
            [Func({func_name = "f_2"; func_arguments = [1]})]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments = [Atom(3)]}};
           {sign = Pos; pred = {pred_name = "p"; pred_arguments =
             [Func({func_name = "f_4"; func_arguments = [1; 3]})]}}]]}
       [])
    );
  ]

let _ = run_test_tt ~verbose:true test_fixture
