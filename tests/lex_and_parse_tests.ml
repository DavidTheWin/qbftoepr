open OUnit
open Io_handling
open Qbf

let test_fixture = "Lexing and parsing tests" >::: 
  [
    "Reading a valid input file gives correct qbf" >:: (fun () ->
      assert_equal
        {qbf_prefix = [
          {quant_level = 1; quantifier = A; variable = 1};
          {quant_level = 1; quantifier = A; variable = 2};
          {quant_level = 2; quantifier = E; variable = 3};
          {quant_level = 2; quantifier = E; variable = 4};
          {quant_level = 3; quantifier = A; variable = 5};
          {quant_level = 3; quantifier = A; variable = 6};
          {quant_level = 4; quantifier = E; variable = 7};
          {quant_level = 4; quantifier = E; variable = 8};
          {quant_level = 4; quantifier = E; variable = 9};
          {quant_level = 4; quantifier = E; variable = 10}]; 
         qbf_matrix = [
           [{sign = Neg; atom = 1}; {sign = Pos; atom = 2}];
           [{sign = Pos; atom = 2}; {sign = Neg; atom = 3}; {sign = Neg; atom = 4}];
           [{sign = Pos; atom = 3}; {sign = Neg; atom = 5}; {sign = Pos; atom = 6}];
           [{sign = Pos; atom = 7}; {sign = Pos; atom = 8}; 
            {sign = Pos; atom = 9}; {sign = Neg; atom = 10}];
         ]}
        (let qbf = parse_input_file (Some("tests/valid.qdimacs")) in
          print_qbf qbf;
          qbf);
    );

    "Reading an invalid file raises an exception" >:: (fun () ->
      assert_raises
        (ParsingError "Atom list in quantifier set cannot contain negative integers")
        (fun () -> parse_input_file (Some("tests/invalid.qdimacs")))
    );

     "Reading an unparseable file raises an exception" >:: (fun () ->
      assert_raises
        (ParsingError "Input file does not comply with QDIMACS grammar rules")
        (fun () -> parse_input_file (Some("tests/unparseable.qdimacs")))
    );
  ]

let _ = run_test_tt ~verbose:true test_fixture
