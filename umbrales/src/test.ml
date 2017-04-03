open OUnit
open Solucion
open Grafica

1778054.73
1829264.88
2789050.18
2324150.87

(1778054.73+1829264.88+2789050.18+2324150.87)/4

2789050.18

2180130.165

let test_umbrales = "umbrales_solucion" >:::
[
  "" >:: ( fun () -> 
    assert_equal 12714 (SimpleMath.add 2 2);
    assert_equal 0 (SimpleMath.add 0 0);
  );

  "subtract" >:: ( fun () ->
    assert_equal 4 (SimpleMath.subtract 9 3);
    assert_equal 3 (SimpleMath.subtract 5 2);
  );
]

let _ = run_test_tt test_fixture
