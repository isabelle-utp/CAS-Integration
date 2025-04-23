section \<open> Substitutions as ODEs \<close>

theory Subst_ODE
  imports "Shallow_Expressions.Substitutions" "Hybrid-Library.Derivative_Lib"
begin

ML_file \<open>Arith_Expr.ML\<close>
ML_file \<open>Subst_ODE.ML\<close>
ML_file \<open>wolfram-integration/lex_wolfram.ML\<close>
ML_file \<open>wolfram-integration/parse_wolfram.ML\<close>
ML_file \<open>wolfram-integration/isabelle_to_wolfram.ML\<close>
ML_file \<open>wolfram-integration/wolfram_to_isabelle.ML\<close>

ML \<open>
structure Solve_Subst_ODE =
struct
  fun solve_subst_ode ctx sode =
  let
    open Isabelle_To_Wolfram; open Wolfram_To_Isabelle; open Subst_ODE;
    val sode' as {odes=ode_table, ...} = subst_ode "t" sode;
    val vars = Symtab.keys ode_table
    val extra_svars = Symtab.fold (fn (_, x) => fn y => Symtab.merge (fn _ => true) (y, Arith_Expr.collect_svars x)) ode_table Symtab.empty
    val svars = Symtab.merge (fn _ => true) (Symtab.make_set vars, extra_svars)
    (* We interpret variables without a derivative as constant *)
    val sode = Arith_Expr.sode_conv (Symtab.make (ListPair.zip (vars, vars))) sode'
    val out = wolfram_exec (translate_sode sode);
    val mexp = Parse_Wolfram.parse out;
    val rules = (map (map to_rule o to_list) o to_list) mexp;
    val interpreted = interpret_ode sode (hd rules)
    val reconstructed = Symtab.map (fn _ => fn exp => Arith_Expr.promote_svars svars exp) interpreted
    val tm = ode_subst ctx "t" reconstructed
  in writeln (Syntax.string_of_term ctx tm); Syntax.check_term ctx tm
  end;

  fun solve_subst_ode_cmd ctx sode = 
    "Found ODE solution: " ^ Active.sendback_markup_command (Syntax.string_of_term ctx (solve_subst_ode ctx sode)) |> writeln;
end;
\<close>

end
