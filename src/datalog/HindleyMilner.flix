namespace HindleyMilner {
  /// var |-> tpe
  rel Subst(var: Int, tpe: Int)
  /// var |-> (fromTpe -> toTpe)
  rel FSubst(var: Int, fromTpe: Int, toTpe: Int)

  pub enum Expr {
    case Var(Int)
    case Abs(Int, Expr, Int)
    case App(Expr, Expr, Int) 
  }

  pub def infer(expr: Expr): List[(Int, Int)] = {
    (query getConstraints(expr), rules()
      select (a, b) from Subst(a, b) |> Array.toList) as & Pure
  }

  def rules(): #{ FSubst, Subst } = #{
    Subst(x, y) :- FSubst(var, x, _), FSubst(var, y, _), if x < y.
    Subst(x, y) :- FSubst(var, _, x), FSubst(var, _, y), if x < y.
  }

  def getConstraints(expr: Expr): #{ FSubst, Subst } = match expr {
    case Var(_) => #{}
    case Abs(x, body, id) =>
      let bodyId = getId(body);
      #{
        FSubst(id, x, bodyId).
      } <+> getConstraints(body)
    case App(func, arg, id) =>
      let funcId = getId(func);
      let argId = getId(arg);
      #{
        FSubst(funcId, argId, id).
      } <+> getConstraints(func) <+> getConstraints(arg)
  }

  def getId(expr: Expr): Int = match expr {
    case Var(id) => id
    case Abs(_, _, id) => id
    case App(_, _, id) => id
  }
}