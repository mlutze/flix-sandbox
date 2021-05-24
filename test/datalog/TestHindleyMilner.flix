namespace TestHindleyMilner {
  use HindleyMilner.Expr;
  use HindleyMilner.Expr.{Var, Abs, App};
  use HindleyMilner.infer;

  @test
  def testInfer(): Bool = {
    let id = Abs(1, Var(1), 0);
    infer(id) == Nil
  }
}