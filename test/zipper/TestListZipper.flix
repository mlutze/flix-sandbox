namespace TestListZipper {

  @test
  def leftFail(): Bool = {
    let lz = ListZipper.fromList(1 :: Nil);
    ListZipper.left(lz) == None
  }

  @test
  def rightFail(): Bool = {
    let lz = ListZipper.fromList(1 :: Nil);
    match ListZipper.right(lz) {
      case None => false
      case Some(lz2) => ListZipper.right(lz2) == None
    }
  }

  @test
  def insert(): Bool = {
    let lz = ListZipper.fromList(1 :: 3 :: Nil);
    match ListZipper.right(lz) {
      case None => false
      case Some(lz2) => ListZipper.toList(ListZipper.insert(2, lz2)) == 1 :: 2 :: 3 :: Nil
    }
  }

  @test
  def deleteFail(): Bool = {
    let lz = ListZipper.fromList(1 :: Nil);
    match ListZipper.right(lz) {
      case None => false
      case Some(lz2) => ListZipper.delete(lz2) == None
    }
  }

  @test
  def delete(): Bool = {
    let lz = ListZipper.fromList(1 :: 2 :: 3 :: Nil);
    match ListZipper.right(lz) {
      case None => false
      case Some(lz2) => match ListZipper.delete(lz2) {
        case None => false
        case Some(lz3) => ListZipper.toList(lz3) == 1 :: 3 :: Nil
      }
    }
  }
}