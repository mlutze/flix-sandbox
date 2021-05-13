namespace ListZipper {

  instance Eq[ListZipper[a]] with Eq[a] {
    pub def eq(lz1: ListZipper[a], lz2: ListZipper[a]): Bool = match (lz1, lz2) {
      case (ListZipper(l1, l2), ListZipper(r1, r2)) => l1 == r1 and l2 == r2
    }
  }

  opaque type ListZipper[a] = (List[a], List[a])

  pub def left(lz: ListZipper[a]): Option[ListZipper[a]] = match lz {
    case ListZipper(Nil, _) => None
    case ListZipper(x :: l, r) => Some(ListZipper(l, x :: r))
  }

  pub def right(lz: ListZipper[a]): Option[ListZipper[a]] = match lz {
    case ListZipper(_, Nil) => None
    case ListZipper(l, x :: r) => Some(ListZipper(x :: l, r))
  }

  pub def insert(x: a, lz: ListZipper[a]): ListZipper[a] = {
    let ListZipper(l, r) = lz;
    ListZipper(l, x :: r)
  }

  pub def delete(lz: ListZipper[a]): Option[ListZipper[a]] = match lz {
    case ListZipper(_, Nil) => None
    case ListZipper(l, _ :: r) => Some(ListZipper(l, r))
  }

  pub def fromList(l: List[a]): ListZipper[a] = ListZipper(Nil, l)

  pub def toList(lz: ListZipper[a]): List[a] = {
    let ListZipper(l, r) = lz;
    List.reverse(l) ::: r
  }
}