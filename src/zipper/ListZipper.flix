namespace ListZipper {
  opaque type ListZipper[a] = (List[a], List[a])

  pub def left(lz: ListZipper[a]): Option[ListZipper[a]] = match lz {
    case ListZipper(Nil, _) => None
    case ListZipper(x :: l, r) => Some(ListZipper(l, x :: r))
  }

  pub def right(lz: ListZipper[a]): Option[ListZipper[a]] = match lz {
    case ListZipper(_, Nil) => None
    case ListZipper(l, x :: r) => Some(ListZipper(x :: l, r))
  }
}