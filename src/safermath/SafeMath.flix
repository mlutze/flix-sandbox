namespace SafeMath {
  pub enum SafeInt {
    case Value(BigInt)
    case Undefined
  }

  instance Mul[SafeInt] {
    pub def mul(x: SafeInt, y: SafeInt): SafeInt = match (x, y) {
      case (Value(x1), Value(y1)) => Value(x1 * y1)
      case _ => Undefined
    }
  }

  instance Div[SafeInt] {
    pub def div(x: SafeInt, y: SafeInt): SafeInt = match (x, y) {
      // protect from div by zero
      case (Value(_), Value(0ii)) => Undefined
      case (Value(x1), Value(y1)) => Value(x1 / y1)
      case _ => Undefined
    }
  }

  instance Add[SafeInt] {
    pub def add(x: SafeInt, y: SafeInt): SafeInt = match (x, y) {
      case (Value(x1), Value(y1)) => Value(x1 + y1)
      case _ => Undefined
    }
  }

  instance Sub[SafeInt] {
    pub def sub(x: SafeInt, y: SafeInt): SafeInt = match (x, y) {
      case (Value(x1), Value(y1)) => Value(x1 - y1)
      case _ => Undefined
    }
  }

  pub def toBigInt(i: SafeInt): Option[BigInt] = match i {
    case Value(v) => Some(v)
    case Undefined => None
  }

  pub def ofBigInt(i: BigInt): SafeInt = Value(i)
}