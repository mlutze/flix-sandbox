namespace TypeClasses {
  enum Animal {
    case Cat
    case Dog
  }

  pub def areAllEqual(x: Animal, y: Animal, z: Animal): Bool = {
    x == y and y == z
  }

  instance Eq[Animal] {
    pub def eq(l: Animal, r: Animal): Bool = match (l, r) {
      case (Cat, Cat) => true
      case (Dog, Dog) => true
      case _ => false
    }
  }
}