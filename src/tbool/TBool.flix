namespace TBool {

    pub opaque type TBool[_b :# Bool] = Unit

    class CBool[b :# Bool] {
        def toTerm(t: TBool[b]): Bool
    }

    instance CBool[true] {
        def toTerm(t: TBool[true]): Bool = true
    }

    instance CBool[false] {
        def toTerm(t: TBool[false]): Bool = false
    }

    class IsTrue[b :# Bool]

    instance IsTrue[true]

    class IsFalse[a]

    instance IsFalse[false]

    class Something[a]

    instance Something[a -> b & e] with [e : IsTrue]

}