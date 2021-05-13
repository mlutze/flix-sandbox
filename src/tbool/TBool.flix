namespace TBool {

    pub opaque type TBool[_b: Bool] = Unit

    lawless class CBool[b: Bool] {
        pub def toTerm(t: TBool[b]): Bool
    }

    instance CBool[true] {
        pub def toTerm(_: TBool[true]): Bool = true
    }

    instance CBool[false] {
        pub def toTerm(_: TBool[false]): Bool = false
    }

    class IsTrue[b: Bool]

    instance IsTrue[true]

    class IsFalse[a]

    instance IsFalse[false]

    class Something[a]

    instance Something[a -> b & e] with IsTrue[e]

}