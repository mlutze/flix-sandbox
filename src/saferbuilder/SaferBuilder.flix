

namespace SaferBuilder {
    type alias Pizza = {
        dough: Bool,
        sauce: Bool,
        cheese: Bool
    }

    type alias Order = {
        dough: Unit,
        sauce: Unit,
        cheese: Unit
    }

    def pizzaBuilder(): ({}, Order) = {
        ({}, {dough = (), sauce = (), cheese = ()})
    }

    def withCheese(cheese: Bool, builder: ({|p}, {cheese: Unit | o})): ({cheese: Bool | p}, {|o}) = {
        match builder {
            case (pizza, order) => (
                {+cheese = cheese | pizza},
                {-cheese | order}
            )
        }
    }

    def withDough(dough: Bool, builder: ({|p}, {dough: Unit | o})): ({dough: Bool | p}, {|o}) = {
        match builder {
            case (pizza, order) => (
                {+dough = dough | pizza},
                {-dough | order}
            )
        }
    }

    def withSauce(sauce: Bool, builder: ({|p}, {sauce: Unit | o})): ({sauce: Bool | p}, {|o}) = {
        match builder {
            case (pizza, order) => (
                {+sauce = sauce | pizza},
                {-sauce | order}
            )
        }
    }

    def build(builder: (Pizza, {})): Pizza = {
        match builder {
            case (pizza, _order) => pizza
        }
    }

    @test
    def test(): Pizza = {
        let pb1 = pizzaBuilder();
        let pb2 = withDough(true, pb1);
        let pb3 = withSauce(true, pb2);
        let pb4 = withCheese(true, pb3);
        /// let pb5 = withCheese(true, pb4); error
        pb4.build()
    }


}