namespace TestRegex {
    use Regex.Regex;
    use Regex.parse;

    @test
    def empty(): Bool = {
        parse("") == None
    }

    @test
    def singleChar(): Bool = {
        parse("a") == Some(Regex.Lit('a'))
    }

    @test
    def concat(): Bool = {
        parse("abc") == Some(Regex.Concat(Regex.Lit('a'), Regex.Concat(Regex.Lit('b'), Regex.Lit('c'))))
    }

    @test
    def star(): Bool = {
        parse("a*") == Some(Regex.Star(Regex.Lit('a')))
    }

    @test
    def pipe(): Bool = {
        parse("a|b") == Some(Regex.Or(Regex.Lit('a'), Regex.Lit('b')))
    }

    @test
    def complex1(): Bool = {
        parse("(a*b|c)*|(de)") == 
            Some(Regex.Or(
                Regex.Star(
                    Regex.Or(
                        Regex.Concat(
                            Regex.Star(
                                Regex.Lit('a')),
                            Regex.Lit('b')),
                        Regex.Lit('c'))),
                Regex.Concat(
                    Regex.Lit('d'),
                    Regex.Lit('e'))))

    }
}