namespace CommandLine {
/*
    from https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html

    Arguments are options if they begin with a hyphen delimiter (‘-’).
    Multiple options may follow a hyphen delimiter in a single token if the options do not take arguments. Thus, ‘-abc’ is equivalent to ‘-a -b -c’.
    Option names are single alphanumeric characters (as for isalnum; see Classification of Characters).
    Certain options require an argument. For example, the ‘-o’ command of the ld command requires an argument—an output file name.
    An option and its argument may or may not appear as separate tokens. (In other words, the whitespace separating them is optional.) Thus, ‘-o foo’ and ‘-ofoo’ are equivalent.
    Options typically precede other non-option arguments.

    The argument ‘--’ terminates all options; any following arguments are treated as non-option arguments, even if they begin with a hyphen.
    A token consisting of a single hyphen character is interpreted as an ordinary non-option argument. By convention, it is used to specify input from or output to the standard input and output streams.
    Options may be supplied in any order, or appear multiple times. The interpretation is left up to the particular application program. 

*/
    type alias Arg = {
        shortName: Option[String],
        longName: Option[String]
    }

    pub enum Rule[a] {
        case MutuallyExclusive(List[a])
        case Codependent(List[a])
        case Required(a)
    }

    type alias Parser[a] = {
        args: List[a],
        options: Map[String, a],
        rules: Set[Rule[a]]
    }

    pub def emptyParser(): Parser[a] = {
        args = Nil,
        options = Map#{},
        rules = Set#{}
    }

    pub def withArg(value: a, parser: Parser[a]): Parser[a] = { args = value :: parser.args | parser }
    
    type alias ParsingResult[a] = Result[Map[a, String], ParsingError[a]]

    pub enum ParsingError[a] {
        case UnexpectedOption(String)
        case BrokenRule(Rule[a])
    }

    pub def parse(args: List[String], parser: Parser[a]): ParsingResult[a] = {
        ???
    }

}