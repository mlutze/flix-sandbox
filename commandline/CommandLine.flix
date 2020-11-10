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
    type alias Parser[a] = Map[String, a]

    enum Arg {
        case Positional
        case Named
    }

    type alias Arg = {
        shortName: Option[String],
        longName: Option[String]
    }

    enum Rule {
        case MutuallyExclusive(List[Arg])
        case Codependent(List[Arg])
    }

    type alias Parser[a] = {
        positionalArgs: List[a],
        namedArgs: Map[]
    }

    def emptyParser(): Parser[a] = {
        positionalArgs = Nil,
        namedArgs = Map#{}
    }

    def withArg(name: String, parser: Parser): = {

    }
    
    type alias ParsingResult[a] = Result[Map[a, String], ParsingError]

    enum ParsingError {
        case UnexpectedOption(String)
    }

    pub def parse(args: List[String], parser: Parser[a]): ParsingResult[a] = {
        match args {
            case Nil
        }
    }
}