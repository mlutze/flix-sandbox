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

    pub enum Rule[a] {
        case MutuallyExclusive(List[a])
        case Codependent(List[a])
        case Required(a)
        case NoTailArgs
    }

    type alias Parser[a] = {
        args: List[a],
        options: Map[String, (a, Bool)],
        rules: List[Rule[a]]
    }

    type alias MultiMap[k, v] = Map[k, List[v]]

    def insert(key: k, value: v, map: MultiMap[k, v]): MultiMap[k, v] = {
        Map.insertWith(List.append, key, value :: Nil, map)
    }

    pub def emptyParser(): Parser[a] = {
        args = Nil,
        options = Map#{},
        rules = Nil
    }

    pub def withArg(value: a, parser: Parser[a]): Parser[a] = { args = value :: parser.args | parser }
    pub def withRule(rule: Rule[a], parser: Parser[a]): Parser[a] = { rules = rule :: parser.rules | parser }
    
    type alias ParsingSuccess[a] = {
        options: MultiMap[a, String],
        tailArgs: List[String]
    }

    type alias ParsingResult[a] = Result[ParsingSuccess[a], ParsingError[a]]

    pub enum ParsingError[a] {
        case UnexpectedOption(String)
        case BrokenRule(Rule[a])
        case UnexpectedArgument(a)
        case MissingOptionValue(a)
    }

    pub def parse(args: List[String], parser: Parser[a]): ParsingResult[a] = {
        parseHelper(args, parser, Map#{}, Nil)
    }

    def parseHelper(args: List[String], parser: Parser[a], matchedArgs: MultiMap[a, String], tailArgs: List[String]): ParsingResult[a] = {
        match args {
            case Nil => checkRules(matchedArgs, tailArgs, parser.rules)
            case head :: tail => // last argument
                match (argType(head)) {
                    // Case 1: simple argument
                    case Argument(arg) => match parser.args {
                        // The argument is expected
                        case name :: names =>
                            let matchedArgs2 = insert(name, arg, matchedArgs);
                            let parser2 = { args = names | parser };
                            parseHelper(args, parser2, matchedArgs2, tailArgs)
                        // The argument is not expected
                        case Nil =>
                            parseHelper(args, parser, matchedArgs, arg :: tailArgs)
                    }
                    // Case 2: long option
                    case LongOption(opt) => match Map.get(opt, parser.options) {
                        // The option is expected and takes a value
                        case Some((name, true)) =>
                            match tail {
                                // The option has a value
                                case value :: rest =>
                                    let matchedArgs2 = insert(name, value, matchedArgs);
                                    parseHelper(rest, parser, matchedArgs2, tailArgs)
                                // The option has no value
                                case Nil =>
                                    Err(MissingOptionValue(name))
                            }
                        // The option is expected and does not take a value
                        case Some((name, false)) =>
                            let matchedArgs2 = insert(name, "true", matchedArgs);
                            parseHelper(tail, parser, matchedArgs2, tailArgs)
                        // The option is not expected.
                        case None =>
                            Err(UnexpectedOption(head))
                    }
                    // Case 3: short option
                    case ShortOption(opt) => match Map.get(opt, parser.options) {
                        // The option is expected and takes a value
                        case Some((name, true)) => ??? // TODO
                        case _ => ???
                    }
                }
        }
    }

    enum ArgType {
        case Argument(String)
        case ShortOption(String)
        case LongOption(String)
    }

    def argType(arg: String): ArgType = { // TODO handle -- and - differently
        if (String.startsWith(arg, "--")) {
            LongOption(String.drop(2, arg))
        } else if (String.startsWith(arg, "-")) {
            ShortOption(String.drop(1, arg))
        } else {
            Argument(arg)
        }
    }

    def checkRules(options: MultiMap[a, String], tailArgs: List[String], rules: List[Rule[a]]): ParsingResult[a] = {
        let brokenRule = List.find(breaksRule(options, tailArgs), rules);
        match brokenRule {
            case None => Ok({
                options = options,
                tailArgs = tailArgs
            })
            case Some(rule) => Err(BrokenRule(rule))
        }
    }

    def breaksRule(options: MultiMap[a, String], tailArgs: List[String], rule: Rule[a]): Bool ={
        match rule {
            case MutuallyExclusive(opts) =>
                let matches = Map.count(k -> _ -> List.memberOf(k, opts), options);
                matches > 1
            case Codependent(opts) =>
                let matches = Map.count(k -> _ -> List.memberOf(k, opts), options);
                matches != 0 and matches != List.length(opts)
            case Required(opt) =>
                not Map.memberOf(opt, options)
            case NoTailArgs =>
                not List.isEmpty(tailArgs)
        }
    }
}