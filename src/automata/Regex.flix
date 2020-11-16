namespace Regex {
    pub enum Regex {
        case Star(Regex)
        case Or(Regex, Regex)
        case Concat(Regex, Regex)
        case Lit(Char)
    }

    /// Parses a regular expression.
    pub def parse(string: String): Option[Regex] = {
        match parseOr(String.toList(string)) {
            case Some((regex, Nil)) => Some(regex)
            case _ => None
        }
    }

    /// Parses an alternation of regexes.
    def parseOr(chars: List[Char]): Option[(Regex, List[Char])] = {
        match parseConcat(chars) {
            case Some(left, '|' :: leftRest) => 
                match parseOr(leftRest) {
                    case Some(right, rightRest) => Some(Or(left, right), rightRest)
                    case None => None
                }
            case Some(regex, rest) => Some(regex, rest)
            case None => None
        }
    }

    /// Parses a concatenation of regexes.
    def parseConcat(chars: List[Char]): Option[(Regex, List[Char])] = {
        match parseStar(chars) {
            case Some(left, '|' :: tail) => Some(left, '|' :: tail)
            case Some(left, ')' :: tail) => Some(left, ')' :: tail)
            case Some(left, '*' :: tail) => Some(left, '*' :: tail)
            case Some(left, Nil) => Some(left, Nil)
            case Some(left, leftRest) =>
                match parseConcat(leftRest) {
                    case Some(right, rightRest) => Some(Concat(left, right), rightRest)
                    case None => None
                }
            case None => None
        }
    }

    /// Parses a possibly star-tagged regex.
    def parseStar(chars: List[Char]): Option[(Regex, List[Char])] = {
        match parseUnit(chars) {
            case Some(regex, '*' :: rest) => Some(Star(regex), rest)
            case Some(regex, rest) => Some(regex, rest)
            case None => None
        }
    }

    /// Parses a single token or a parenthesized group.
    def parseUnit(chars: List[Char]): Option[(Regex, List[Char])] = {
        match chars {
            case '(' :: tail => 
                match parseOr(tail) {
                    case Some((left, ')' :: leftRest)) => Some(left, leftRest)
                    case _ => None
                }
            case '*' :: _ => None
            case '|' :: _ => None
            case ')' :: _ => None
            case Nil => None
            case c :: tail => Some((Lit(c), tail))
            
        }
    }

    pub def toAutomaton(regex: Regex): Automaton[Int, Char] = {
        ???
    }
}