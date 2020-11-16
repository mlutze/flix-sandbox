type alias Automaton[q, a] = {
    states: Set[q],
    alphabet: Set[a],
    transitions: Set[(q, a, q)],
    initialStates: Set[q],
    finalStates: Set[q]
}

/// Returns true if the automaton accepts the input.
pub def accepts(input: List[a], automaton: Automaton[q, a]): Bool = {
    let lookup_one = state -> input_one -> Set.flatMap(x ->
        match x {
            case (from, in, to) if from == state && in == input_one => Set#{to}
            case _ => Set#{}
        }
        )(automaton.transitions);
    let lookup_all = states -> input_one -> Set.flatMap(state -> lookup_one(state, input_one))(states);
    let finalStates = Set.toList(List.fold(lookup_all, automaton.initialStates, input));
    List.exists(state -> Set.memberOf(state, automaton.finalStates))(finalStates)
}

/// An automaton is normal if:
/// * It has 1 initial state
/// * It has 1 final state
/// * It has no transitions entering the initial state
/// * It has no transitions leaving the final state
pub def isNormal(automaton: Automaton[q, a]): Bool = {
    Set.size(automaton.initialStates) == 1 &&
    Set.size(automaton.finalStates) == 1 &&
    !Set.exists(x -> 
        match x {
            case (from, _, to) => Set.memberOf(to, automaton.initialStates) || Set.memberOf(from, automaton.finalStates)
        }
    )(automaton.transitions)
}

pub def normalize(automaton: Automaton[q, a], newInitial: q, newFinal: q): Automaton[q, a] = {
    let leavingInitial = Set.filter(x -> 
        match x {
            case (from, _, _) => Set.memberOf(from, automaton.initialStates)
        }
    )(automaton.transitions);
    let enteringFinal = Set.filter(x ->
        match x {
            case (_, _, to) => Set.memberOf(to, automaton.finalStates)
        }
    )(automaton.transitions);
    let leavingNewInitial = Set.map(x ->
        match x {
            case (_, in, to) => (newInitial, in, to)
        }
    )(leavingInitial);
    let enteringNewFinal = Set.map(x ->
        match x {
            case (from, in, _) => (from, in, newFinal) 
        }
    )(enteringFinal);
    { 
        states = Set.union(automaton.states, Set#{newInitial, newFinal}),
        alphabet = automaton.alphabet,
        transitions = Set.union(Set.union(automaton.transitions, leavingNewInitial), enteringNewFinal),
        initialStates = Set#{newInitial},
        finalStates = Set#{newFinal}
    }
}

pub def charToStringAutomaton(automaton: Automaton[q, Char]): Automaton[q, String] = {
    alphabet = Set.map(Char.toString, automaton.alphabet),
    transitions = Set.map(x -> 
        match x {
            case (from, in, to) => (from, Char.toString(in), to)
        }
    )(automaton.transitions)
    | automaton
}