namespace TestCommandLine {

    @test
    def emptyCommand(): Bool = {
        let parser = CommandLine.emptyParser();
        let result = CommandLine.parse(Nil, parser);
        match result {
            case Err(_) => false
            case Ok(success) =>
                Map.isEmpty(success.options) and List.isEmpty(success.tailArgs)
        }
    }
}
