namespace TestCommandLine {

    @test
    def emptyCommand(): Bool = {
        let parser = CommandLine.emptyParser();
        let result = CommandLine.parse(Nil, parser);
        match result {
            case Err(_) => false
            case Ok(success) =>
                Map.isEmpty(success.options) && List.isEmpty(success.tailArgs)
        }
    }
    
    @test
    def fail(): Bool = false
}
