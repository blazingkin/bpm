import { Arguments, Argv} from "yargs"

exports.command = "source <command>"
exports.desc = "manage tracked sources"
exports.builder = (yargs: Argv) => {
    return yargs.commandDir("source_cmds")
}
exports.handler = (argv: Arguments) => {
    console.log()
}
