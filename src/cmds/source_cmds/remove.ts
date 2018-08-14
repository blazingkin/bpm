import { Arguments } from "yargs"

exports.command = "remove <name> [names..]"
exports.desc = "remove named sources"
exports.builder = {}
exports.handler = (argv: Arguments) => {
    console.log("removing sources %s",
                [].concat(argv.name).concat(argv.names).join(", "))
}
