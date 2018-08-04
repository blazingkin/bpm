import { Arguments } from "yargs"

exports.command = "add <name> <url|path>"
exports.desc = "add remote named <name> for <url|path>"
exports.builder = {}
exports.handler = (argv: Arguments) => {
    console.log("adding remote %s for %s", argv.name, argv.url)
}
