import { Arguments } from "yargs"

exports.command = "init [dir]"
exports.desc = "create an empty pulse"
exports.builder = {
    dir: {
        default: ".",
    },
}
exports.handler = (argv: Arguments) => {
    console.log("init called for dir: ", argv.dir)
}
