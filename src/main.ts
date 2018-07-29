import * as yargs from "yargs"

const pathOptions: yargs.Options = {
    alias: "p",
    default: ".",
    describe: "path to pulse directory",
    normalize: true}

// In the future, use the method here:
//   https://github.com/yargs/yargs/blob/master/docs/api.md#envprefix
// to allow users to store cardiovascular credentials in env variables
const argv: yargs.Arguments = yargs
    .scriptName("bpm")
    .command("init [path]", "initialize a blank pulse", {path: pathOptions})
    .command(["beat", "$0"], "synchronize dependencies")
    .help("h")
    .argv

switch (argv._[0]) {
    case "init":
        // TODO: CLI helper (like yeoman or npm init)
        console.log("not implemented. feel free to help at\n\
\t https://github.com/J-Vaughan/bpm")
        break

    default:
        break
}
