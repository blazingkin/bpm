import * as yargs from "yargs"
import { beat } from "./beat"
import { Dependency } from "./dependency"
import { Heartbeat, parseHB } from "./heartbeat"

const pathOptions: yargs.Options = {
    alias: "p",
    default: ".",
    describe: "path to pulse directory",
    normalize: true}

// In the future, use the method here:
//   https://github.com/yargs/yargs/blob/master/docs/api.md#envprefix
// to allow users to store cardiovascular credentials in env variables.

// In the near future, use this:
//   https://github.com/yargs/yargs/blob/master/docs/advanced.md#commanddirdirectory-opts
const argv: yargs.Arguments = yargs
    .scriptName("bpm")
    .command("init [path]", "initialize a blank pulse", {path: pathOptions})
    .command(["beat"], "synchronize dependencies")
    .help("h")
    .argv

switch (argv._[0]) {
    case "init":
        // TODO: CLI helper (like yeoman or npm init) (Must include autolink to
        // Core package, since it's required by all Blaze programs.)
        console.log("not implemented. feel free to help at\n\
\t https://github.com/J-Vaughan/bpm")
        break

    case "beat":
        // TODO: Standardized output
        console.log("beating")
        devBeat(parseHB())
        break;

    default:
        yargs.showHelp()
        break
}

// UNSAFE
function devBeat(HB: Heartbeat): void {
    console.log(HB.metadata.name)
    console.log(HB.metadata.version.toString())
    beat(HB.devDeps!)
}
