import * as yargs from "yargs"

// In the future, use the method here:
//   https://github.com/yargs/yargs/blob/master/docs/api.md#envprefix
// to allow users to store cardiovascular credentials in env variables.
const argv: yargs.Arguments = yargs
    .scriptName("bpm")
    .commandDir("cmds")
    .demandCommand()
    .help()
    .argv
