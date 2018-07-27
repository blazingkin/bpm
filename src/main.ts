import * as yargs from 'yargs'
import { normalize } from 'path'

let pathOptions: yargs.Options = {
    describe: 'path to pulse directory',
    default: '.',
    alias: 'p',
    normalize: true
}

// In the future, use the method here:
//   https://github.com/yargs/yargs/blob/master/docs/api.md#envprefix
// to allow users to store cardiovascular credentials in env variables
let argv: yargs.Arguments = yargs
                                        //flat?
    .scriptName('bpm')
    .command('init [path]', 'initialize a blank pulse', {
        path: pathOptions
    })
    .command(['beat', '$0'], 'synchronize dependencies')
    .help('h')
    .argv

console.log(argv)

switch (argv._[0]) {
    case "init":
        initialize(argv.path)
        break

    default:
        break
}

function initialize(path: String) {
    // make and fill directory, overwrite optional (prompt)
}