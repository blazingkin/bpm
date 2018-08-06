import { Arguments } from "yargs"
import { pump } from "../pump"

exports.command = "pump"
exports.desc = "upload current version to cardiovascular"
exports.builder = {}
exports.handler = (argv: Arguments) => {
    pump()
}
