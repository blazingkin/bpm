import { Arguments } from "yargs"
import { parseHB } from "../heartbeat"

exports.command = "beat [env]"
exports.desc = "update and sync dependencies [in env]"
exports.builder = {
    env: {
        default: "*",
    },
}
exports.handler = (argv: Arguments) => {
    parseHB()
}
