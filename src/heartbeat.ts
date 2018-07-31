import * as fs from "fs"
import { Dependency, Scope } from "./dependency"

// TODO: Comments, etc.
export function parseHeartbeat(): [Dependency] {
    const dependencies: [Dependency] = [(new Dependency("", Scope.Development))]
    // Initially, this Regex did not work because it included \n, and split()
    // removes the \n from each line.
    // TODO: Support for |dependency\n|, i.e. without a version constraint.
    const dependencyRegex: RegExp = /\s*(.+),\s(.*)/

    // TODO: Scope (like dev:\n\t... and prod:\n\t...), and only do this under
    // those headings.
    fs.readFileSync("Heartbeat").toString().split("\n").forEach((line) => {
        const parsedLine: RegExpExecArray | null = dependencyRegex.exec(line)

        if (parsedLine !== null) {
            // First element of match array is whole matched string, so start
            // checking at index 1.
            if (parsedLine[1] !== null && parsedLine[2] !== null) {
                dependencies.push(new Dependency(parsedLine[1],
                                                Scope.Development,
                                                undefined,
                                                parsedLine[2]))
            } else if (parsedLine[1] !== null && parsedLine[2] === null) {
                dependencies.push(new Dependency(parsedLine[1],
                                                Scope.Development))
            }
        }
    })

    // To get around hacky empty-array definition above, drop the first empty
    // element using shift.
    if (dependencies.length > 1) {
        dependencies.shift()
    }

    return dependencies
}
