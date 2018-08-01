import * as fs from "fs"
import { Dependency, Scope } from "./dependency"
import { Metadata } from "./metadata"
import { Version } from "./version"

export class Heartbeat {
    public metadata: Metadata
    public devDeps?: Dependency[]
    public prodDeps?: Dependency[]

    public constructor(m: Metadata, d?: Dependency[], p?: Dependency[]) {
        // Maybe pass in name and version strings instead of relying on calling
        // function to create a new Metadata object.
        this.metadata = m
        this.devDeps = d
        this.prodDeps = p
    }
}

export function parseHB(): Heartbeat {
    // TODO: Allow other configurable names (~/.bpmrc?)
    const HB: string[] = fs.readFileSync("Heartbeat").toString().split("\n")
    // Temporarily holds name out of for-loop scope.
    let name: string | null = null
    // Temporarily holds version out of for-loop scope.
    let version: Version | null = null

    // Removes and assigns metadata.
    for (let index = 0; index < HB.length; index++) {
        const line = HB[index];
        // Captures name in [1].
        const nameRegExp: RegExp = /\bname:\s+(.*)\s*/
        // Captures maj, min, pat, optionally msg, in [1-4].
        const versionRegExp: RegExp = /\bversion:\s+(\d+)\.(\d+)\.(\d+)\.?(.*)?/

        // Tests and assigns name field for metadata.
        if (nameRegExp.test(line)) {
            // ! ignores null-check, because null is checked for above.
            name = nameRegExp.exec(line)![1]
        }

        // Tests, constructs, and assigns version field for metadata.
        if (versionRegExp.test(line)) {
            // Constructs and passes Version object to version.
            const m = versionRegExp.exec(line)! // See explanation above.
            version = new Version(parseInt(m[1], 10),
                                  parseInt(m[2], 10),
                                  parseInt(m[3], 10),
                                  m[4] ? m[4] : undefined)
        }

        if (name && version) {
            break
        }
    }

    // Check if name and version exist.
    if (name === null || version === null) {
        console.error("name or version field not found in Heartbeat.")
    }

    // Pass the remaining strings to parseDeps.
    // TODO: Parse and pass the scopes to parseDeps.
    const parsedDependencies: Dependency[] = parseDeps(HB)

    // Construct Metadata object from name and version. ! overrides null-check,
    // since existence is tested above.
    const metadata: Metadata = new Metadata(name!, version!)

    // Construct and return Heartbeat object.
    return new Heartbeat(metadata, parsedDependencies)
}

// TODO: Comments (in Heartbeat), etc.
function parseDeps(lines: string[]/*, scope: Scope*/): Dependency[] {
    const dependencies: Dependency[] = [(new Dependency("", Scope.Development))]
    // Initially, this Regex did not work because it included \n, and split()
    // removes the \n from each line.
    // TODO: Support for |dependency\n|, i.e. without a version constraint.
    const dependencyRegex: RegExp = /\s*(.+),\s(.*)/

    // TODO: Scope (like dev:\n\t... and prod:\n\t...), and only do this under
    // those headings.
    lines.forEach((line) => {
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
