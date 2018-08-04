import * as bombadil from "@sgarciac/bombadil"
import * as fs from "fs"
import { Dependency, Scope } from "./dependency"
import { Metadata } from "./metadata"
import { Version } from "./version"

export class Heartbeat {
    public metadata: Metadata
    public deps: Dependency[]
    public devDeps?: Dependency[]
    public prodDeps?: Dependency[]

    public constructor(m: Metadata,
                       d: Dependency[],
                       v?: Dependency[],
                       p?: Dependency[]) {
        // Maybe pass in name and version strings instead of relying on calling
        // function to create a new Metadata object.
        this.metadata = m
        this.deps = d
        this.devDeps = v
        this.prodDeps = p
    }
}

export function parseHB(): Heartbeat {
    // Reads and stores contents of Heartbeat.toml.
    // TODO: Allow other configurable names (~/.bpmrc?)
    const reader: bombadil.TomlReader = new bombadil.TomlReader()
    // Holds Metadata.
    let metadata: Metadata

    // Temporary dependency arrays.
    // tslint:disable-next-line:prefer-const
    let deps: Dependency[]
    // tslint:disable-next-line:prefer-const
    let devDeps: Dependency[]
    // tslint:disable-next-line:prefer-const
    let prodDeps: Dependency[]

    // Checks if Heartbeat exists.
    if (fs.existsSync("Heartbeat.toml")) {
        // Read and parse TOML.
        reader.readToml(fs.readFileSync("Heartbeat.toml").toString())
    } else {
        console.error("Heartbeat.toml not found")
        process.exit(1)
        // Typescript throws a fit if this line isn't here, even though it's
        // unreachable.
        throw new Error("Appease me, Seymour.")
    }

    // Checks for invalid TOML.
    if (reader.result === null) {
        throw reader.errors
    }

    // Assigns reader.result to shorter name
    const HB = reader.result

    // Checks for required fields (name, version).
    if (HB.name === null) {
        console.error("name required in Heartbeat.toml")
        process.exit(1)
    } else if (HB.version === null) {
        console.error("version required in Heartbeat.toml")
        process.exit(1)
    } else {
        metadata = new Metadata(HB.name, Version.fromString(HB.version))
    }

    // Dependency existence check (required).
    if (HB.deps === null) {
        console.error("dependency field ([deps]) not found")
        process.exit(1)
    }

    // Core package existence check (required).
    if (HB.deps.Core === null) {
        console.error("Core package required")
        process.exit(1)
    }

    // Global package processing.
    Object.keys(HB.deps).forEach((key) => {
        if (key !== "dev" && key !== "prod") {
            deps.push(new Dependency(key,
                                     Scope.Global,
                                     undefined,
                                     HB.deps[key]))
        }
    })

    // Development package existence check.
    if (HB.deps.dev !== null) {
        // Development package processing.
        Object.keys(HB.deps.dev).forEach((key) => {
            devDeps.push(new Dependency(key,
                                        Scope.Development,
                                        undefined,
                                        HB.deps.dev[key]))
        })
    }

    // Production package existence check.
    if (HB.deps.prod !== null) {
        // Production package processing.
        Object.keys(HB.deps.prod).forEach((key) => {
            prodDeps.push(new Dependency(key,
                                         Scope.Production,
                                         undefined,
                                         HB.deps.prod[key]))
        })
    }

    // Constructs and returns Heartbeat object.
    // ! ignores initialized-check, because this method will not return if any
    // of the following arguments are not accounted for.
    return new Heartbeat(metadata!, deps!, devDeps!, prodDeps!)
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
