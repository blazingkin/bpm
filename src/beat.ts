import * as fs from "fs"
import * as os from "os"
import * as path from "path"
import { Dependency } from "./dependency"
import * as hb from "./heartbeat"
import { Version } from "./version"

const defaultSource: string = process.env.BLZPACKAGES || path.join(os.homedir(),
                                                             ".bpm", "packages")

export function beat(deps: Dependency[]): void {
    for (let index = 0; index < deps.length; index++) {
        const dep = deps[index];
        addDep(dep)
    }

    // TODO: Summary of packages added, removed, changed, whatever.
}

function addDep(dep: Dependency, source = defaultSource): void {
    const dirName: string = path.join(source, dep.name /*dep.version.toString*/)
    // const depIdentifier: string = path.join(source,
    //                                    dep.name.concat(dep.version.toString))

    // Creates "./Packages/" if it does not exist.
    // TODO: Account for running this command in, say, "$PROJECTROOT/src/".
    // Maybe it should search for a Heartbeat as an anchor for the project root?
    if (!fs.existsSync(path.join(process.cwd(), "Packages"))) {
        fs.mkdirSync(path.join(process.cwd(), "Packages"))
    }

    // Check if matching directory exists.
    if (fs.existsSync(dirName)) {
        // TODO: Version specification. I'm thinking name/0.1.0/, name/0.2.1/
        fs.symlink(path.join(source, dep.name), // Target
                   // TODO: Change dep.name into depIdentifier with version
                   // specification.
                   path.join(process.cwd(), "Packages", dep.name), // Link path
                   (err: NodeJS.ErrnoException) => {
                       if (err) {
                           throw err
                        }
                    })
    } /* else if (isInIndex(dep)) {
        getDep(dep) } */
    // Remove ↓ if above is implemented.
    // tslint:disable-next-line:one-line
    else {
        console.error(`${dep.name} not found`)
    }
}
