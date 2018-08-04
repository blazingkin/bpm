import { Version } from "./version"

export enum Scope { Production, Development, Global }

export class Dependency {
    public name: string
    public resolvedVersion?: Version

    public scope: Scope
    public versionConstraint: string // TODO: version constraint class

    // public subDependencies?: Dependency[]

    public constructor(name: string,
                       scope: Scope,
                       resolvedVersion?: Version,
                       versionConstraint?: string,
                     /*subdeps?: [Dependency]*/) {
        this.name = name
        this.scope = scope

        if (versionConstraint) {
            this.versionConstraint = versionConstraint
        } else {
            this.versionConstraint = "*" // Any
        }

        if (resolvedVersion) {
            this.resolvedVersion = resolvedVersion
        }

        /* if (subdeps) {
            this.subDependencies = subdeps
        } */
    }
}
