import { Version } from "./version"

export enum Scope { Prod, Development }

export class Dependency {
    public name: string
    public resolvedVersion?: Version

    protected scope: Scope
    protected versionConstraint: string // TODO: version constraint class

    public constructor(name: string,
                       scope: Scope,
                       resolvedVersion?: Version,
                       versionConstraint?: string) {
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
    }
}
