import { Version } from "./version";

export class Metadata {
    public name: string
    public version: Version

    public author?: string
    public repository?: string
    public license?: string // TODO: SPX License identifier verification.
    public category?: string

    public constructor(name: string, version: Version) {
        this.name = name
        this.version = version

        // TODO: Fill out the rest of the optional fields.
    }
}
