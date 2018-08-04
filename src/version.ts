export class Version {

    public static fromString(input: string): Version {
        const semVerRegexp: RegExp = /\s*(\d+)\.(\d+)\.(\d+)\.?(.*)?/
        let version: Version

        if (semVerRegexp.test(input)) {
            // ! ignores null-check, because null is checked for above.
            const m = semVerRegexp.exec(input)!
            version = new Version(parseInt(m[1], 10),
                                  parseInt(m[2], 10),
                                  parseInt(m[3], 10),
                                  m[4] ? m[4] : undefined)
        } else {
            console.error("Incompatible version syntax")
            process.exit(1)
        }

        // ! ignores null-check, because null is checked for above.
        return version!
    }
    public major: number
    public minor: number
    public patch: number
    public message?: string

    public constructor(maj: number,
                       min: number,
                       pat: number,
                       mes?: string) {
        this.major = maj
        this.minor = min
        this.patch = pat
        if (mes) {
            this.message = mes
        }
    }

    public toString() {
        let mes: string | null

        if (this.message !== undefined) {
            switch (this.message) {
                case " ": // TODO: Account for -message, _message, .message, etc
                    mes = this.message
                    break;

                default:
                    mes = `.${ this.message }`
                    break;
            }
        } else {
            mes = null
        }
        return `${ this.major }.${ this.minor }.${ this.patch }${ mes ? mes : "" }`
    }
}
