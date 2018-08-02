export class Version {
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
