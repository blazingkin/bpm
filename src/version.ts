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

    public asString() {
        let mes: string | undefined

        if (this.message) {
            switch (this.message) {
                case " ":
                    mes = this.message
                    break;

                default:
                    mes = `.${ this.message }`
                    break;
            }
        }
        return `${ this.major }.${ this.minor }.${ this.patch }${ mes }`
    }
}
