// Forcing style guidelines is not a priority, nor is it a main feature or any
// modern package manager. I'm leaving this for later.
/*
export function initialize(project: Project, path: string) {
    const prevDir: string = process.cwd()

    if (!fs.existsSync(path)) {
        fs.mkdirSync(path)
    }

    process.chdir(path)

    fs.open("Heartbeat", "w", (err, fd) => {
        if (err) { throw err }
        fs.write(fd, JSON.stringify(project, undefined, 4), (werr, writ, str) => {
            if (werr) {throw werr}
        })
    })

    process.chdir(prevDir)
}
*/
