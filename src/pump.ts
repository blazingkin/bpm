import { readFileSync } from "fs"
import { join } from "path";
import * as request from "request"
import { create } from "tar"
import { parseHB } from "./heartbeat"
import { Metadata } from "./metadata"
import * as user from "./user"

const cardiovascularUrl: string = "http://localhost:2387"

export function pump() {
    // Get project metadata.
    const md = parseHB().metadata

    // Format tarball name.
    const tarName = `${md.name}-${md.version}.tgz`

    // Construst platform-agnostic absolute path to tarball.
    const tarPath = join(process.cwd(), tarName)

    // TODO: Ignore files in (.bpmignore?)
    // Create tarball of project root.
    create( { gzip: true, file: tarPath }, ["."]).then(() => {
        // Log in if token is expired.
        if (!user.isLoggedIn()) {
            user.logIn()
        }

        // In case user.logIn() fails and program does not exit for some reason.
        if (user.isLoggedIn()) {
            publish(md, tarPath)
        }
    })
}

function publish(md: Metadata, tarPath: string): void {
    // Convert tarball to base64 encoded string.
    const tarball: string = readFileSync(tarPath).toString("base64")

    // Construct object expected by Cardiovascular.
    const body = new Object({
        access_token: user.token,
        name: md.name,
        tarball,
        version: md.version.toString(),
    })

    // Upload object to Cardiovascular
    request.post(cardiovascularUrl + "/pulses", { json: body },
        (err, res) => {
            if (err) {
                // Throw any request() errors.
                throw err
            } else {
                // All (v1.0.0) defined status codes accounted for, and
                // catchall.
                // TODO: Great use-case for standardized output, possibly to fit
                // in with enquirer style, eg: `> Output`
                switch (res.statusCode) {
                    case 200:
                        console.log(md.name, md.version.toString(),
                            "successfully pumped")
                        return

                    // Questionable output
                    case 400:
                        console.log("missing name, version, or tarball")
                        return

                    // Should be unreachable.
                    case 401:
                        console.log("invalid authentication token, try logging",
                            "in again")
                        return

                    case 409:
                        console.log("conflicting package versions")
                        return

                    case 500:
                        console.log("internal server error")
                        return

                    // Hopefully unreachable.
                    default:
                        console.log("unforseen error")
                        return
                }
            }
        })
}
