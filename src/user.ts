import * as base64 from "base-64"
import { existsSync, readFileSync, writeFileSync } from "fs"
import { homedir } from "os"
import { join } from "path"
import * as request from "request"

import Enquirer = require("enquirer");
const enquirer = new Enquirer()
// tslint:disable-next-line:no-var-requires
enquirer.register("password", require("prompt-password"))
// tslint:disable-next-line:no-var-requires
enquirer.register("confirm", require("prompt-confirm"))

const jwtPath = join(homedir(), ".bpmjwt")
const cardiovascularUrl = "http://localhost:2387"

export const token: string = readFileSync(jwtPath).toString()

export function isLoggedIn(): boolean {
    if (existsSync(jwtPath)) {
        // Splits JWT token into its parts:
        //      0: header
        //      1: payload
        //      2: signature
        // and takes the payload.
        const payloadB64: string = token.split(".")[1]

        // Decodes payload and parses it into JSON
        const payload = JSON.parse(base64.decode(payloadB64))

        // now is defined to be compatible with server side implementation.
        // server: Math.floor(Date.now() / 1000) + (60 * 60) // as of v1.0.0
        const now = Math.floor(Date.now() / 1000)
        if (payload.exp && payload.exp > now) {
            return true
        }
    }
    return false;
}

// TODO: Maybe async/await will clean this up.
// Prime example of ugly OOP ↓
export function logIn(): boolean {
    // Input prompts, self-explanatory.
    const questions = [{
        message: "Email:",
        name: "email",
        type: "input",
    },
    {
        message: "Password:",
        name: "password",
        type: "password",
    }]

    // Asks the user using above array of prompts.
    enquirer.prompt(questions)
        .then((answers: any) => {
            // Email and password should always be defined, but just in case...
            if (answers.email && answers.password) {
                // TODO: Manage different authorities, or store the URI
                // somewhere else.
                // Make a request to the cardiovascular API for an authorization
                // token, with the user-provided email and password as the body.
                request.post(cardiovascularUrl + "/auth", {
                    json: answers,
                }, (err, res, body) => {
                    if (err) {
                        // Throw any request() errors.
                        throw err
                    } else {
                        // All (v1.0.0) defined status codes accounted for, and
                        // catchall.
                        // TODO: Great use-case for standardized output,
                        // possibly to fit in with enquirer style, eg:
                        // `> Output`
                        switch (res.statusCode) {
                            case 200:
                                // Save validation token into jwtPath,
                                // overwriting any previous contents, because
                                // what good is an expired token?
                                writeFileSync(jwtPath, body, { flag: "w+" })
                                return true

                            case 422:
                                // Checks for specific error response from API.
                                // Is it worth putting this case into a seperate
                                // function, what with the context and all?
                                if (body[0] === "user not found" ) {
                                    // Input prompt, self-explanatory.
                                    const newAccPrompt = {
                                        message:
                                            "Create new account with email: "
                                            + answers.email
                                            + "?",
                                        name: "newAcc",
                                        type: "confirm",
                                    }

                                    // Prompt user to create a new account.
                                    enquirer.prompt(newAccPrompt)
                                        .then((answer: any) => {
                                            if (answer.newAcc === true) {
                                                newUser(answers.email,
                                                    answers.password)
                                            }
                                        })
                                }

                            default:
                                return false
                        }
                    }
                })
            }
        })

    // Should be unreachable.
    return false
}

export function newUser(email?: string, password?: string): boolean {
    // Input prompts, self-explanatory.
    const questions = [{
        message: "Email:",
        name: "email",
        type: "input",
    },
    {
        message: "Password:",
        name: "password",
        type: "password",
    }]

    // If optional parameters aren't supplied, get them from the user.
    if (!email && !password) {
        enquirer.prompt(questions)
            .then((answers: any) => {
                if (answers.email && answers.password) {
                    email = answers.email
                    password = answers.password
                }
            })
    }

    // Request to create a new user with the credential specified by the user.
    request.post(cardiovascularUrl + "/users", {
        json: { email, password },
    }, (err, res) => {
        if (err) {
            // Throw any request() errors.
            throw err
        } else {
            // All (v1.0.0) defined status codes accounted for, and catchall.
            // TODO: Great use-case for standardized output, possibly to fit in
            // with enquirer style, eg: `> Output`
            switch (res.statusCode) {
                case 201:
                    console.log("user created")
                    return true

                case 403:
                    console.error("email already used or password too weak")
                    return false

                case 422:
                    console.error("email or password missing")
                    return false

                default:
                    console.error("unknown error occured")
                    return false
            }
        }
    })

    // Should be unreachable.
    return false
}
