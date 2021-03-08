const cp = require("child_process");
const fs = require("fs");
const nodemon = require("nodemon");
// noinspection JSUnusedLocalSymbols
const path = require("path");

// process.chdir(path.resolve(__dirname, ".."))
console.log("starting");
nodemon({
    exec: "echo a",
    ext: "elm",
    watch: ["src"],
    verbose: true,
})
nodemon.on('start', bundle)
nodemon.on('restart', bundle)

function bundle() {
    cp.execSync("elm make src/Main.elm --output=tmp/bundle.elm.js")
    fs.copyFileSync('tmp/bundle.elm.js', 'public/bundle.elm.js')
    // fs.writeFileSync('public/bundle.elm.js',fs.readFileSync('tmp/bundle.elm.js'))
}