const util = require('util')
const {spawnSync} = require('child_process');


let spawnSyncReturns = spawnSync("elm.cmd", [
    "make",
    "src/Main.elm",
    "--output=/dev/null",
    "--report=json",
]);

async function main() {
    const elmErrorJson = JSON.parse(spawnSyncReturns.stderr.toString());
    const output = elmErrorJson
        .errors
        .map(e => 'Error: ' + e.problems[0].title + ': ' + e.path)
        .join('\\n')


    console.log(output);
}

main().catch(console.error)