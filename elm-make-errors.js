// noinspection JSUnusedLocalSymbols
const util = require('util')
const {spawnSync} = require('child_process');


let spawnSyncReturns = spawnSync("elm.cmd", [
    "make",
    "src/Main.elm",
    "--output=src/main.elm.js",
    "--report=json",
]);

async function main() {
    const stdErrString = spawnSyncReturns.stderr.toString();
    if(stdErrString.length === 0) return
    const json = JSON.parse(stdErrString);

    const output = json
        .errors
        .map(function (e) {

            const filePath = e.path;
            const problems = e["problems"];
            const firstProblem = problems[0]
            // console.log(util.inspect(firstProblem, {depth: 3}))
            const start = firstProblem.region.start;
            const filePathWithLineAndColumn = `${filePath}:${(start.line)}:${(start.column)}`;
            return `Error: ${firstProblem.title} ${filePathWithLineAndColumn} ${firstProblem.title}`;
        })
        .join('\n')

    console.error(output);
    process.exit(1)
}

main().catch(console.error)