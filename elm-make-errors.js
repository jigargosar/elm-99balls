// noinspection JSUnusedLocalSymbols
const util = require('util')
const {spawnSync} = require('child_process');


let spawnSyncReturns = spawnSync("elm.cmd", [
    "make",
    "src/Main.elm",
    "--output=/dev/null",
    "--report=json",
]);

async function main() {
    const stdErrString = spawnSyncReturns.stderr.toString();
    const json = JSON.parse(stdErrString);

    const output = json
        .errors
        .map(function (e) {

            const filePath = e.path;
            const problems = e["problems"];
            const firstProblem = problems[0]
            console.log(util.inspect(firstProblem, {depth: 3}))
            const filePathWithLineAndColumn = `${filePath}:${(firstProblem.region.start.line)}:${(firstProblem.region.start.column)}`;
            return `Error: ${firstProblem.title} ${filePathWithLineAndColumn} ${firstProblem.title}`;
        })
        .join('\n')


    console.log(output);


}

main().catch(console.error)