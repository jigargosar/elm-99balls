const HtmlWebpackPlugin = require("html-webpack-plugin");
const path = require("path");
// noinspection JSUnusedLocalSymbols
const cp = require("child_process")
// noinspection JSUnusedLocalSymbols
const fs = require("fs")

module.exports = {
    mode: "development",
    entry: './src/index.js',
    output: {
        filename: '[name].bundle.js',
        path: path.resolve(__dirname, 'dist'),
        clean: true,
        pathinfo: false,
    },
    optimization: {
        removeAvailableModules: false,
        removeEmptyChunks: false,
        splitChunks: false,
    },
    devtool: false,
    devServer: {
        contentBase: 'public',
        overlay: true,
        hot: false,
        liveReload: true,
        watchContentBase: true,
        // before: function (app, server, compiler) {
        //     app.get('/bundle.elm.js', function (req, res) {
        //         cp.execSync("elm make src/Main.elm --output=tmp/bundle.elm.js")
        //         res.setHeader('content-type', 'text/javascript');
        //         res.write(fs.readFileSync('tmp/bundle.elm.js'));
        //         res.end();
        //     });
        // },
    },
    externals: {
        // './Main.elm': '{Elm}',
    },
    module: {
        rules: [
            {
                test: /\.css$/i,
                use: ['style-loader', 'css-loader'],
            },
            {
                test: /\.elm$/i,
                include: path.resolve(__dirname, 'src'),
                use: [{loader: 'elm-webpack-loader', options: {debug: false}}],
            },
        ]
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: "public/index.html"
        }),
    ],
}