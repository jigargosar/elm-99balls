const HtmlWebpackPlugin = require("html-webpack-plugin");
const {CleanWebpackPlugin} = require("clean-webpack-plugin");
// const path = require("path")

module.exports = {
    mode: "development",
    entry: './src/index.js',
    devtool: 'inline-source-map',
    devServer:  {

        contentBase: 'public',
        overlay:true

    },
    module: {
        rules: [
            {
                test: /\.css$/i,
                use: ['style-loader', 'css-loader'],
            },
            {
                test: /\.elm$/i,
                use: [{loader: 'elm-webpack-loader', options: {debug: false}}],
            },
        ]
    },
    plugins: [
        new CleanWebpackPlugin(),
        new HtmlWebpackPlugin({
            template: "public/index.html"
        }),
    ],
}