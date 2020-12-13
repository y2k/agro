var webpack = require("webpack");
var path = require("path");

var config = {
    mode: "development",
    entry: "./src/App.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8080,
        proxy: [{
            path: '/api',
            target: 'http://localhost:8090'
        }],
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
};

module.exports = (env, argv) => {
    if (argv.mode === 'production') {
    } else {
        config.devtool = 'source-map';
        config.plugins = [
            new webpack.HotModuleReplacementPlugin(),
        ]
    }
    return config;
};
