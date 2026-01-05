const path = require('path');
const SpeedMeasurePlugin = require("speed-measure-webpack-plugin");
const smp = new SpeedMeasurePlugin();

module.exports = smp.wrap({
    mode: 'development',
    entry: './assets/sass/main.scss', // A file that just imports your CSS/SCSS
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'css-test.js',
    },
    module: {
        rules: [
            {
                test: /\.(sa|sc|c)ss$/,
                use: [
                    "style-loader",
                    //"css-loader",
                    {
                        loader: "css-loader",
                        "options": {
                            "sourceMap": false,
                        }
                    },
                    //"sass-loader",
                    {
                        loader: "sass-loader",
                        options: {
                            //implementation: require('sass'),
                            sassOptions: {
                                outputStyle: 'expanded',
                                sourceMap: false
                            }
                        }
                    }
                ],
            },
        ]
    }
});
