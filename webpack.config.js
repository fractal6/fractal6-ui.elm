'use strict';
const path = require('path');
const webpack = require('webpack');
const { merge } = require('webpack-merge');

const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin')

// Production CSS assets - separate, minimised file
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const safePostCssParser = require('postcss-safe-parser');

// deprecated
const autoprefixer = require('autoprefixer');

const commitHash = require('child_process')
  .execSync('git rev-parse --short HEAD')
  .toString()
  .trim();

// additional webpack settings for local env (when invoked by 'npm start')
module.exports = (env, argv) => {
    // determine build env
    var CMD = process.env.npm_lifecycle_event;
    var MODE = argv.mode;
    const isDev = MODE == 'development';
    const isProd = MODE == 'production';

    var API_URL;
    if (isDev || CMD == 'webprod') {
        API_URL = {
            auth: 'http://localhost:8888/auth',
            graphql: 'http://localhost:8888/api',
            rest: 'http://localhost:8888/q',
            doc: 'http://localhost:8888/data'
            // @debug: CORS error.
            // Would it be possible to get that data from the browser? CORS doesn seems to allow it.
            //doc: 'https://gitlab.com/fractal6/doc/-/raw/master/data'
        }
    }
    else if (isProd) {
        API_URL = {
            auth: 'https://api.fractale.co/auth',
            graphql: 'https://api.fractale.co/api',
            rest: 'https://api.fractale.co/q',
            doc: 'https://api.fractale.co/data'
        }
    }

    // entry and output path/filename variables
    const entryPath = path.join(__dirname, 'static/index.js');
    const outputPath = path.join(__dirname, 'dist');
    const outputFilename = isProd ? '[name]-[fullhash].js' : '[name].js'

    console.log(
        '\x1b[36m%s\x1b[0m',
        `Webpack run: Building for "${MODE}"\n`
    );

    // common webpack config (valid for dev and prod)
    var common = {
        stats: { colors: true }, // "error-only"
        mode: MODE,
        entry: entryPath,
        output: {
            path: outputPath,
            publicPath: '/',
            filename: `static/js/${outputFilename}`,
        },
        resolve: {
            extensions: ['.js', '.elm', '.scss'],
            modules: ['node_modules']
        },
        plugins: [
            new webpack.DefinePlugin({
                'AUTH_API': JSON.stringify(API_URL.auth),
                'GRAPHQL_API': JSON.stringify(API_URL.graphql),
                'REST_API': JSON.stringify(API_URL.rest),
                'DOC_API': JSON.stringify(API_URL.doc),
                'VERSION': JSON.stringify(commitHash)
            }),
            new webpack.LoaderOptionsPlugin({
                options: {
                    postcss: [autoprefixer()]
                }
            })
        ],
        module: {
            rules: [
                {
                    test: /\.js$/,
                    exclude: /node_modules/,
                    use: [
                        { loader: 'babel-loader' },
                        //options: {
                        //    presets: ['@babel/preset-env']
                        //}
                    ],
                },
                // fonts
                {
                    test: /\.svg(\?v=\d+\.\d+\.\d+)?$/,
                    type: 'asset/resource',
                },
                {
                    test: /\.(woff|woff2|eot|ttf|otf)$/i,
                    type: 'asset/resource',
                },
            ]
        }
    };

    // additional webpack settings for prod env (when invoked via --mode
    if (isDev) {
        return merge(common, {
            optimization: {moduleIds: 'named'},
            plugins: [
                // Generates an `index.html` file with the <script> injected.
                new HtmlWebpackPlugin({
                    template: 'static/index.html',
                    inject: 'body',
                    filename: 'index.html'
                }),
                // Prevents compilation errors causing the hot loader to lose state
                new webpack.NoEmitOnErrorsPlugin()
            ],
            module: {
                rules: [
                    {
                        test: /\.(sa|sc|c)ss$/,
                        exclude: [/elm-stuff/, /node_modules/],
                        use: [
                            { loader: "style-loader" },
                            { loader: "css-loader" },
                            { loader: "sass-loader" },
                        ],
                    },
                    {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/, /tests/],
                    use: [
                        { loader: 'elm-hot-webpack-loader' },
                        {
                            loader: 'elm-webpack-loader',
                            options: {
                                // add Elm's debug overlay to output
                                debug: true,
                            }
                        }
                    ]
                }]
            },
            devServer: {
                // serve index.html in place of 404 responses
                hot: true,
                historyApiFallback: true,
                //contentBase: './static',
                //proxy: [],
                // feel free to delete this section if you don't need anything like this
                //before(app) {
                //    // on port 3000
                //    app.get("/test", function(req, res) {
                //        res.json({ result: "OK" });
                //    });
                //}
            },
        })
    } else if (isProd) {
        return module.exports = merge(common, {
            plugins: [
                // Generates an `index.html` file with the <script> injected.
                new HtmlWebpackPlugin({
                    template: 'static/index.html',
                    inject: 'body',
                    filename: 'index.html',
                    minify: {
                        removeComments: true,
                        collapseWhitespace: true,
                        removeRedundantAttributes: true,
                        useShortDoctype: true,
                        removeEmptyAttributes: true,
                        removeStyleLinkTypeAttributes: true,
                        keepClosingSlash: true,
                        minifyJS: true,
                        minifyCSS: true,
                        minifyURLs: true,
                    },
                }),
                // Delete everything from output-path (/dist) and report to user
                new CleanWebpackPlugin({
                    root: __dirname,
                    exclude: [],
                    verbose: true,
                    dry: false
                }),
                // Copy public images to the build folder
                new CopyPlugin({
                    patterns: [{
                        from: 'assets/images',
                        to: 'static/images/' ,
                        globOptions: { ignore: ['*.swp'] }
                    }],
                }),
                // Note: this won't work without ExtractTextPlugin.extract(..) with rule loader.
                new MiniCssExtractPlugin({
                    filename: 'static/css/[name].[fullhash].css',
                    //chunkFilename: 'static/css/[name].[contenthash:8].chunk.css',
                }),
            ],
            module: {
                rules: [
                    {
                        test: /\.elm$/,
                        exclude: [/elm-stuff/, /node_modules/, /tests/],
                        use: {
                            loader: 'elm-webpack-loader',
                            options: { optimize: true }
                        }
                    },
                    {
                        test: /\.(sa|sc|c)ss$/,
                        exclude: [/elm-stuff/, /node_modules/],
                        use: [
                            MiniCssExtractPlugin.loader,
                            "css-loader",
                            "sass-loader",
                            {
                                loader: "postcss-loader",
                                options: {
                                    postcssOptions: {
                                        parser: "postcss-scss", // allow inline comment (//)
                                    },
                                },
                            },
                        ],
                    },
                ]

            },
            optimization: {
                minimizer: [
                    // extract CSS into a separate file
                    // minify & mangle JS/CSS
                    new TerserPlugin({
                        parallel: true,
                        terserOptions: {
                            format: {
                                // Turned on because emoji and regex is not minified properly using default
                                // https://github.com/facebook/create-react-app/issues/2488
                                ascii_only: true,
                            },
                            compress: {
                                passes: 3,
                                // Disabled because of an issue with Uglify breaking seemingly valid code:
                                // https://github.com/facebook/create-react-app/issues/2376
                                // Pending further investigation:
                                // https://github.com/mishoo/UglifyJS2/issues/2011
                                comparisons: false,
                                pure_getters: true,
                                keep_fargs: false,
                                unsafe: true,
                                unsafe_comps: false, // break graphpack
                                unsafe_math: true,
                                pure_funcs: [ 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9'],
                                //keep_fnames: true,
                            },
                        },
                    }),

                    new CssMinimizerPlugin({
                        // @debug: Causes errors.
                        //minimizerOptions: {
                        //    processorOptions: {
                        //        parser: safePostCssParser,
                        //    },
                        //},
                    }),
                ]
            }
        })
    }
};
