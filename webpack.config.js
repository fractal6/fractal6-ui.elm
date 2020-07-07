const path = require('path');
const webpack = require('webpack');
const { merge } = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const autoprefixer = require('autoprefixer');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');


const prod = 'production';
const dev = 'development';

// determine build env
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;
const isDev = TARGET_ENV == dev;
const isProd = TARGET_ENV == prod;

// entry and output path/filename variables
const entryPath = path.join(__dirname, 'static/index.js');
const outputPath = path.join(__dirname, 'dist');
const outputFilename = isProd ? '[name]-[hash].js' : '[name].js'

console.log('WEBPACK GO! Building for ' + TARGET_ENV);

// common webpack config (valid for dev and prod)
var commonConfig = {
    output: {
        path: outputPath,
        filename: `static/js/${outputFilename}`,
    },
    resolve: {
        extensions: ['.js', '.elm'],
        modules: ['node_modules']
    },
    module: {
        noParse: /\.elm$/,
        rules: [{
            test: /\.(eot|ttf|woff|woff2|svg)$/,
            use: 'file-loader?publicPath=../../&name=static/css/[hash].[ext]'
        }]
    },
    plugins: [
        new webpack.LoaderOptionsPlugin({
            options: {
                postcss: [autoprefixer()]
            }
        }),
        new HtmlWebpackPlugin({
            template: 'static/index.html',
            inject: 'body',
            filename: 'index.html'
        })
    ]
}

// additional webpack settings for local env (when invoked by 'npm start')
if (isDev === true) {
    module.exports = merge(commonConfig, {
        entry: entryPath,
       //[
       //    'webpack-dev-server/client?http://localhost:8080',
       //    entryPath
       //],
        devServer: {
            // serve index.html in place of 404 responses
            historyApiFallback: true,
            stats: { colors: true },
            hot: true
            //contentBase: './src'
        },
        module: {
            rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/, /tests/],
                use: [{
                    loader: 'elm-webpack-loader',
                    options: {
                        verbose: true,
                        debug: true
                    }
                }]
            },{
                test: /\.sc?ss$/,
                use: ['style-loader', 'css-loader', 'postcss-loader', 'sass-loader']
                    //{ loader: 'postcss-loader', options: { path: "some/other/path" },
            }]
        }
    });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (isProd === true) {
    module.exports = merge(commonConfig, {
        entry: entryPath,
        module: {
            rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/, /tests/],
                use: 'elm-webpack-loader'
            }, {
                test: /\.sc?ss$/,
                use: ExtractTextPlugin.extract({
                    fallback: 'style-loader',
                    use: ['css-loader', 'postcss-loader', 'sass-loader']
                })
            }]
        },
        plugins: [
            new ExtractTextPlugin({
                filename: 'static/css/[name]-[hash].css',
                allChunks: true,
            }),
            new CopyWebpackPlugin([{
                from: 'assets/images',
                to: 'static/img/'
            }, {
                from: 'src/favicon.ico'
            }],
                { ignore: ['*.swp'] }
            ),

            // extract CSS into a separate file
            // minify & mangle JS/CSS
            new webpack.optimize.UglifyJsPlugin({
                minimize: true,
                //compressor: {
                //    warnings: false
                //},
				output: {
					comments: false,
					// Turned on because emoji and regex is not minified properly using default
					// https://github.com/facebook/create-react-app/issues/2488
					ascii_only: true,
				},
				compress: {
					passes: 2,
					warnings: false,
					// Disabled because of an issue with Uglify breaking seemingly valid code:
					// https://github.com/facebook/create-react-app/issues/2376
					// Pending further investigation:
					// https://github.com/mishoo/UglifyJS2/issues/2011
					comparisons: false,
					pure_getters: true,
					keep_fargs: false,
					unsafe_comps: true,
					unsafe: true,
					pure_funcs: [
						'A2',
						'A3',
						'A4',
						'A5',
						'A6',
						'A7',
						'A8',
						'A9',
						'F2',
						'F3',
						'F4',
						'F5',
						'F6',
						'F7',
						'F8',
						'F9',
					],
				},
				//mangle: true,
				mangle: { safari10: true, },
			})
		]
	});
}
