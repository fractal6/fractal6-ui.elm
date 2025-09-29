// see import issue: https://github.com/FullHuman/purgecss/issues/1295
const purgeCSSPlugin = require('@fullhuman/postcss-purgecss').default;
const varCompress = require('postcss-variable-compress')
const cssnano = require('cssnano')

const cssVarSafeList = [
    // All var used in the elm/js code (see var() and getPropertyValue()) !
    // --
    '--body-background-color',
    '--bulma-radius-large', '--text', '--text-weak', '--text-evidence', '--link', '--link2',
    //Circles
    '--gp-lvl-0-bg', '--gp-lvl-1-bg', '--gp-lvl-2-bg', '--gp-lvl-3-bg', '--gp-lvl-4-bg', '--gp-lvl-5-bg', '--gp-lvl-6-bg', '--gp-lvl-7-bg',
    // Roles
    '--owner', '--member', '--coordinator', '--peer', '--bot', '--guest', '--pending', '--retired',
];

module.exports = {
    parser: 'postcss-scss',

    // It saves 500Mb.
    // see Bulma theme issue:
    plugins: [
        require('autoprefixer'),
        purgeCSSPlugin({
            // file paths to your contents to remove unused styles.
            content: ['./public/**/*.html', './src/**/*.elm', './public/**/*.js', './assets/js/**/*.js' ],
            // other wise our aria-selected is removed (like with purgecss online)
            dynamicAttributes: ['aria-selected'],
            variables: true, // remove unused CSS variables
            safelist: {
                variables: cssVarSafeList,
            }
        }),

        // It saves around 100Mb
        varCompress(cssVarSafeList), // compress css variables

        // It just save around 10kb...
        //cssnano({
        //    preset: 'default',
        //}),
    ]
}


