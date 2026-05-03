// Used only by babel-jest (CommonJS context). Targets current Node so the
// only transform applied is ESM → CJS for the Jest VM.
module.exports = {
    presets: [
        ['@babel/preset-env', { targets: { node: 'current' } }],
    ],
};
