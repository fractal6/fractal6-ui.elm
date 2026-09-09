// Jest config — uses babel-jest to translate the assets/js/ ESM modules
// into something the jsdom test VM can run. Keeps the source files untouched
// (they remain ESM for webpack/native consumers).
//
// (Native ESM via --experimental-vm-modules works for individual files but
// trips the "module is already linked" bug when two test files transitively
// import the same module — switching to babel-jest dodges that entirely.)
module.exports = {
    testEnvironment: 'jsdom',
    roots: ['<rootDir>/tests/Js'],
    transform: {
        '^.+\\.js$': 'babel-jest',
    },
    // d3 packages are ESM-only: let babel-jest transform them (needed by
    // graphpack_d3 tests that exercise the real module).
    transformIgnorePatterns: ['/node_modules/(?!d3-)'],
    setupFiles: ['<rootDir>/tests/Js/setup.js'],
    moduleFileExtensions: ['js'],
    testMatch: ['**/*.test.js'],
    // Stub heavy deps ports.js drags in but the markdown helpers don't touch
    // (d3 via graphpack_d3, minisearch, bulma_drivers). Avoids Jest
    // tripping over ESM-only packages in node_modules.
    moduleNameMapper: {
        '^minisearch$': '<rootDir>/tests/Js/__mocks__/empty.js',
        '^\\./graphpack_d3$': '<rootDir>/tests/Js/__mocks__/empty.js',
        '^\\./bulma_drivers$': '<rootDir>/tests/Js/__mocks__/empty.js',
    },
};
