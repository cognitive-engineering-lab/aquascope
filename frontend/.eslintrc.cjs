module.exports = {
  "env": {
    "es2021": true
  },
  "extends": [
    "eslint:recommended"
  ],
  "parser": "@typescript-eslint/parser",
  "parserOptions": {
    "ecmaVersion": 13,
    "sourceType": "module"
  },
  "plugins": [
    "@typescript-eslint",
    "prettier",
    "react"
  ],
  "ignorePatterns": [
    "*.d.ts"
  ],
  "rules": {
    "no-empty-pattern": "off",
    "no-undef": "off",
    "no-unused-vars": "off",
    "no-cond-assign": "off",
    "@typescript-eslint/no-unused-vars": [
      "error",
      {
        "argsIgnorePattern": "^_",
        "varsIgnorePattern": "^_"
      }
    ],
    "no-constant-condition": [
      "error",
      {
        "checkLoops": false
      }
    ],
    "prettier/prettier": "error",
    "react/prop-types": "off",
    "react/no-unescaped-entities": "off"
  },
  "settings": {
    "react": {
      "version": "detect"
    }
  }
}