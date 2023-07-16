module.exports = {
  "extends": "../../.eslintrc.cjs",
  "env": {
    "browser": true
  },
  "plugins": [
    "react"
  ],
  "rules": {
    "react/prop-types": "off",
    "react/no-unescaped-entities": "off"
  },
  "settings": {
    "react": {
      "version": "detect"
    }
  }
}