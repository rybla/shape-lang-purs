/*
For some reason, this line is problematic in `app.js`
```
  var req = typeof module === "undefined" ? undefined : module.require;
```
because `module` is not defined, so it causes an error to do `module.require`
even in the `:` of a conditional expression. So, this script just replaces that line with
```
  var req = undefined;
```
which seems to make everything work.
*/

const replace = require('replace-in-file')
const options = {
  files: './app/app.js',
  from: 'var req = typeof module === "undefined" ? undefined : module.require;',
  to: 'var req = undefined;'
}
console.log("Trying to replace problematic line `./app/app.js`")
try {
  const results = replace.sync(options)
  console.log('Replacement results:', results)
} 
catch (error) {
  console.error('Error occured:', error)
}