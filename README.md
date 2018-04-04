# interpreter-scheme

An interpreter for a Java/C style language written in scheme.
To run a program file, call (interpret "file_name")


Sample Program:
```
var x = 14;
var y = 3 * x - 7;
function gcd(a,b) {
  if (a < b) {
    var temp = a;
    a = b;
    b = temp;
  }
  var r = a % b;
  while (r != 0) {
    a = b;
    b = r;
    r = a % b;
  }
  return b;
}
function main () {
  return gcd(x,y);
}
```
