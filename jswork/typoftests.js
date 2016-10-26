/*
 * This is a JavaScript Scratchpad.
 *
 * Enter some JavaScript, then Right Click or choose from the Execute Menu:
 * 1. Run to evaluate the selected text (Ctrl+R),
 * 2. Inspect to bring up an Object Inspector on the result (Ctrl+I), or,
 * 3. Display to insert the result in a comment after the selection. (Ctrl+L)
 */
console.log(' typeof undefined ' + typeof undefined);
console.log(' typeof null ' +  typeof null);
console.log(' typeof true ' + typeof true);

console.log(' typeof "jayshah" ' + typeof "jayshah");
console.log(' typeof 2.3 ' + typeof 2.3);
console.log(' typeof {} ' + typeof {});
console.log(' typeof [] ' + typeof []);
console.log(' typeof function(){} ' + typeof function(){});

var kk = {};
var jj = function() {
    console.log('ji');
}
var tt = [];
var uu = null;
var oo; // is undefined by default

console.log(' typeof kk = ' + typeof kk);
console.log(' type of jj = ' + typeof jj);
console.log(' type of tt = ' + typeof tt);
console.log(' type of uu = ' + typeof uu);
console.log(' type of oo = '+ typeof oo);




function Person(){
    console.log('person ');
}

var p1 = new Person();

console.log(p1 instanceof Person);
// true
console.log(p1.constructor === Person);
// true

var ll = {};

console.log(ll.constructor === Object);
// true

function Tree(name){
    this.name = name;
}

var theTree = new Tree('Redwood');
console.log('theTree.constructor is '+ theTree.constructor);
