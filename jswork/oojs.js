

/*
 * This is a JavaScript Scratchpad.
 *
 * Enter some JavaScript, then Right Click or choose from the Execute Menu:
 * 1. Run to evaluate the selected text (Ctrl+R),
 * 2. Inspect to bring up an Object Inspector on the result (Ctrl+I), or,
 * 3. Display to insert the result in a comment after the selection. (Ctrl+L)
 */



var bb = {title: "hi"};

console.log(bb.hasOwnProperty('title'));
console.log(bb.hasOwnProperty('hasOwnProperty'));
console.log(Object.prototype.hasOwnProperty('hasOwnProperty'));

let obj = {};

console.log(obj.prototype); // this returns undefined
console.log(Object.getPrototypeOf(obj)); // but this works fine ?


// class blueprint is a function with capital name
function Person(name){
    this.name = name;
}

var p1 = new Person("jay");

console.log(p1); // { name: jay}
console.log('Object.getOwnPropertyNames(p1) = ', Object.getOwnPropertyNames(p1)); // [name]
console.log(p1.prototype); //undefined
console.log(Person.prototype);//__proto__ and constructor present
console.log(Object.getPrototypeOf(p1)); //__proto__ and constructor present
console.log(p1.hasOwnProperty('constructor')); // false, it is in Person.prototype
console.log(Object.getPrototypeOf(p1).hasOwnProperty('constructor'));// true

console.log(p1.constructor); // function Person, goes to prototype retn by getProtoof.

//putting common functions between instances inside prototype
Person.prototype.sayName = function(){
    console.log(' name is : '+ this.name);
}

p1.sayName();

console.log(Object.keys(p1)); // [name]
console.log(Object.getOwnPropertyNames(p1)); //[name]

console.log(Object.keys(Object.getPrototypeOf(p1))); // [sayName]
console.log(Object.getOwnPropertyNames(Object.getPrototypeOf(p1))); // [constructor, sayName]

console.log(Object.keys(Person.prototype));// [sayName]
console.log(Object.getOwnPropertyNames(Person.prototype)); // [constructor, sayName]

console.log(Object.keys(p1.prototype));//Error because p1.prototype is undefined
//One cannot directly access prototype from instance, use Object.getPrototypeOf instead

let arr = [1,2];
arr = [1,2,3];
console.log('Object.getPrototypeOf(arr) = ', Object.getPrototypeOf(arr));// Array
console.log('arr.prototype = ', arr.prototype); //undefined
console.log('Object.keys(Object.getPrototypeOf(arr))', Object.keys(Object.getPrototypeOf(arr))); // []
console.log('Object.getOwnPropertyNames(Object.getPrototypeOf(arr)) = ', Object.getOwnPropertyNames(Object.getPrototypeOf(arr)));
// [length, join, reverse, push, pop, slice, splice .. total 21]
console.log('Object.getOwnPropertyNames(Array.prototype) = ', Object.getOwnPropertyNames(Array.prototype));
// [length, join, reverse, push, pop, slice, splice .. total 21]

let fn = function(a, b){
    return a + b;
}

console.log('Object.getPrototypeOf(fn) = ', Object.getPrototypeOf(fn));// function ()
console.log('fn.prototype = ', fn.prototype);//{constructor, __proto__}

console.log('Object.keys(fn) = ', Object.keys(fn));// []
console.log('Object.keys(fn.prototype) = ', Object.keys(fn.prototype));// []
console.log('Object.keys(Object.getPrototypeOf(fn)) = ', Object.keys(Object.getPrototypeOf(fn)));// []
console.log('Object.keys(Object.getPrototypeOf(Object.getPrototypeOf(fn))) = ', Object.keys(Object.getPrototypeOf(Object.getPrototypeOf(fn))));// []
console.log('Object.keys(Function.prototype)', Object.keys(Function.prototype));//[]
console.log('Object.keys(Function) = ', Object.keys(Function));

console.log('Object.getOwnPropertyNames(fn) = ', Object.getOwnPropertyNames(fn));// [prototype,length,name]
console.log('Object.getOwnPropertyNames(fn.prototype) = ', Object.getOwnPropertyNames(fn.prototype));// [constructor]
console.log('Object.getOwnPropertyNames(Object.getPrototypeOf(fn)) = ', Object.getOwnPropertyNames(Object.getPrototypeOf(fn)));// [bind, apply, call, .. 11 total]
console.log('Object.getOwnPropertyNames(Object.getPrototypeOf(Object.getPrototypeOf(fn))) = ', Object.getOwnPropertyNames(Object.getPrototypeOf(Object.getPrototypeOf(fn))));// [__definGetter, valueOf, toLocaleString .. 15 total]
console.log('Object.getOwnPropertyNames(Function.prototype)', Object.getOwnPropertyNames(Function.prototype));//[bind, apply, call .. 11 total]
console.log('Object.getOwnPropertyNames(Function) = ', Object.getOwnPropertyNames(Function));//[property, length, name]

console.log('fn.prototype.prototype = ', fn.prototype.prototype);// undefined
console.log('Object.getPrototypeOf(Object.getPrototypeOf(fn)) = ',Object.getPrototypeOf(Object.getPrototypeOf(fn)));


