




[1,2,3].map(x => x*x);
// object destructuring assignment

let node = {
    type: "Identifier",
    name: "foo"
};

let { type, name } = node;
type;
name;
// array destructuring with default values
let colors = ["red"];
let [firstColor, secondColor="green"] = colors;

console.log(firstColor);
console.log(secondColor);

let node2 = {
    type: "Identifier",
    name: "foo"
},
    type2 = "Literal",
    name2 = 5;

function outputInfo(value) {
    console.log(value === node2);
}

outputInfo({ type2, name2 } = node2);
console.log(type2);
console.log(name2);

// es6 adds name property to every function

function doSomething() {
    
}

function doAnotherThing() {
    
}

console.log(doSomething.name);
console.log(doAnotherThing.name);

// functions created using bind have bound in their name
console.log(doSomething.bind().name);
console.log((new Function()).name);
console.log((()=>{}).name);

// clarifying dual purpose of functions in ES6
function Animal(name) {
    this.name = name;
}
// the above function can be called with or without new
var an1 = new Animal("Dog");// this inside is a new object which is returned
var notAAnimal = Animal("Me"); // sets a name property in global obj in non-str

console.log(an1);
console.log(notAAnimal);

// js has two different internal only methods for functions : [[Call]]
// and [[Construct]].
// When function is called without new, the [[Call]] method is executed
// When function is called with new, the [[Construct]] method is responsible
// for creating new object, called the new target, and executing the
// function body with this set to new target. Functions that have [[Construct]]
// method are called constructors

// Not all functions have [[Construct]] e.g. arrow functions

/* Determining if a function was called with new */
/* ES5 way */
function Guy(name) {
    if (this instanceof Guy) {
        this.name = name;
    } else {
        throw new Error(" You must new with guy");
    }
}
// but above function has subtle bug
// e.g. if i call it with explicit this
var kakashi = new Guy("kakashi");
var notAGuy = Guy.call(kakashi, "Naruto");

/* So new Es6 way */
/* Es6 introduces new.target metaproperty */
// When a functions [[Construct]] method is called, new.target
// is filled with target of the new operator which is typically constructor
// if [[Call]] is executed, then new.target is undefined always
function BetterGuy(name) {
    if(typeof new.target !== "undefined") {
        this.name = name;
    } else {
        throw new Error(" you must new with betterguy");
    }
}

var g2 = new BetterGuy("Nicholas");
var g3 = BetterGuy.call(g2,"Michael");// this will show error correctly

/* Arrow functions */
/* They behave differently from normal javascript functions in may ways:
   1. No this, super, arguments and new.target bindings
   2. Cannot be called with new, coz they miss [[Construct]]
   3. No prototype (since you can't use new on arrow function)
   4. Can't change this (set to lexical this by default)
   5. No arguments object (instead rely on named and rest parameters)
   6. No duplicate named parameters
 */

// wrap the arrow function in parentheses to create IIFEs
let p1 = ((name) => {
    return {
        getName: function() {
            return name;
        }
    };
})("Nicholas");

p1.getName();


// TODO tCO in ES6

// consise method syntax

// Object.assign() for mixins




