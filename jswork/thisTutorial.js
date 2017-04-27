// Use case 1
// usually when functions are called, the this is determined as
// object to left of (.) at call site
var somebody = {
    name: 'anonymous',
    say: function(){
        console.log(' saying '+ this.name);
    }
}

somebody.say();

// Using Fn.call with this.
function identify() {
    return this.name.toUpperCase();
}

function speak() {
    var greeting = "Hello I'm "+ identify.call(this);
    console.log(greeting);
}

var me = {
    name: 'Chetna'
};

var you = {
    name: 'Reader'
};

// call is used to customize/explicitly specify
// 'this' used in a function
// identify(me) will because the this in that case is window
console.log(identify.call(me));
speak.call(me);
speak.call(you);

// Use case 2
// Another use case for Fn.call(this, args) is to reuse
// constructor functions for populating this in other
// constructor functions
function Product(name, price) {
    this.name = name;
    this.price = price;
}

// A food is a product, so product costructor function
// will populate respective properties.
function Food(category, name, price) {
    // a convention to call other constructor functions
    // first with this, then populate our properties
    Product.call(this, name, price);// reusing property population logic of other constructor Function
    this.category = category;
}

// Similarly, A toy is a product
function Toy(color, name, price) {
    Product.call(this, name, price);
    this.color = color;
}

var genericProduct = new Product('testproduct', 11);
var teletubby = new Toy('red', 'chetna', 100);

console.log(genericProduct);
console.log(teletubby);



// Use case 3
// specifying context with this
function devgreet() {
    var reply = [this.person, 'Is an Awesome', this.role].join(' ');
    console.log(reply);
}

var i = { person: 'Doug CrockFord', role: 'JS developer'};

devgreet.call(i);



