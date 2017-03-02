
// Object.create first argument is the prototype you want to inherit from

var person1 = {
    name: "Nicholas",
    sayName: function() {
        console.log(this.name);
    }
};

person1.sayName();

var person2 = Object.create(person1, {
    name: { // this own property name will overshadow prototype person1's name property
        configurable: true,
        enumerable: true,
        value: "Greg",
        writable: true
    }
});

person2.sayName(); // sayName is being used from prototype object person1 but name is person2's own name

console.log(person1.hasOwnProperty("name"));
console.log(person2.hasOwnProperty("name"));
console.log(person1.hasOwnProperty("sayName"));
console.log(person2.hasOwnProperty("sayName"));

var nakedObject = Object.create(null); // Such property less objects are useful

console.log(' nakedObject = ', nakedObject);
// console.log(' nakeObject.hasOwnProperty(toString)', nakedObject.hasOwnProperty("toString"));
// can't do this because there is no hasOwnProperty on nakedObject
console.log(' "toString" in nakedObject : ', "toString" in nakedObject );

function Car(){ //The birth of the Car.prototype and its constructor property happens behind the scenes by engine

}

/*
 Engine does this for you behind the scenes:
 Car.prototype = Object.create(Object.prototype, {
   constructor: { configurable: true, enumerable: true, value: Car, writable: true}
 });
 */

console.log(' Car.prototype.constructor = ', Car.prototype.constructor);
console.log(' Object.getPrototypeOf(Car.prototype) = ', "toString" in Object.getPrototypeOf(Car.prototype));
console.log(' Car.prototype.prototype = ', Car.prototype.prototype);



