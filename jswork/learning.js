
function Fruit(theColor, theSweetness, name){
    this.color = theColor;
    this.sweetness = theSweetness;
    this.fruitName = name;
}

Fruit.prototype.printInfo = function (){
    console.log(' color of fruit is ', this.color);
}

var ff = new Fruit("Orange", 2, "Orange");
var kk = new Fruit("Yellow", 1, "Pineapple");

console.log('ff = ', ff);
console.log('kk = ', kk);

console.log('ff.color = ', ff.color);
console.log('kk.fruitName = ', kk.fruitName);

function Rabbit() {
}

console.log('Rabbit.prototype = ', Rabbit.prototype.constructor)
kk.printInfo();
