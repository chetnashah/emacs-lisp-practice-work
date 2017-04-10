// after reading mdn page on inheritance
// make sure see this https://www.youtube.com/watch?time_continue=1&v=PMfcsYzj-9M



function Person(first, last, age, gender, interests) {
    this.name = {
        first,
        last
    };
    this.age = age;
    this.gender = gender;
    this.interests = interests;
}

Person.prototype.greeting = function() {
    console.log('Hi im ' + this.name.first + '.');
}

//Teacher will inherit from person.
// must have all members from person
// an extra property subject, which the teacher teaches.
// an updated greeting method which sounds more formal


function Teacher(first, last, age, gender, interests, subject) {
    // like java's super, but here we deal with 'this' which we got
    // one when Teacher was new'ed
    Person.call(this, first, last, age, gender, interests);
    // call's first parameter is the this that you want to use
    this.subject = subject;
}

// We are not done yet,
// the prototypes are not linked, we cannot do greeting etc. with a new Teacher() inst.

Teacher.prototype = Object.create(Person.prototype);// link prototype
Teacher.prototype.constructor = Teacher;// point to right constructor

/*
 function Constructor() {}
 o = new Constructor();
 // is equivalent to:
 o = Object.create(Constructor.prototype);
 */

// give Teacher a overriding greeting method
Teacher.prototype.greeting = function() {
    var prefix;

    if (this.gender === 'male' || this.gender === 'Male' || this.gender === 'm' || this.gender === 'M') {
        prefix = 'Mr.';
    } else if (this.gender === 'female' || this.gender === 'Female' || this.gender === 'f' || this.gender === 'F') {
        prefix = 'Mrs.';
    } else {
        prefix = 'Mx.';
    }

    console.log('Hello. My name is ' + prefix + ' ' + this.name.last + ', and I teach ' + this.subject + '.');
}




