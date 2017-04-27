
// definitive reference at:
//https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions



// ways to instantiage a Regexp object
//1. /pattern/flags
var re = /ab+c/g;
console.log(re);

//2. new RegExp('pattern', 'flags')
var re2 = new RegExp('ab+c','g');

// RegExp methods: test and exec
// String methods: match, replace, search and split

// when g flag is set re has a member named lastIndex
// which is the index at which to start next match

function kk(){
    console.log('hi');
}
