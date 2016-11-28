
function testfn(){
    // even thoug function defn/declaration does not have params,
    // arguments object is perfectly filled by parameters given from callee
    console.log('arguments obj = ', arguments);
}


testfn();
 
//
testfn(1,2,3);
