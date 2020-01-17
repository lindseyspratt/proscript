"use strict";
// https://stackoverflow.com/questions/11168939/function-calls-are-expensive-vs-keep-functions-small

let a = function(val) { return val+1; };

let b = function(val) { return val-1; };

let c = function(val) { return val*2 };

let tag = function(word) {return (word >>> 27) & 7;};
let val = function(word) {return word & ((1 << 27)-1)};

let reps = 100000000;
let word = 1000 ^ (3 << 27);
let tmp1;
let time = process.hrtime.bigint();

for(let i = 0; i < reps; i++) { tmp1 = a(b(c(100))); }
//for(let i = 0; i < reps; i++) { tmp1 = tag(word); }
//for(let i = 0; i < reps; i++) { tmp1 = val(word); }

let time2 = process.hrtime.bigint();
let timeWith = time2 - time;
console.log(`Elapsed time with function calls: ${ timeWith } nanoseconds`);
let tmp2;
time = process.hrtime.bigint();

for(let i = 0; i < reps; i++) { tmp2 = 100*2 - 1 + 1; }
//for(let i = 0; i < reps; i++) { tmp2 = (word >>> 27) & 7; }
//for(let i = 0; i < reps; i++) { tmp2 = word & ((1 << 27)-1); }

time2 = process.hrtime.bigint();
let timeWithout = time2 - time;
console.log(`Elapsed time without function calls: ${ timeWithout } nanoseconds`);

let percentWith = 100 * parseFloat(timeWith) / parseFloat(timeWithout);
console.log(`\nThe time with function calls is ${ percentWith } percent\n` +
    `of time without function calls.`);

console.log(`\nEach repetition with a function call used roughly ` +
    `${ parseFloat(timeWith) / reps } nanoseconds.` +
    `\nEach repetition without a function call used roughly ` +
    `${ parseFloat(timeWithout) / reps } nanoseconds.`);