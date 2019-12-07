"use strict";

var stdout_buffer = "";

stdout = function (msg)
{
    // if(! debugging) {
    //     return;
    // }

    var lines = (stdout_buffer + msg).split('\n');
    for (var i = 0; i < lines.length-1; i++)
    {
        console.log(lines[i]);
    }
    stdout_buffer = lines[lines.length-1];
};

predicate_flush_stdout = function ()
{
    if (stdout_buffer !== "")
        stdout("\n");
    return true;
};

function alert(msg) {
    console.log('alert:' + msg);
}
