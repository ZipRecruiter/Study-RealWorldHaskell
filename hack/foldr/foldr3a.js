var thunk = function(v){
  return function(){ return v; }
};
var cycle = function(val){
  var x = [thunk(val)];
  return (x.next = function(){ return x.push(thunk(val)) && x.shift(); }) && x
};
var foldr = function(f, z, list){
  return list.length == 0 ? z : f(list.next(), function(){ foldr(f, z, list); })
};
console.log( foldr(function(x, a){ return x() || a(); }, false, cycle(true)) )
console.log( foldr(function(x, a){ return a() || x(); }, false, cycle(true)) )
