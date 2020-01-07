var thunk = function(something, list){
  var t = function(){
    if(list){
      list.push(t);
    }
    return something;
  };
  return t;
};
var cycle = function(val){
  var x = [];
  x.push(thunk(val, x));
  return x;
};
var step_returns = function(x, a){
  return x || a();
};
var step_inf = function(x, a){
  return a() || x;
};
var foldr = function(f, z, list){
  if(list.length == 0){
    // this is pattern matching
    // foldr _ z [] = z
    return z;
  } else {
    var head = list[0]();
    list.shift();
    return f(head, function(){
      foldr(f, z, list);
    });
  }
};
var list = cycle(true);
console.log( foldr(step_returns, false, list) );
console.log( foldr(step_inf, false, list) );
