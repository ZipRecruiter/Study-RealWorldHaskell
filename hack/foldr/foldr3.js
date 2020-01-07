var thunk = function(something){
  var t = function(){
    return something;
  };
  return t;
};
var cycle = function(val){
  var x = {
    _data: [],
  };
  x._data.push(thunk(val));
  x.next = function(){
    x._data.push(thunk(val));
    return x._data.shift();
  };
  x.push = x._data.push;
  return x;
};
var step_returns = function(x, a){
  return x() || a();
};
var step_inf = function(x, a){
  return a() || x();
};
var foldr = function(f, z, list){
  if(list.length == 0){
    // this is pattern matching
    // foldr _ z [] = z
    return z;
  } else {
    var head = list.next();
    return f(head, function(){
      foldr(f, z, list);
    });
  }
};
var list = cycle(true);
console.log( foldr(step_returns, false, list) );
console.log( foldr(step_inf, false, list) );
