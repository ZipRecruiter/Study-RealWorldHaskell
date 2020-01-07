thunk = (v)->
  -> v

cycle = (val)->
  x = [thunk(val)]
  (x.next = -> x.push(thunk val) and x.shift()) and x

foldr = (f, z, list)->
  if list.length == 0 then z else f list.next(), -> foldr f, z, list

console.log foldr ((x, a)-> x() || a()), false, cycle true
console.log foldr ((x, a)-> a() || x()), false, cycle true
