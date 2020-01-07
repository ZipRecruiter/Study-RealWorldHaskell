thunk = (value, list)->
  t = ->
    list.push t if list
    value

cycle = (value)->
  list = []
  list.push thunk value, list
  list

foldr = (step, acc, list)->
  return acc if list.length == 0
  step list.shift()(), ->
    foldr step, acc, list

console.log foldr ((x, a)-> x || a()), false, cycle true
console.log foldr ((x, a)-> a() || x), false, cycle true
