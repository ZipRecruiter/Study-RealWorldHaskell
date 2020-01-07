thunk = (value)->
  -> value

cycle = (value)->
  list = [thunk(value)]
  list.next = ->
    list.push thunk(value)
    list.shift()
  list

foldr = (step, acc, list)->
  return acc if list.length == 0
  step list.next(), ->
    foldr step, acc, list

console.log foldr ((x, a)-> x() || a()), false, cycle true
console.log foldr ((x, a)-> a() || x()), false, cycle true
