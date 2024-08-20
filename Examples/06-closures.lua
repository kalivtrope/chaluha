local f
do
  local x = 7
  function f(foo, bar, baz)
    local g
    function g()
      x = x + 1
      return x, foo
    end
    return g
  end
end

x = 8
if x == 8 then
  local x = x + 5
  print("foo", x)
elseif x == 6 then
  print(4)
elseif x == 8 then
  print("bar", 8)
else
  print(5)
end
print("baz", x)

g1 = f("qux")
g2 = f()
g3 = f()
x = 3
print(g1())
print(g2())
print(x)
