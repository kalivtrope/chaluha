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
  print("foo", x)  -- 13
elseif x == 6 then
  print(4)
elseif x == 8 then
  print("bar", 8)
else
  print(5)
end
print("baz", x) -- "baz"  8

g1 = f("qux")
g2 = f("quz")
g3 = f("qut")
x = 3
print(g1())
print(g2())
print(g3())
print(x)
