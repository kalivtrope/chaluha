function foo()
  return "foo", "bar", "baz"
end

x, y, z = foo(), foo()
print(x,y,z)    -- "foo"   "foo"   "bar"
