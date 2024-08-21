x = -- foo
8
do
  y = 6
  r = 8
end

print(x, y)

val = 0 -- truthy in lua
val2 = "foo"
val3 = nil
if val then
  y = r
  print("y1", y)
  local x, y = 5, 8
  print("y2", y)
elseif val2
  then z = 5
elseif val3
  then z = "bar"
end
    -- 
print(x, y, z) -- 8  8  nil
