local i = 10
do
  local x = 42
  do
    local x = x + 6
    print(x) -- 48
    do
      x = 35
      local x = x + 8
      print(x) -- 43
    end
    print(x) -- 35
  end
end
print(i, x, y, z) -- 10  nil  nil  nil
