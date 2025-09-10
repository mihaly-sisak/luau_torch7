local function print_tensor(x)
    local line = "tensor<"
    for d = 1, x:dim() do
        line = line .. tostring(x:size(d))
        if d ~= x:dim() then
            line = line .. "x"
        end
    end
    line = line .. ">("
    for i = 1, x:nElement() do
        line = line .. x:storage()[x:storageOffset()+i-1]
        if i ~= x:nElement() then
            line = line .. ", "
        end
    end
    line = line .. ")"
    print(line)
end

local x = torch.FloatTensor(2, 2):fill(1)
local y = torch.FloatTensor(2, 2):fill(2)
local z = x + y

print_tensor(x)
print_tensor(y)
print_tensor(z)

