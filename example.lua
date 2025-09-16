local x = torch.Tensor(2, 2):range(1,4)
local y = torch.Tensor(2, 2):fill(1)
local z = x + y

print(x)
print(y)
print(z)

print(c_tensor_init())
print(c_tensor_modify(c_tensor_init()))

local test = function(a)
    print(a)
    error(a)
end

test("a")
