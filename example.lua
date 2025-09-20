local x = torch.Tensor(2, 2):range(1,4)
local y = torch.Tensor(2, 2):fill(1)
local z = x + y

print(x)
print(y)
print(z)

print(c_tensor_init())
print(c_tensor_modify(c_tensor_init()))

if (torch.hasNoise) then

    local time_start = os.clock()

    local size = 2290
    --local n = torch.node2D("GwkaCQ0JBwAAgN1CBAIIAABADEMMAwAAgD8EAwAAgL8E", 0, 0, size, size, 1337)
    local n = torch.simplex2D(100, 0, 0, size, size, 1337)
    -- get position of tensor values 0.1 < n < 0.2
    local s = torch.nonzero(torch.cbitand(torch.gt(n, 0.1), torch.lt(n, 0.2)))

    local time_stop = os.clock()

    print("noise:")
    print("    generated and filtered " .. size*size .. " values")
    print("    took " .. time_stop-time_start .. " seconds")
    print("    min = " .. torch.min(n) .. ", max = " .. torch.max(n))
    print("    num of indexes = " .. s:size(1))
    print()
end

local tracetest1 = function()
    print("tracetest1")
    error("example error")
end

local tracetest2 = function()
    print("tracetest2")
    tracetest1()
end

local tracetest3 = function()
    print("tracetest3")
    tracetest2()
end

tracetest3()
