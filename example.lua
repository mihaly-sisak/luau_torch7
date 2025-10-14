local x = torch.FloatTensor(2, 2):range(1,4)
local y = torch.FloatTensor(2, 2):fill(1)
local z = x + y

print(x)
print(y)
print(z)

print(c_tensor_init())
print(c_tensor_modify(c_tensor_init()))

if (torch.hasNoise) then

    local function test_noise(size, dim, name, gen_func)
        local time_start = os.clock()
        local n = gen_func(size)
        -- get position of tensor values 0.1 < n < 0.2
        local s = torch.nonzero(torch.cbitand(torch.gt(n, 0.1), torch.lt(n, 0.2)))
        local time_stop = os.clock()
        print(name .. ":")
        print("    generated and filtered " .. math.pow(size, dim) .. " values")
        print("    took " .. time_stop-time_start .. " seconds")
        print("    min = " .. torch.min(n) .. ", max = " .. torch.max(n))
        print("    num of indexes = " .. s:size(1))
        print()
    end

    test_noise(2290, 2, "simplex2D", function(size) return torch.simplex2D(0, 0, size, size, 1, 1, 1337) end)
    test_noise(2290, 2, "node2D"   , function(size) return torch.node2D("GwkaCQ0JBwAAgN1CBAIIAABADEMMAwAAgD8EAwAAgL8E", 0, 0, size, size, 1, 1, 1337) end)
    test_noise( 174, 3, "simplex3D", function(size) return torch.simplex3D(0, 0, 0, size, size, size, 1, 1, 1, 1337) end)
    test_noise( 174, 3, "node3D"   , function(size) return torch.node3D("GwkaCQ0JBwAAgN1CBAIIAABADEMMAwAAgD8EAwAAgL8E", 0, 0, 0, size, size, size, 1, 1, 1, 1337) end)

end

local function tracetest1()
    print("tracetest1")
    error("example error")
end

local function tracetest2()
    print("tracetest2")
    tracetest1()
end

local function tracetest3()
    print("tracetest3")
    tracetest2()
end

tracetest3()
