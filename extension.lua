
-- Générateur de nombres premiers
function iter_prime()
    return coroutine.create(function()
        local n, primes, is_prime, n_prime, i
        i = 0
        n = 2
        n_prime = 0
        is_prime = true
        primes = {}
        while true do
            while i < n_prime do
                if n % primes[i] == 0 then
                    is_prime = false
                    i = n_prime -- break
                end
                i = i + 1
            end
            if is_prime then
                primes[n_prime] = n
                n_prime = n_prime + 1
                coroutine.yield(n)
            end
            n = n + 1
            is_prime = true
            i = 0
        end
    end)
end

function prime_gen()
    local co 
    co = iter_prime()
    return function()
        return coroutine.mini_resume(co)
    end
end
i = 0
prime = prime_gen()
print("---- 10 premiers nombres premiers ----")
while i < 10 do
    print(prime())
    i = i + 1
end

-- Générateur de nombres pseudo-aléatoires
function iter_random()
    return coroutine.create(function()
        local n, a, c, m, x
        n = 0
        a = 16807
        c = 0
        m = 2147483648 - 1 -- 2^31 - 1
        x = 1
        while true do
            x = (a * x + c) % m
            coroutine.yield(x)
        end
    end)
end

function random_gen()
    local co 
    co = iter_random()
    return function()
        return coroutine.mini_resume(co)
    end
end

i = 0
random = random_gen()
print("---- 20 nombres pseudo-aléatoires ----")
while i < 20 do
    print(random())
    i = i + 1
end
