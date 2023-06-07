
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
while i < 10 do
    print(prime())
    i = i + 1
end
