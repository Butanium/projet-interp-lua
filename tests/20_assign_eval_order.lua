x = {}
x.a = 1
function f() y = 1; return x end

y = 2
f().a = y
print(x.a, y)


x = {}
z = {}
y = 0;

function f() print("tab", x.a); x.a = 1; return x end
function g() print("key", x.a); x.a = 2; return "a" end
function h() print("val", x.a); x.a = 3; return 12 end

f()[g()] = h()
print(x.a)

