R = [f"R{r}" for r in range(0, 360, 90)]
print(R)

C = ["Clockwise", "CounterClockwise"]

N = [[
    ("3>>0", [(-1, 0), (-1,-1), ( 0, 2), (-1, 2)]),
    ("0>>1", [(-1, 0), (-1, 1), ( 0,-2), (-1,-2)]),
    ("1>>2", [( 1, 0), ( 1,-1), ( 0, 2), ( 1, 2)]),
    ("2>>3", [( 1, 0), ( 1, 1), ( 0,-2), ( 1,-2)]),
],[
    ("1>>0", [( 1, 0), ( 1,-1), ( 0, 2), ( 1, 2)]),
    ("2>>1", [(-1, 0), (-1, 1), ( 0,-2), (-1,-2)]),
    ("3>>2", [(-1, 0), (-1,-1), ( 0, 2), (-1, 2)]),
    ("0>>3", [( 1, 0), ( 1, 1), ( 0,-2), ( 1,-2)]),
]]

I = [[
    ("3>>0", [( 1, 0), (-2, 0), ( 1,-2), (-2, 1)]),
    ("0>>1", [(-2, 0), ( 1, 0), (-2,-1), ( 1, 2)]),
    ("1>>2", [(-1, 0), ( 2, 0), (-1, 2), ( 2,-1)]),
    ("2>>3", [( 2, 0), (-1, 0), ( 2, 1), (-1,-2)]),
],[
    ("1>>0", [( 2, 0), (-1, 0), ( 2, 1), (-1,-2)]),
    ("2>>1", [( 1, 0), (-2, 0), ( 1,-2), (-2, 1)]),
    ("3>>2", [(-2, 0), ( 1, 0), (-2,-1), ( 1, 2)]),
    ("0>>3", [(-1, 0), ( 2, 0), (-1, 2), ( 2,-1)]),
]]

for c, n in zip(C, I):
    for r, m in zip(R, n):
        _, p = m
        p = [ (-y,x) for (x, y) in p ]
        print(f"kicks I {c} {r}\t\t= {p}")

for c, n in zip(C, N):
    for r, m in zip(R, n):
        _, p = m
        p = [ (-y,x) for (x, y) in p ]
        print(f"kicks _ {c} {r}\t\t= {p}")

