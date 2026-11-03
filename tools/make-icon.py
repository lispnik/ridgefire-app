#!/usr/bin/env python3
"""make-icon.py -- write res/Ridgefire.xcassets, the app icon: a tank on a
ridge at dusk, the dotted arc of its shell, and the burst where it lands.
1024x1024 with no alpha, drawn with the standard library alone.

    python3 tools/make-icon.py
"""
import json, math, os, struct, zlib

N = 1024
SKY_TOP, SKY_BOTTOM = (26, 20, 66), (245, 133, 92)
HILLS = (82, 46, 78)
EARTH_TOP, EARTH_BOTTOM = (117, 77, 54), (46, 28, 23)
GRASS = (128, 168, 72)
TANK, TANK_DARK = (237, 69, 69), (150, 40, 40)


def lerp(a, b, t):
    return tuple(int(a[i] + (b[i] - a[i]) * t) for i in range(3))


def ground(x):
    u = x / N
    return 300 + 120 * math.sin(u * 2.4 + 0.3) + 40 * math.sin(u * 7.0 + 1.0)


def far(x):
    u = x / N
    return 480 + 60 * math.sin(u * 4.0 + 2.0) + 25 * math.sin(u * 11.0)


TANK_X = 270
TANK_Y = ground(TANK_X)
BURST = (800, ground(800) + 10)


def arc_points():
    """A parabola from the barrel to the burst, as dots."""
    x0, y0 = TANK_X + 40, TANK_Y + 75
    x1, y1 = BURST
    top = 900
    pts = []
    for i in range(1, 15):
        t = i / 15
        x = x0 + (x1 - x0) * t
        y = (1 - t) ** 2 * y0 + 2 * (1 - t) * t * top + t * t * y1
        pts.append((x, y))
    return pts


DOTS = arc_points()


def pixel(x, y):
    # y up from the bottom.
    if y < ground(x):
        if y > ground(x) - 14:
            c = GRASS
        else:
            c = lerp(EARTH_BOTTOM, EARTH_TOP, max(0, y / ground(x)) ** 0.7)
            c = lerp(c, (0, 0, 0), 0.06 * (1 + math.sin(y * 0.09)))
    elif y < far(x):
        c = lerp(lerp(SKY_BOTTOM, SKY_TOP, y / N), HILLS, 0.6)
    else:
        c = lerp(SKY_BOTTOM, SKY_TOP, y / N)
    # The burst.
    d = math.hypot(x - BURST[0], y - BURST[1])
    if d < 150:
        glow = (1 - d / 150) ** 1.5
        c = lerp(c, (255, 235, 150) if d < 60 else (255, 150, 50), min(1, glow * 1.6))
    # The tank: treads, hull, dome, barrel.
    tx, ty = x - TANK_X, y - TANK_Y
    if abs(tx) < 70 and 0 <= ty < 26:
        c = (40, 38, 44)
    elif abs(tx) < 60 and 26 <= ty < 48:
        c = TANK_DARK
    elif math.hypot(tx, ty - 50) < 36 and ty >= 48:
        c = TANK
    else:
        # Barrel at 55 degrees from the dome's centre.
        a = math.radians(55)
        bx, by = tx, ty - 58
        along = bx * math.cos(a) + by * math.sin(a)
        across = -bx * math.sin(a) + by * math.cos(a)
        if 0 < along < 95 and abs(across) < 10:
            c = TANK_DARK
    for (px, py) in DOTS:
        if math.hypot(x - px, y - py) < 11:
            c = (255, 245, 220)
    return c


def png(width, height, rows):
    def chunk(kind, data):
        return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF)
    raw = b"".join(b"\x00" + bytes(v for p in row for v in p) for row in rows)
    return (b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(raw, 9)) + chunk(b"IEND", b""))


rows = [[pixel(x + 0.5, N - 1 - y + 0.5) for x in range(N)] for y in range(N)]
root = os.path.join(os.path.dirname(__file__), "..", "res", "Ridgefire.xcassets")
icon = os.path.join(root, "AppIcon.appiconset")
os.makedirs(icon, exist_ok=True)
with open(os.path.join(icon, "icon.png"), "wb") as f:
    f.write(png(N, N, rows))
with open(os.path.join(root, "Contents.json"), "w") as f:
    json.dump({"info": {"author": "ridgefire", "version": 1}}, f, indent=2)
with open(os.path.join(icon, "Contents.json"), "w") as f:
    json.dump({"images": [{"filename": "icon.png", "idiom": "universal", "platform": "ios", "size": "1024x1024"}],
               "info": {"author": "ridgefire", "version": 1}}, f, indent=2)
print("wrote", os.path.normpath(os.path.join(icon, "icon.png")))
