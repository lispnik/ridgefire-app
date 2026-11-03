#!/usr/bin/env python3
"""make-sounds.py -- write res/sounds/*.wav, synthesised with the standard
library alone: 16-bit mono at 44.1kHz.

    python3 tools/make-sounds.py

fire     a shell leaves the barrel: a low thump with a crack
boom     a small blast: filtered noise with a falling tone
bigboom  a large blast: longer, deeper, with a rumble
thud     earth moved: a dull knock
coin     a purchase: two bright notes
click    a button: a tick
turn     your turn: a soft rising blip
win      a round won: an arpeggio
"""
import math, os, random, struct, wave

RATE = 44100
random.seed(7)


def tone(freq, seconds, volume=0.5, decay=8.0, glide=None, harmonics=((1, 1.0),)):
    out, phase, n = [], 0.0, int(seconds * RATE)
    for i in range(n):
        t = i / RATE
        f = freq if glide is None else freq + (glide - freq) * (i / n)
        phase += 2 * math.pi * f / RATE
        env = math.exp(-decay * t) * min(1.0, i / 60)
        out.append(volume * env * sum(a * math.sin(k * phase) for k, a in harmonics))
    return out


def noise(seconds, volume=0.6, decay=6.0, smooth=0.2):
    """Low-passed white noise: SMOOTH near 0 is darker."""
    out, y, n = [], 0.0, int(seconds * RATE)
    for i in range(n):
        t = i / RATE
        y += smooth * (random.uniform(-1, 1) - y)
        out.append(volume * y * math.exp(-decay * t) * min(1.0, i / 30))
    return out


def mix(*parts):
    length = max(int(offset * RATE) + len(samples) for offset, samples in parts)
    out = [0.0] * length
    for offset, samples in parts:
        start = int(offset * RATE)
        for i, s in enumerate(samples):
            out[start + i] += s
    return out


def write(name, samples):
    peak = max(1e-9, max(abs(s) for s in samples))
    scale = min(1.0, 0.95 / peak)
    path = os.path.join(os.path.dirname(__file__), "..", "res", "sounds", name + ".wav")
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(RATE)
        w.writeframes(b"".join(struct.pack("<h", int(32767 * s * scale)) for s in samples))
    print("wrote", os.path.normpath(path))


write("fire", mix((0, tone(110, 0.25, 0.7, 14, glide=55)), (0, noise(0.12, 0.5, 30, 0.6))))
write("boom", mix((0, noise(0.6, 0.9, 6, 0.12)), (0, tone(80, 0.5, 0.6, 7, glide=35))))
write("bigboom", mix((0, noise(1.4, 1.0, 2.8, 0.06)), (0, tone(60, 1.2, 0.8, 3, glide=25)),
                     (0.05, noise(1.0, 0.4, 4, 0.3))))
write("thud", mix((0, tone(140, 0.25, 0.7, 18, glide=70)), (0, noise(0.2, 0.4, 20, 0.08))))
write("coin", mix((0, tone(1320, 0.12, 0.35, 18, harmonics=((1, 1), (2, 0.3)))),
                  (0.07, tone(1760, 0.3, 0.35, 10, harmonics=((1, 1), (2, 0.3))))))
write("click", tone(2200, 0.03, 0.3, 120))
write("turn", tone(520, 0.18, 0.3, 12, glide=780, harmonics=((1, 1), (3, 0.15))))
write("win", mix(*[(0.11 * i, tone(f, 0.5, 0.3, 5, harmonics=((1, 1), (2, 0.4), (3, 0.15))))
                   for i, f in enumerate((523, 659, 784, 1047))]))
