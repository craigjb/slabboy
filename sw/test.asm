SECTION "ROM0", ROM0

inc a

inc b
inc b
inc b

add a, b

inc c
inc c

sub a, c
sub a, c

dec c
sub a, c

dec h
xor a, h

inc a
inc a
inc a
inc a

inc l

or a, l

ld a, $AA
ld b, $BB
ld c, $CC
ld d, $DD
ld e, $EE
ld h, $55
ld l, $77

halt
