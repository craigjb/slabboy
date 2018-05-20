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

ld a, b
ld b, l
ld h, d
ld d, c

ld h, $0
ld l, $4
ld a, [hl]
ld h, $0
ld l, $3
add a, [hl]

ld h, $20
ld l, $00
ld b, a
ld [hl], b

inc l
ld [hl], $58
inc l
ld [hl+], a
ld [hl+], a
ld [hl-], a

ld h, $00
ld l, $00
ld a, [hl+]
ld a, [hl+]
ld a, [hl+]

ld a, $7F
ldh [$27], a
ld a, $AA
ldh a, [$27]

ld c, $40
ld [c], a
ld a, $AA
ld a, [c]

ld h, $30
ld l, $00
inc [hl]
inc [hl]
inc [hl]
dec [hl]

ld a, $75
ld [$3040], a
ld a, $AA
ld a, [$3040]

scf
ccf
ccf

ld a, $0F
cpl

halt
