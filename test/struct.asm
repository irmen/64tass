
		.dsection declaration

		.union
a		nop
		.section declaration
new		.struct
		nop
		.ends
new2		.union
		nop
		.word 2
		nop
		.endu
		.send declaration
		.struct
b		nop
c		nop
		.ends
x		.struct
b		nop
c		nop
d		.union
e		nop
f		nop
		.endu
g		.struct
h		nop
i		nop
		.ends
n		.dstruct new
n2		.dunion new2
		.ends
		.endu

		.if 0
		.union
		.endu
		.struct
		.ends
		.fi

		.cerror a != b
		.cerror a != (c - 1)
		.cerror a != x
		.cerror a != x.b
		.cerror a != (x.c - 1)
		.cerror a != (x.d - 2)
		.cerror a != (x.d.e - 2)
		.cerror a != (x.d.f - 2)
		.cerror a != (x.g - 3)
		.cerror a != (x.g.h - 3)
		.cerror a != (x.g.i - 4)
		.cerror a != (x.n - 5)
		.cerror a != (x.n2 - 6)

		.cerror size(a) != 1
		.cerror size(b) != 1
		.cerror size(c) != 1
		.cerror size(x) != 8
		.cerror size(x.g) != 2
		.cerror size(x.n2) != 2
