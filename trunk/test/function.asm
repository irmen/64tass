
wpack		.function a,b=0
		.endf a+b*256

		.cerror wpack(1)!=$01
		.cerror wpack(1,2)!=$0201

m1		.macro
t		= \1
		.endm

e		#m1 2
		.cerror e.t != 2

mb		.block		;local macro
m1		.macro
t		= \1
		.endm
		.bend

silly		= [mb.m1]

silly2		.function a
		.endf a

e2		#silly[1-1] 2
		.cerror e2.t != 2

e3		#silly2(mb.m1) 2
		.cerror e3.t != 2
