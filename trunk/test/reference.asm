
a		= b
b		= c
c		= d
d		= e
e		= f
f		= x

x		.block
o		= "test"
		.bend

		.cerror a.o != "test"

;		lda unused

unused		= +.test
+		.proc
test
		.fill 9999999,0
		.pend
