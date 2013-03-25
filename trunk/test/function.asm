
wpack		.function a,b=0
		.endf a+b*256

		.cerror wpack(1)!=$01
		.cerror wpack(1,2)!=$0201

