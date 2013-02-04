
.cerror "teszt"[1]!='e'
.cerror "teszt"[1:]!='eszt'
.cerror "teszt"[1:5]!='eszt'
.cerror "teszt"[:5]!='teszt'
.cerror "teszt"[:]!='teszt'
.cerror "teszt"[:-1]!='tesz'
.cerror "teszt"[1:-1]!='esz'

list .var [1,2,3,4,5]
.cerror list[1]!=2
.cerror list[1:]!=[2,3,4,5]
.cerror list[1:5]!=[2,3,4,5]
.cerror list[:5]!=[1,2,3,4,5]
.cerror list[:]!=[1,2,3,4,5]
.cerror list[:-1]!=[1,2,3,4]
.cerror list[1:-1]!=[2,3,4]

list .var (1,2,3,4,5)
.cerror list[1]!=2
.cerror list[1:]!=(2,3,4,5)
.cerror list[1:5]!=(2,3,4,5)
.cerror list[:5]!=(1,2,3,4,5)
.cerror list[:]!=(1,2,3,4,5)
.cerror list[:-1]!=(1,2,3,4)
.cerror list[1:-1]!=(2,3,4)
