
    .cerror 2 ** 2 ** 3 != 256
    .cerror 10 / 3 != 3
    .cerror 10 % 3 != 1
    .cerror (-10) / 3 != -4
    .cerror (-10) % 3 != 2
    .cerror 10 / (-3) != -4
    .cerror 10 % (-3) != -2
    .cerror (-10) / (-3) != 3
    .cerror (-10) % (-3) != -1

    .end

; "logical" operators

    .cerror ("" && []) != ""
    .cerror ("" && (1,)) != ""
    .cerror (2 && "") != ""
    .cerror (1 && 2) != 2

    .cerror (() || "") != ""
    .cerror ("" || 2) != 2
    .cerror (2 || "") != 2
    .cerror (1 || [2,]) != 1

    .cerror ("" ^^ []) != 0
    .cerror ("" ^^ 2) != 2
    .cerror (1 ^^ "") != 1
    .cerror (1 ^^ 2) != 0

    .cerror 1 ? 0 : unknown
    .cerror 0 ? unknown : 0
    .cerror 0 && unknown
    .cerror !(1 || unknown)

    .cerror (5 <=> 7) != -1
    .cerror (6 <=> 6) != 0
    .cerror (7 <=> 5) != 1
    .cerror 1==0
    .cerror 0!=0
    .cerror 1 < 0
    .cerror 0 > 1
    .cerror 0 >= 1
    .cerror 1 <= 0
    .cerror 2*2!=4
    .cerror 6/2!=3
    .cerror 6%4!=2
    .cerror 2+2!=4
    .cerror 2-3!=-1
    .cerror 2&6!=2
    .cerror 2|4!=6
    .cerror 7^12!=11
    .cerror 1 << 3 != 8
    .cerror -8 >> 3 != -1
    .cerror ~0!=-1
    .cerror (^2049)!="2049"
    .cerror 2**3 != 8
    .cerror 2**0 != 1
    .cerror "a"=="b"
    .cerror "a"!="a"
    .cerror "a">"b"
    .cerror "b"<"a"
    .cerror "a">="b"
    .cerror "b"<="a"
    .cerror 2.5+25e-1!=5
    .cerror [1,2]!=[1,2]
    .cerror (1,2)!=(1,2)

    .cerror "abcdef"[1]!="b"
    .cerror "abcdef"[1:]!="bcdef"
    .cerror "abcdef"[:2]!="ab"
    .cerror "abcdef"[:22]!="abcdef"
    .cerror "abcdef"[1:2]!="b"
    .cerror "abcdef"[-2:-1]!="e"
    .cerror "abcdef"[-23:-1]!="abcde"
    .cerror "abcdef"[-2:]!="ef"
    .cerror "abcdef"[:]!="abcdef"
    .cerror "abcdef"[22:]!=""
    .cerror "abcdef"[-2:-13]!=""
    .cerror "abcdef"[::2]!="ace"
    .cerror "abcdef"[::3]!="ad"
    .cerror "abcdef"[:-2:2]!="ac"
    .cerror "abcdef"[1:-2:2]!="bd"
    .cerror "abcdef"[[1,3,5]]!="bdf"

    .cerror b"abcdef"[1]!="b"
    .cerror b"abcdef"[1:]!=b"bcdef"
    .cerror b"abcdef"[:2]!=b"ab"
    .cerror b"abcdef"[:22]!=b"abcdef"
    .cerror b"abcdef"[1:2]!=b"b"
    .cerror b"abcdef"[-2:-1]!=b"e"
    .cerror b"abcdef"[-23:-1]!=b"abcde"
    .cerror b"abcdef"[-2:]!=b"ef"
    .cerror b"abcdef"[:]!=b"abcdef"
    .cerror b"abcdef"[22:]!=b""
    .cerror b"abcdef"[-2:-13]!=b""
    .cerror b"abcdef"[::2]!=b"ace"
    .cerror b"abcdef"[::3]!=b"ad"
    .cerror b"abcdef"[:-2:2]!=b"ac"
    .cerror b"abcdef"[1:-2:2]!=b"bd"
    .cerror b"abcdef"[[1,3,5]]!=b"bdf"

    .cerror (1,2,3,4,5,6)[1]!=2
    .cerror (1,2,3,4,5,6)[1:]!=(2,3,4,5,6)
    .cerror (1,2,3,4,5,6)[:2]!=(1,2)
    .cerror (1,2,3,4,5,6)[:22]!=(1,2,3,4,5,6)
    .cerror (1,2,3,4,5,6)[1:2]!=(2,)
    .cerror (1,2,3,4,5,6)[-2:-1]!=(5,)
    .cerror (1,2,3,4,5,6)[-23:-1]!=(1,2,3,4,5)
    .cerror (1,2,3,4,5,6)[-2:]!=(5,6)
    .cerror (1,2,3,4,5,6)[:]!=(1,2,3,4,5,6)
    .cerror (1,2,3,4,5,6)[22:]!=()
    .cerror (1,2,3,4,5,6)[-2:-13]!=()
    .cerror (1,2,3,4,5,6)[::2]!=(1,3,5)
    .cerror (1,2,3,4,5,6)[::3]!=(1,4)
    .cerror (1,2,3,4,5,6)[:-2:2]!=(1,3)
    .cerror (1,2,3,4,5,6)[1:-2:2]!=(2,4)
    .cerror (1,2,3,4,5,6)[[1,3,5]]!=(2,4,6)

    .cerror (0 ? 2 : 3)!=3
    .cerror (1 ? 2 : 3)!=2
    .cerror (1 ? 1 ? 2 : 3 : 1 ? 4 : 5)!=2
    .cerror (1 ? 1 ? 2 : 3 : 0 ? 4 : 5)!=2
    .cerror (1 ? 0 ? 2 : 3 : 1 ? 4 : 5)!=3
    .cerror (1 ? 0 ? 2 : 3 : 0 ? 4 : 5)!=3
    .cerror (0 ? 1 ? 2 : 3 : 1 ? 4 : 5)!=4
    .cerror (0 ? 1 ? 2 : 3 : 0 ? 4 : 5)!=5
    .cerror (0 ? 0 ? 2 : 3 : 1 ? 4 : 5)!=4
    .cerror (0 ? 0 ? 2 : 3 : 0 ? 4 : 5)!=5

    .cerror "abc"[-1 ? 1 : 2 : 0 ? 1 : 2]!="b"
    .cerror [-1 ? 1 : 2 , 0 ? 1 : 2]!=[1,2]
    .cerror "a".."b"!="ab"
    .cerror "a"..b"b"!=b"ab"
    .cerror b"a".."b"!=b"ab"
    .cerror b"a"..b"b"!=b"ab"
    .cerror (1,2)..(3,4)!=(1,2,3,4)
    .cerror [1,2]..[3,4]!=[1,2,3,4]

    .cerror (3 | %)[0]!=1
    .cerror $1234[4:8]!=3
    .cerror $9..($1234 >> 4)!=$9123
    .cerror len(15 | %)!=4
    .cerror len((-64) | %)!=7
    .cerror len((+63) | %)!=6
    .cerror len(-$123)!=12
    .cerror len(~%101)!=3

    .cerror (-680876936 >> 8) != -2659676

    .cerror !(1 in [1,2,3])
    .cerror 1 in [2,3]
    .cerror !("a" in "abc")
    .cerror "a" in "bc"
    .cerror "abc" x 2 != "abcabc"
    .cerror [1,2,3] x 2 != [1,2,3,1,2,3]

    .cerror [1,2]*[3,4]!=[3,8]
    .cerror [[1,2],[3,4]]*[5,6]!=[[5,12],[15,24]]
    .cerror [5,6]*[[1,2],[3,4]]!=[[5,12],[15,24]]
    .cerror [[5],[6]]*[[1,2],[3,4]]!=[[5,10],[18,24]]

    .cerror ? != ?
    .cerror !(? == ?)
    .cerror ? in (1,2)
    .cerror !(? in (1,?,3))

a   .block
c   = 1
    .bend

b   .block
c   = 2
-
    .bend

    .cerror (a,b).c != (a.c, b.c)
    .cerror a.(c,c) != (a.c, a.c), a.(c,c), (a.c, a.c)
    .cerror (a,b).(c,c) != (a.c, b.c)
    .cerror b.(-) != b
    .cerror b.(-,-) != (b,b)
    .cerror (b,b).(-,-) != (b,b)
    .cerror (b,b).(-) != (b,b)
    .cerror {1:"test","3":4}[1] != "test"

    .cerror (~$f)..(~$e) != (~$fe)
    .cerror (~$f)..$e != (~$f1)
    .cerror $f..(~$e) != $f1
    .cerror $f..$e != $fe

    ; bit and length
    .cerror repr($0f & $f) != repr($f)
    .cerror repr($f & $0f) != repr($f)
    .cerror repr(~$0f & $f) != repr($0)
    .cerror repr(~$f & $0f) != repr($00)
    .cerror repr($0f & ~$f) != repr($00)
    .cerror repr($f & ~$0f) != repr($0)
    .cerror repr(~$0f & ~$f) != repr(~$0f)
    .cerror repr(~$f & ~$0f) != repr(~$0f)

    ; bit or length
    .cerror repr($0f | $f) != repr($0f)
    .cerror repr($f | $0f) != repr($0f)
    .cerror repr(~$0f | $f) != repr(~$00)
    .cerror repr(~$f | $0f) != repr(~$0)
    .cerror repr($0f | ~$f) != repr(~$0)
    .cerror repr($f | ~$0f) != repr(~$00)
    .cerror repr(~$0f | ~$f) != repr(~$f)
    .cerror repr(~$f | ~$0f) != repr(~$f)

    .cerror (1,b)+(1,x) != (2,b,x)
    .cerror (3,x)-(1,x) != 2

    .cerror (1 <=> 2) != ($1 <=> $2)
    .cerror (1 <=> 1) != ($1 <=> $1)
    .cerror (2 <=> 1) != ($2 <=> $1)
    .cerror (~1 <=> ~2) != (~$1 <=> ~$2)
    .cerror (~1 <=> ~1) != (~$1 <=> ~$1)
    .cerror (~2 <=> ~1) != (~$2 <=> ~$1)
    .cerror (1 <=> 2) != (b"1" <=> b"2")
    .cerror (1 <=> 1) != (b"1" <=> b"1")
    .cerror (2 <=> 1) != (b"2" <=> b"1")
    .cerror ("a" <=> "ab") != (b"a" <=> b"ab")
    .cerror ("a" <=> "a") != (b"a" <=> b"a")
    .cerror ("ab" <=> "a") != (b"ab" <=> b"a")
    .cerror ~"a" != ~+"a"
    .cerror ~b"abcd" != ~+"abcd"
    .cerror (`b"abcd") != (`+"abcd")
    .cerror (<b"abcd") != (<+"abcd")
    .cerror (>b"abcd") != (>+"abcd")
    .cerror (<>b"abcd") != (<>+"abcd")
    .cerror (><b"abcd") != (><+"abcd")

; check all operators

; bit or
    .byte $0a | $b0
; bit xor
    .byte $0a ^ $ba

; bool not
    lda !*

; force 8bit addr
    lda @b $ff
; force 16bit addr
    lda @w $ff

    .cpu 65816

; force 24bit addr
    lda @l $ff
