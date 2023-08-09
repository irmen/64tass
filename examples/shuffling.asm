;
; While there's no built-in shuffle function at the moment
; here's an example function which can shuffle sliceable
; objects like lists, strings, bytes.
;
        .seed 2021

shuffle .function _list
        .for _i in random(range(len(_list), 0, -1))
_list   := _list[:_i] .. _list[_i+1:] .. _list[_i:_i+1]
        .next
        .endf _list

        .warn shuffle(['a','b','c','d','e','f','g','h'])
