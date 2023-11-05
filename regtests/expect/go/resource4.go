// Advanced Resource Embedder 1.4.0
package Resource4

import (
  "strings"
  "sort"
)

type Content struct {
    Content  []byte
    Size  int64
    Modtime  int64
    Format   int
}


var names= []string {
  "config/test4.xml",
  "web/css/main.css",
  "web/images/wiki-create.png",
  "web/main.html",

}


var contents = []Content {
 { []byte("<config>test4\n</config>"),
    23,  1622214093, 0,
 }, 
 { []byte("\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x03K\xcaO\xa9T\xa8\xe6R\x00\x82\xa4\xc4\xe4\xec\xf4\xa2\xfc\xd2\xbc\x14+\x05\xe5\xd4\xd4Tk\x05\x05\xaeZ\xae\x02\xa8lr~N~\x11P" +
"\xc2(\x11\x04\xa1rE\xa9h\xb2&\x89 \x08\x91MB\x933H\x04A\x88\x5cJf\x19\xba\xb9\x06 \x08\x91\x05\x00\x89\xfd'.\x95\x00\x00\x00"),
    94,  1622212604, 0,
 }, 
 { []byte("\x89PNG\x0d\n\x1a\n\x00\x00\x00\x0dIHDR\x00\x00\x00@\x00\x00\x00@\x08\x06\x00\x00\x00\xaaiq\xde\x00\x00\x00\x04sBIT\x08\x08\x08\x08|\x08d\x88\x00\x00" +
"\x00\x09pHYs\x00\x00\x16%\x00\x00\x16%\x01IR$\xf0\x00\x00\x0dpIDATx\x9c\xed[{lT\xd7\x99\xff\x9d{\xef\xcc\x9d\x87\x1f3\xe3\x19\x8f\xc1\x18c" +
"\x1b\xb0\x8b\x09\x04Lx6\x81\x12P\x09\xd9\x10\x88\x1a\xab\xc9\x16'\xa1@VQ\xb2h\x95m\xb7\xea&K\xb2\x1b%E\x956\xed\xae\x886\x09\n\x0b\x0d\xe9\x96\xad\xd2*" +
"J\xd2\x884\x09}\x01-iXH\x0c\xb11~\xe0\x19{\xc6\x9e\x17\x9e;\xaf\xfb:\xfb\xc7\x8c\xed\x19{l\xdf;\xb6\x93V\xe2'\x8dd\xdd{\xee9\xdf\xef;\xdf\xf9" +
"\x9d\xef|\xf7\x9a@#\x0e\x1e<\xc8\xd4WW\xdf\x09J\xef\x06\xd0\x00B\xcc\xa0\xd4\x0f\xe0\x8f\x8a\xa2\xfcl\xf7\xfe\xfd\x1e\xad}\xfd%\x81hi\xf4\xd3\xd7^\xbb]\x02}" +
"\x85\x03iH1\x8a\xea)J\x90\x14\xab\x12G\xc2\xa0T$\xcd\xac\n\xaa\x10\x90\xff2\xc4\xe3\xdfk~\xfcqa\xb6\x8d\x9eIL\xe9\x807\x8e\x1c\xf9;\xca\x90\x97\xbaJ" +
"b\xea\xcf\x17z\xd9O\x1d\x11(\xcc\xe8}\x9bh\xc0\x9d\xbd\xe5\xd8\xd19We@:x\xc2nn~\xf8a\xefl\x1a=\x93\x98\xd4\x01'\x8e\x1e\xbd\x9fPz\xf2\xd4|" +
"?~\xb2\xb8;\x87\xf8XT\nf|\xff\xcf\x0dr\xb1d\xe8\x00cX\xd5\xd2\xd2\x12\x9bicg\x03\x13R:~\xfcx\xb9\n\xf5\xb5?\x96\x87\xe8\xb1\x86\x9e\x1c\xf2\xef" +
";\x0f\xa0\xb3\xe2\xb9\x9c\xf6\xde\xa2\x04^X\xd5\xc6\x81\xd2\xc5\xac,\xff\xdb\xacY<\xc3\xe0&\xbc!\xcb\xff \x13j9\xb6\xa4\x9bP\xd0\x9c{fb\x84\x85\x18\xc7=\xe3" +
"\xb1\xc6\xf1\xce\x82~\xe6\x9e\xce9\x7f\xff\xe0\xddw\xfbC\x81@T\xa7=\x14@\x0f\xec\xf6\x0f\xde{\xef\xbd\x14\x00\xbcy\xecX\x99\x94J\x1d\x07!\xd5\xa0\x94RB(\xa1" +
"T\xcd\xb4'\x00\x18J\x08\x07\x80%\xe9\xe7\x03\x94\xd2\x97\xbf\xb9\x7f\xffq-\x03\xe6\x8d\x00J)\x91\xa1\xb4\x9cw\x87\x99\x88Q\xd2\xc5\xe0\x83\xaaA0\x84a\x17-Y\xb2" +
"\x01\x80]\xe7\xcf\x01\x60\x05\x09\x87[\xb6.[f\x05\x00\x81\x90$%\xa4\x16@#\x08YJ\x80[@\xc8\xf2\xccoY\xe6Z\x03\x01\x16\x01X\x0c\x60=\x80W\x8f\x1e=" +
"j\xd2bo\xde\x08\xf8\xe9\x91#\xe5\x1c\xcb\xcem\xb3\xa5'p>\xeb\xc0b\xce=r\xdfF\xcc\xe0\xc1a\x0b\xff\x95\x91k\x1e%\x8c\xcfe\x1f\x02\xa6\x14\xc2&\x09\x1b6" +
"m\xda\xb6\xf7\x89'6\x02  \x04\xc8\xe8MFt\x08!\x04\x94R\xc2\xa4\xefA\x94\xa4d[k\xeb\xa7\xaf\xbc\xf8\xe2\xe9\x98 \x943\x16\xcb\xb7\xb6.[\xf6zKK" +
"K\xec\x8dW_\xdd\xc4Rz\n\x84,\xd3B\x8a\x10b4%\x93\x15\x00\xba\x0br\x80\xca\xb2\xe5\x0c\x80\x08/\x02\x00\xee3\xaf\xc0\x8b\xa5\xf7\x8fk\xf7\xbe\xf3\xc0\xc8\xdf\x87" +
"c\xa7\xf1x\xe4\x7f\x00\x00a^\x04(5\x98-\x16\x83\x16\x83\x01\xc0h2Y\x9a\xd6\xae\xdd\xf8\xc2\xe1\xc3\x8b~\xf0\xf4\xd3\xc7\xfa\xae_\xc7\xb0\x13\x1e\xdc\xb7\xcf\x7f\xe2\xa5" +
"\x976q\x06\xc3\xaf\x00\xac\xd1\xd2\x1f\xcbq\x13.\xefl\xe4]\x02\xaa\xa2$\x00\x80WX\x00\xc0\xafSW\xb0/\xf2\xfa\xc8\xef\x9a<\x88(M\xe6\x5c{#~~\xe4y" +
"\xb3\xc2\x82aY-\xe3\x8f\x83\xc3\xe9\x9c\xfb\xd4\x0b/\xec\x9dW]]D(uf\x9c\x60\xfd\xdb\xc7\x1e\x0b\xf3\x84l\x05\xa5\xa75uD\xa9\xa2\xa5Y^\x07$T\xb5G" +
"\x05MU\x09\x16\x00\xc0gR\x1f\x8e\xc4~?\xf2\xf3\xa9C\x88S1\xe7\xda\x19\xf1\x1a\x00\xc0\xa82(\x8f\xf1(*.\xd6dg>\x94\xd8l\xae\x7fz\xee\xb9G*\xe6" +
"\xce5g;a\xe7\xde\xbdQ\x12\x8dn\x07\xa5\xefN\xd9\xc9t\x1c\xf0\xe8\xa3\x8fJ\x00~\xb5\xd6\xe7P\x18\xaa)Y\x1cA\xd3\x80\x1d,%p\xba\xddS7\x9e\x04v\x87" +
"\xc3\xfd\xbd\xe7\x9f\x7f\xd8\xe1r\xf1\xd9Nh~\xf2\xc9\x04\xb1\xd9vQ\xe0\xe7\x00P\x02&/\x09\x96\x105\xcf\xe5q\x980\x0f\x60A\xfe\xdd\x99\xe4\xd9\xcd\x1e\x97f\xa3\x0d" +
"*\x83\xfb;\xabP\xec\xb0\xc1f\xb7k~n\x228]\xae\xca\xa7\x0f\x1dz\xc8VV\x96\xeb\x84\xe6f\xb1r\x8ee\xbf)!\xf8\x1ad\x16\xcbe\x0eV5w\xa2R\x0c" +
"3=\x07<\xb0g\xcf\xef(\xa1\xc7v\x7f^\xad.\x8e\xe4\x86\xf3\xdf\x04\x0f\xa3\xd1\xfflnG\x94\x60ok\x0d*\xe2&,]v\xabf\x92S\xc1\xe5v\xcf\xff\x97C" +
"\x87ZJ\x1c\x0e\xe3\xb0\x13^\xf9\xe1?\xd7H\x09\xe1\xc3\x90\xe0\xa9x\xeb\x93\xdf\xca\xe4\x86\x80\x86H\x12\x169\x8b\xb3\xc6%0i|\xbf\xf5\xf2\xcb\x96\x1b<wJ\xa5t" +
"\xdd\x89\xfa\xeb\xcc\x07U\x03P\x08\x1d\xd7\xae<\xc1c\xef\x95Z,\x0d\x96\xa0n\xd1b8\x9cN\x9d4GQb\xb3\x81\xcd#\xe0\x03\xfd\xfd=\xcf|\xf7\xbb\xc7 I\xe4" +
"\xa1\xdd\xf7\xec\x09x\xbb*[[\xaf\xa2\xbd\xad\x13\xcb\xca\x17\xc8\x0f\xae\xb8\x83S\x19\x06W*m\x90Y\x06\x04(k\xde\xb7/4\xd5xS.\xf0\x93'O\x9a%a\xe8" +
"?\x09\x98o\x87\x8d\xa2z\xbe\x22\xcc\xf4\x16\xc5!\xb2*lI\x03\x1a\x22%\xb8u\xb0\x14\nU!\xde\x88B\x16\xc5\x02\xa9\xa7a-.\xc6\xe6{\xef\x05o\x1a\x9f\xc7\x0c" +
"\xf4\xf5]\xef\xbc\xf8\x072\xd8\xdbQ\xd5\xd6\xfa9:;\xba\xc1P\x02\x97\xd3\x85\xaf\xde\xb2J^\x9a0r1\xab\x19]\xb5n\x90\xd2R\xbe\xb9\xb9yJc4+\xdc\x81" +
"={\xfe\xb5\xae\xbe~\xa7\xc3\xedjd\xd4\xd1\xa5#+\x12\xe4D\nR2\x09\xaajZvS\xa2\xe9\xf6\xdbQS_\x9fsMUdt|\xfc\x11\xae\x5c\xfa\x04\x9f]" +
"\xba\x8c\xce\xf6NX\x0c\x1c\xca\xca\xddX\xb9~-\x96\xde\xb6\n\x03g.JU\x1d!C\xe7\xca\x85\xe1]\x07\x9eph\x19KS\xb2\x00\x00\x9f\xb7\xb6v\xb6\xb5\xb6\xfe\xec" +
"\xe9C\x87v-X\xb8\xb0IQ\x14$b1\xfc\xe6\xed\xb7\xa1\xce\x10q\x00\x60\x18\x06\x0eW\xae\xf0\xaa\xb2\x84k\x17~\x8b\xf6\x8b\x7fBGk;\xda\xdb:\x60\xa1\x80\xd3" +
"Q\x86\xe5\xeb\xd6\xa2~\xf9r\x04\xfc~\x84\x8a\x19\xc3\x156r\xa9\xcabz[\xebx\x9a\x1d\x90\x8d\xe1\xf0\xb4X\xad\xf8\xda\x8e\x1d\xf0\xf5\xf6\x82\xd2\xf1\xda\xa0\x17\x84\x10T" +
"TU\xa1\xd4\x915yj\x0c\xfd\x1d\x9f\xa2\xed\xc29\x5cnm\xc7\xb5\xf6.\x94\xf2&8KKq\xdb\xa6;\xd0\xd0\xd4\x84A\xbf\x1f7\xc2\x11\x84\x83\x81O\x04S|\xe3" +
"7\xf7\xed\xd3\x5c\x94\xd1\xec\x00\x06H\xcb\x1f\xc9]5v\xa7\x13\xf6i\x88\xded 4\x0ek\xf0\x19\xf0\x91Att[\xd1y\xb5\x0b\x06\xc2\xc0QV\x86[\xd6\xaf\xc3\xd2" +
"5\xab\xe1\xf3z1\xe8\xf7#8\x18\xe8M\xa9\xd2\xfd\xcf\xbe\xf4\xbf\xba*R\x93\x948\xbe\x5c\x10\x1a\x875p\x10\xacp\x05\x0b,~l[\xd4\x8bRk\x11\x9cn7V" +
"~u=\x96\xaeZ\x89~o\x1f\xfa\xbd^\x5c\xeb\xb8v\xfd\xa3\xdf\xfd\xf9\xe8\xe9\xd3\x17\xb6n\xdc\xb8\xb1H\xcf8\x9a#\x80R\x9a>\xd5\x91\xd1\x10\xa0\x94\xe2\xda\xe5\xcb3" +
"\xba\x04\xe6\xcc\x9f\x8f\xba\x86jXC\xcf\x80\x15\xae@\x89*HxR\xa8#\x02\x1e\xb9\xc3\x8e\xbe\xb2;1\xaf~1\x22\x81\x00B\xc1 xK\x11\x1ex\xec;|\xef\xc1" +
"g\xb8\xfe\xde^\xa79\x99\xdc\xbdc\xfd\xfa\xd7\xdf:sFS-B\xbb\x06\xb0,0F\xec\xfc\x1e\x0f\xfe\xef\xecY},\xa7@\xc8\xd7\x8d%\xb6+\x60S\xd7\xa0\x08\n" +
"\x12^\x11\xc9\x01\x09\xa0@]Q\x18v\xe6\x0c\xfa\x87*\x10\n\x85\x00\xc2\xe2k;\x1f\x84\x917\xb9\xbf\xff\xfc\xf3\xdf>\xf4\xd4SG===\x90T\xf5\x1e\x00oh\x19" +
"O\xfb\x12\xa0\xe9C\x01\x93\xa5\x01\xa9dR'\xbd\xc9a\xe4dl[}\x16\xa6l\xf2~\x11\xc3\x05)\xceHP\xc9u\x83o\xfd\x09\x08\x086\xa7\xc9\x03H\x9f\x1d\xfe\xf1" +
"\xd9gws\x1cG(\xa55MMM\x9a\x8e\xe2\xd3\xd2\x80y55\xa8\xaa\xad\x05g0\x80\xe5\xb8i\xfdL&\xe0\xae5\xe7\xe02\x07\xa1\xc42\xe4}\xb9\xe4\x0d<\x83" +
"@_\n\xc5\x5c)\xee\xdc\xf5\x00\x8c&s\x8e=N\x97\xab\xf2\xd6U\xab\xca\x01\xc0\xc5\xb2\xe6q\x06\xe7\x81v\x0d\x18\xd5\xff\x91\x10\x609\x0ek6o\xd6\xda\xc5\x84\x18V" +
"{V\x08@\xcd&?l\xe40\xf9\xfe\x14\x04\xbe\x11s\x1f\xf9\x0f0FK\xde\xbe\x9cnw1\x00\xbf\xac(\x9a\x92<\xcd\x110[\xdb\xc5(\xf9\xcbi\xf2\x1e\x11\xc9\xfe" +
"\x0cy:J>\xd8/\x22f\x9c\x9c<\x000,\xab\xeb\xfc\xae]\x043\x1a@\xc6\xe4\x01\xb2,#\x12\x08\x14\xb4\x0b0H\xa2\x92\xfe\x18\xac\xd8\x015\x9e\x9e\xf9D>\xf2" +
">\x11\x02\xbf\x04s\x1e\x9e\x9c<\x00\xdd\xe9xA\x99\xe00\x12\xf18>\xfc\xe5/\x91\x88\xc7u?k\xe4dl\xbb\xed,xk0M\xde#\x22\xd1\x97E\xde\x90&\x1f" +
"\xf2\x89\x10\x8c\xda\xc8g\x83cYM3\xa2K\x03\x00\xe4\x84\x80\xaf\xb7\xb7p\xf2\xab\xcf\xc2e\x09BM\xa8\xe9\x99\x1fK\xde\xc4 \xe4\x17!\xf0\x8d\x98\xf3\xd0\x8f5\x93\xa7" +
"$\xcfy}\x12Lki;\x5c.\xdd\xc5O#'\xe3\xeb\xab\xcf\x8d\x92\xf7\x88Hx\xc7\x93\x0f\x0f\x88\x10\x8c\x8d\xa8\xd0A\xbe\x10h\x8f\x00B\x08\xa14'\x0f(u8" +
"\xb0e\xe7N\xf8\xbd^M\x1a\xc0\x92\x14\xeaK\xfe\x1bV5\x00\x9a\xcc\xcc\xbc7\x95\x19\x00\x603a\x1f\x1e\x10\x115\x14H~\xb64\x60\xe404\x06%v;J4\xd4" +
"\xff\x08\x8d\xa7\xd3\xdbh\xf7\x08\xf9d\xd6\xcc\xb3\x06\x02#\xcf <8\x0d\xf2\x05@\xb7\x08R\x1dE\x94a\x8c\x90\x17.\x83\xa623\xef\x11\xd3Q\x935\xf3\x91\x80\x84\xa8" +
"\xb1\x11\x15-\x85\x93\xa7:\xb7#}\x87\xa1\xf1\xd7\xd0v\xe9\x12\xfc\x9e\x89?\x0e\xe1X\x11\xeb\x16\xbe\x0b\xd6\xd0\x9f\x15\xf6\xe3\xc9\xdf\x08H\xf0D+0\xd0\xb0\x1dW\xdf\xff" +
"H\x93M\x84\x10T\xd5\xd5\x8d\xab\x1e\xe9\x81\xee\x08\xc8\xce\x03\xfa{{\xf1\xd9\xf9\xf3\x13\xb65p2\xb6\xad>\x07\xbb!0:\xf3^\x11T\xcd\x90\xe72\xe4\x83\x12\xbaB" +
"\x0e\x5cum\x81:\x10\xd4e\xcf@_\x1f\xecN'lee\x000\xa2EF\x8e\xd3\x14\x09zv\x81q\x110Y\x01t\x98|\xb95\x00\x88t\x94\xbc\x92E\xde\xc4\x60" +
"($\xa1+\xe8\xc0U\xd7v\xa8\x8c\xe6W\x89\xb9v\xc8rA\xcf\x01\x85$BYKa^M\x0d\xfc^/|\x1eON\x06f\xe0dlY\xf1\x078\xcd\x19\xf2\x9e\x14" +
"\x92c\xc9\xf3i\xf2\xdd!'z\xe6\xdd\x0b\xae\x00\xf2\x0c\xc3\x60\xfe\xa2E(+/\xcf2o\x165\x60l\x1a\xcc\xb0,n\xdb\xb81\xe7\x1a\xa1\x09XC\x07\xc1\n\x83\x99" +
"\x99O!\xe1\x15\xa1\xca\xb9\xe4\xa3\x11\x09\x82\xa1\x11_y\xf2Gh\xfc\x02\xd4~\x22h_\x02\x99\x84\x870\x13?2J\xfe\xf2h\xd8{\xf2\x93\x8fr\x8dp}\xebG3" +
"\xbe\xd5\xe9\xadPk\xcf\x03(\xcd\x9b\x07\xa4\x92I\x84\x03\x0104\x81*\xee0X\xb1#M\xbeO\x9c\x60\xe6e\x04\x95\x85\xe0\xb6|\x07\x03\xfe \x00}\xa2\x97\x0d\xc20" +
"p\xba\xddy\xdf$\xdd0\x18f\xf6,0\x8c\xec\xed0.\x08\xf8\xf5/~\x011\x95\xc2\xda\x86>\x88\x8e>H\x12\x07.\x12K\xcf\xbc\xa4\x02\x14\x602\xe4\x85\x1b2\xba" +
"\x82v\xb4\x95\xad\x85\xfa\xe1o\xf4\x0e\x9d\x17\xd6\xe2bl\xbd\xef>p\x86\xc2\x04T\xf3\x12\xc8\x97\x00\xf9<\x1e\x88\xa9t*kP\xab\x11\x08\xed\x80?\xb0&=\xf3\xc3\xe4" +
"\xd9t\x86'\x0c\xc9\xe8\x0c\xd8\xd1VV\xb8\xda\xe7C,\x1aE\xc0\xe7\xcb\xb6svDp\x18\xd9g\x01WE\x05X\x8e\x83\x85S\xc11,\x14E\x81\x22\xf2PRj\xfa" +
"\xf3-6\xbd\xd5\xc5\x86dt\x06\x1ch+\xbbkF\xc9\x03\x80\x91\xe7\xa7\xf5^bZ\xf5\x80b\x9b\x0d[w\xedB\xa4\xfdO\x88\x05; \xa5D\x88\xa2\x8c!n=\xc4" +
"\xc4\x0d\xd4\xb0\xed\x90\xa2\x22\x06\xa4\x050o\x7f\x12\xb7\x1a4}\xb8\xa5\x19\x84aP1o\x1ex\xf3h\xf9ox;6\x86\xc33\x5c\x0f\xa0t\x5cM\x10\x00\x8aJK\x11" +
"\x1c\xf2!\x1a\x89@J\xa5 \x8a\x22$Q\x86$q\xa8\xa6\x12\x86LK0o\xf7\xcc\xab\xfdLa\xda\xa7A)>\x84\xc1\x9ev\x88b*\x13\x01iM\xe0\xcd\x16\x0c:" +
"\xb7\xa3\xe6\x9e\x03_(\xf9YK\x84\xb20\x1a\x01\x94\xe2\xc2\xb9\xb3h\xeb\x8b\xc2\xcd\xa7@83\x88\xad\x1a\xa4\xb8\x02\x92\xc9\x86n\x00\xdd\xef\x9e*\x60\x08\x8d\x860\x0c\xaa" +
"\xea\xea\xb0\xa8\xb1\xb1\xe0>t\x15D0\xc6\xb9\xde\x9e\x1e\xb4wy\xf0\xce\x85^lZ\xbd\n\xce\xe2\xcckmA\x06\x84@\xc1F\xe9Ah\x60\x00N\xb7\xbb\x60!\xd4\x1f" +
"\x01Y\x99\xa0\xaa(0Q\x8ao\xdcuWA\x83\xcf\x14\xf2U\x82\xf9YH\x84\xc6\xe5\x01\x9555\xa8\xf5\xf9\xe0\xf7xf\xe4\xe5\xa8^0,\x8b\xaa\xdaZ\xd8\xb3>\xa8" +
" :\x8b\xa2\xd3\xda\x06\x19\x86\xc1\xca\x0d\x1b\xa6\xd3\xc5\x97\x0e\xed\x99\x60&\x05\x96R\x99\xd4\xef/\x14\x82 \xe8\xfaJK\xb3\x03H\xe6\xcb\xcb\xd6\x8b\x17\xaf\xea5\xea\x8b\x82" +
"(\x8a\xc9\xd6\x0b\x17\x06\x01@v\xb94}'\xa8}\x17PU?!\xa4\xf6\xcd\x13'>s\xb8\x5c\xc5\xb7\xacX\xd1d6\x9b\x8b\x192q\x8d\x94\x8e- \xe4\x81\x96&" +
"S5P\x15E\x1d\x8aD\x06\xdfy\xf3\xcd\x0f\xe2\xb1\x98\x0c 4\xfc\x0f\x17\xd3\xee|\x18_oj\x9aC8\xae\x05\xd3\xd4\x8d\xd9\x06!\x84*\xc0\xc9S\xe7\xceuhj" +
"\xaf\xa7\xf3\xad\xeb\xd6\x95\xb3\x94n\xa0@9S@y|\xb6A\x09\x09\xaa\x92t\xee\xd4\xc7\x1f\xf7~\xd9\xb6\xdc\xc4M\xdc\xc4M\xdc\xc4M\xfc\x15\xe0\xff\x01K\x1e\n\xf8\x9b" +
"'\xa1Z\x00\x00\x00\x00IEND\xaeB\x60\x82"),
    3534,  1622210880, 0,
 }, 
 { []byte("<!DOCTYPE html>\n<html xmlns=\x22http://www.w3.org/1999" +
"/xhtml\x22 lang=\x22en\x22>\n  <head>\n    <meta http-equiv=\x22C" +
"ontent-Type\x22 content=\x22text/html; charset=UTF-8\x22 />\n" +
"    <title>Test 3</title>\n    <link media=\x22screen\x22 " +
"type=\x22text/css\x22 rel=\x22stylesheet\x22 href=\x22css/main.css" +
"\x22 />\n  </head>\n  <body>\n    <p>Hello Test 3</p>\n   " +
" <script src=\x22js/main.js\x22></script>\n  </body>\n</htm" +
"l>\n"),
    360,  1622210810, 0,
 }, 
}

// Returns the data stream with the given name or null.
func Get_content(name string) (*Content) {
    i := sort.Search(4, func(i int) bool {
        return strings.Compare(names[i], name) >= 0
    })
    if i < 4 && strings.Compare(names[i], name) == 0 {
        return &contents[i]
    }
    return nil
}

