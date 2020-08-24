/*

*/

var _Float_fromBytes = F2(function(hi,lo)
{
    // hi, lo : Int(0..0xFFFFFFFF)
    var buffer = new ArrayBuffer(8);
    var view = new DataView(buffer);
    view.setUint32(0, hi);
    view.setUint32(1, lo);
    return view.getFloat64(0);
});

