/*

*/

var reverseBitsMemo = {};

var _BitwiseExtra_reverseBits = F2(function(count, value)
{
    // This function is just a memoization wrapper around actuallyReverseBits.
    // Note: because rev(rev(n)) == n, we can memoize both directions at once:
    //   memo[n] = rev
    //   memo[rev] = n

    var result;

    reverseBitsMemo[count] = reverseBitsMemo[count] || {};

    if (count in reverseBitsMemo && value in reverseBitsMemo[count]) {
        result = reverseBitsMemo[count][value];
    } else {
        result = actuallyReverseBits(count, value);
        reverseBitsMemo[count][value] = result;
        reverseBitsMemo[count][result] = value;
    }

    return result;
});

var actuallyReverseBits = function(count, value) {
    var reversed = 0;
    while (value && count)
    {
       reversed <<= 1;
       reversed |= value & 1;
       value >>= 1;
       count--;
    }
    reversed <<= count;
    return reversed >>> 0;
};
