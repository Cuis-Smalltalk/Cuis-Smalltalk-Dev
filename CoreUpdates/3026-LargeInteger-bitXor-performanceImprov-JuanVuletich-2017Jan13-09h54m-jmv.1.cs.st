'From Cuis 5.0 of 7 November 2016 [latest update: #3025] on 13 January 2017 at 9:55:29 am'!

!LargePositiveInteger methodsFor: 'bit manipulation' stamp: 'jmv 1/13/2017 09:39:07'!
bitXor: arg
	"Primitive 36 deals with only 64-bit values (up to 8 byte LargeIntegers).
	The inherited <primitive: 'primDigitBitXor' module:'LargeIntegers'> deals with 
	arbitrary sized large integers, but is much slower.
	This method gives a performance improvement for integers using 32 to 64 bits on 32 bit VMs,
	but only for 62 to 64 bits on 64 bits VMs.
	See http://forum.world.st/Integer-arithmetic-and-bit-operations-in-Squeak-and-Pharo-32bit-amp-64bit-tc4928994.html#none
	"

    <primitive:36>
    ^super bitXor: arg! !

