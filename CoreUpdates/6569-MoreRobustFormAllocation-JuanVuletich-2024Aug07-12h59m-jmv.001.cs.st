'From Cuis7.1 [latest update: #6568] on 7 August 2024 at 1:09:05 pm'!

!Form methodsFor: 'accessing' stamp: 'jmv 8/7/2024 10:45:39'!
bitsSizeForWidth: aWidth height: aHeight depth: aDepth

	^ (self wordsPerLine: aWidth depth: aDepth) * aHeight! !

!Form methodsFor: 'accessing' stamp: 'jmv 8/7/2024 10:38:53'!
pixelsPerWord: aDepth
	^32 // aDepth! !

!Form methodsFor: 'accessing' stamp: 'jmv 8/7/2024 10:42:37'!
wordsPerLine: aWidth depth: aDepth
	| pixPerWord |
	pixPerWord := self pixelsPerWord: aDepth.
	^ aWidth + pixPerWord - 1 // pixPerWord! !


!Form methodsFor: 'accessing' stamp: 'jmv 8/7/2024 10:46:07'!
bitsSize

	^self bitsSizeForWidth: width height: height depth: self depth! !

!Form methodsFor: 'accessing' stamp: 'jmv 8/7/2024 10:46:56'!
pixelsPerWord
	^self pixelsPerWord: self depth! !

!Form methodsFor: 'accessing' stamp: 'jmv 8/7/2024 10:43:58'!
wordsPerLine

	^self wordsPerLine: width depth: self depth.
! !

!Form methodsFor: 'private' stamp: 'jmv 8/7/2024 13:08:37'!
setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	| newBits newDepth newHeight newWidth |

	"Find parameters and allocate new bits before modifying this instance.
	If, for whatever reason, allocation fails, an exception will be raised
	and this instance (if already initialized) will still be in a consistent state."
	newWidth := extent x asInteger max: 0.
	newHeight := extent y asInteger max: 0.
	newDepth := bitsPerPixel.
	newBits := Bitmap new: (self bitsSizeForWidth: newWidth height: newHeight depth: newDepth abs).

	"If we get to this point, allocation succeeded. If we are already initialized (bits notNil), then set
	width, height and depth before setting bits, only if bits will later shrink. This is to avoid an unlikely
	invalid access in some other process if this instance is used elsewhere. This could be a serious problem
	for instance for Display."
	(bits notNil and: [ 		newBits size < bits size ]) ifTrue: [
		width := newWidth.
		height := newHeight.
		depth := newDepth ].

	"All good. Assign the new state."
	bits := newBits.
	width := newWidth.
	height := newHeight.
	depth := newDepth.! !

