'From Cuis7.7 [latest update: #7965] on 29 May 2026 at 9:27:02 am'!

!Form methodsFor: 'private' stamp: 'jmv 29/May/2026 09:25:13'!
prepareForImageSave
	"Use little memory in saved image"

	bits unpin.
	self setExtent: `64@24` depth: self nativeDepth.
	bits unpin. "In case we ever change initialization to also pin."! !


!Form methodsFor: 'private' stamp: 'jmv 7/Aug/2024 13:08:37'!
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
	(bits notNil and: [ newBits size < bits size ]) ifTrue: [
		width := newWidth.
		height := newHeight.
		depth := newDepth ].

	"All good. Assign the new state."
	bits := newBits.
	width := newWidth.
	height := newHeight.
	depth := newDepth.! !


!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 29/May/2026 09:17:30'!
clearDeferredUpdatingDisplay

	DeferredUpdatingDisplay ifNotNil: [
		DeferredUpdatingDisplay prepareForImageSave.
		DeferredUpdatingDisplay := nil ].! !

!methodRemoval: DisplayScreen #prepareForImageSave stamp: 'jmv 29/May/2026 09:24:28'!
DisplayScreen removeSelector: #prepareForImageSave!
