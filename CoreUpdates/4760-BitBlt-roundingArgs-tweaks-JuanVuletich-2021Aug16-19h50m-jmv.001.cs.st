'From Cuis 5.0 [latest update: #4759] on 16 August 2021 at 7:59:21 pm'!

!BitBlt methodsFor: 'private' stamp: 'jmv 8/16/2021 19:54:48'!
copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	'#copyBitsAgain failed. Requested BitBlt operation not performed. Call stack follows:' print.
	thisContext printStack: 15.! !

!BitBlt methodsFor: 'private' stamp: 'jmv 8/16/2021 19:55:03'!
roundVariables

	| maxVal minVal |
	'-----------' print.
	'BitBlt >> copyBits failed. Will retry with parameters rounded. Requested parameters are:' print.
	{'dest, source, halftone, rule:' . destForm . sourceForm . halftoneForm . combinationRule } print.
	{'dest, extent, source, clipOrigin, clipExtent'. destX@destY. width@height. sourceX@sourceY. clipX@clipY. clipWidth@clipHeight } print.
	{'colorMap'. colorMap } print.
	maxVal _ SmallInteger maxVal.
	minVal _ SmallInteger minVal.
	destX _ destX asInteger min: maxVal max: minVal.
	destY _ destY asInteger min: maxVal max: minVal.
	width _ width asInteger min: maxVal max: minVal.
	height _ height asInteger min: maxVal max: minVal.
	sourceX _ sourceX asInteger min: maxVal max: minVal.
	sourceY _ sourceY asInteger min: maxVal max: minVal.
	clipX _ clipX asInteger min: maxVal max: minVal.
	clipY _ clipY asInteger min: maxVal max: minVal.
	clipWidth _ clipWidth asInteger min: maxVal max: minVal.
	clipHeight _ clipHeight asInteger min: maxVal max: minVal.
! !

