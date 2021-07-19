'From Cuis 5.0 [latest update: #4674] on 17 July 2021 at 8:34:32 pm'!

!BitBlt methodsFor: 'private' stamp: 'jmv 7/17/2021 20:31:38'!
copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	'#copyBitsAgain failed. Requested BitBlt operation not performed.' print! !

!BitBlt methodsFor: 'private' stamp: 'jmv 7/17/2021 20:34:09'!
roundVariables

	| maxVal minVal |
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

