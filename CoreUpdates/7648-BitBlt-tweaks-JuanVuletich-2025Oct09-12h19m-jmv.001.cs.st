'From Cuis7.5 [latest update: #7647] on 9 October 2025 at 12:20:09 pm'!

!BitBlt methodsFor: 'private' stamp: 'jmv 10/9/2025 12:19:50'!
roundVariables

	| maxVal minVal |
	'-----------' print.
	'BitBlt >> copyBits failed. Will retry with parameters rounded. Requested parameters are:' print.
	{'dest, source, halftone, rule:' . destForm . sourceForm . halftoneForm . combinationRule } print.
	{'dest, extent, source, clipOrigin, clipExtent'. destX@destY. width@height. sourceX@sourceY. clipX@clipY. clipWidth@clipHeight } print.
	{'colorMap'. colorMap } print.
	
	maxVal := SmallInteger maxVal.
	minVal := SmallInteger minVal.
	"The BitBltPlugin was written assuming 31 bit SmallInteger. It protects itself against non-SmallInteger
	arguments, but not against too large SmallIntegers for its int32 arguments.
	These two lines are not a real protection against that."
	minVal := ((1 bitShift: 29) negated bitShift: 1).
	maxVal :=  -1 - minVal.
	
	destX := destX asInteger min: maxVal max: minVal.
	destY := destY asInteger min: maxVal max: minVal.
	width := width asInteger min: maxVal max: minVal.
	height := height asInteger min: maxVal max: minVal.
	sourceX := sourceX asInteger min: maxVal max: minVal.
	sourceY := sourceY asInteger min: maxVal max: minVal.
	clipX := clipX asInteger min: maxVal max: minVal.
	clipY := clipY asInteger min: maxVal max: minVal.
	clipWidth := clipWidth asInteger min: maxVal max: minVal.
	clipHeight := clipHeight asInteger min: maxVal max: minVal.
! !

