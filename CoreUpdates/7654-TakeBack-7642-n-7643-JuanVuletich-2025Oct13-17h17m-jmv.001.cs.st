'From Cuis7.5 [latest update: #7652] on 13 October 2025 at 5:22:13 pm'!
!Form methodsFor: 'pixel access' stamp: 'jmv 5/12/2016 18:19'!
primPixelValueAtX: x y: y
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth.
	Typical clients use colorAt: to get a Color"

	<primitive: 'primitivePixelValueAt' module:'BitBltPlugin'>
	^(BitBlt bitPeekerFromForm: self) pixelAt: x@y! !


!BitBlt methodsFor: 'copying' stamp: 'jmv 5/18/2021 14:37:40'!
copyBits
	"Primitive. Perform the movement of bits from the source form to the 
	destination form. Fail if any variables are not of the right type (Integer, 
	Float, or Form) or if the combination rule is not implemented. 
	In addition to the original 16 combination rules, this BitBlt supports
	16	fail (to simulate paint)
	17	fail (to simulate mask)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord
	21	rgbSub: sourceWord with: destinationWord
	22	rgbDiff: sourceWord with: destinationWord
	23	tallyIntoMap: destinationWord
	24	alphaBlend: sourceWord with: destinationWord
	25	pixPaint: sourceWord with: destinationWord
	26	pixMask: sourceWord with: destinationWord
	27	rgbMax: sourceWord with: destinationWord
	28	rgbMin: sourceWord with: destinationWord
	29	rgbMin: sourceWord bitInvert32 with: destinationWord
"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"No alpha specified -- re-run with alpha = 1.0"
	(combinationRule >= 30 and: [combinationRule <= 31]) ifTrue: [
		^ self copyBitsTranslucent: 255].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [^ self oldPaintBits].
	combinationRule = Form oldErase1bitShape ifTrue: [^ self oldErase1bitShapeBits].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and:[colorMap isColormap]) ifTrue:[
		colorMap := colorMap colors.
		^self copyBits].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue:[self roundVariables. ^self copyBitsAgain].

	'Bad BitBlt argument (Maybe a Float or Fraction?); will retry rounding.' print.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain! !

!BitBlt methodsFor: 'copying' stamp: 'jmv 1/21/2015 23:52'!
copyBitsTranslucent: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"! !

!BitBlt methodsFor: 'line drawing' stamp: 'ar 2/2/2001 15:09'!
drawLoopX: xDelta Y: yDelta 
	"Primitive. Implements the Bresenham plotting algorithm (IBM Systems
	Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and
	maintains a potential, P. When P's sign changes, it is time to move in
	the minor direction as well. This particular version does not write the
	first and last points, so that these can be called for as needed in client code.
	Optional. See Object documentation whatIsAPrimitive."
	| dx dy px py P |
	<primitive: 'primitiveDrawLoop' module: 'BitBltPlugin'>
	dx := xDelta sign.
	dy := yDelta sign.
	px := yDelta abs.
	py := xDelta abs.
	"self copyBits."
	py > px
		ifTrue: 
			["more horizontal"
			P := py // 2.
			1 to: py do: 
				[:i |
				destX := destX + dx.
				(P := P - px) < 0 ifTrue: 
						[destY := destY + dy.
						P := P + py].
				i < py ifTrue: [self copyBits]]]
		ifFalse: 
			["more vertical"
			P := px // 2.
			1 to: px do:
				[:i |
				destY := destY + dy.
				(P := P - py) < 0 ifTrue: 
						[destX := destX + dx.
						P := P + px].
				i < px ifTrue: [self copyBits]]]! !

!BitBlt methodsFor: 'private' stamp: 'jmv 8/16/2021 19:54:48'!
copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	'#copyBitsAgain failed. Requested BitBlt operation not performed. Call stack follows:' print.
	thisContext printStack: 15.! !


!WarpBlt methodsFor: 'primitives' stamp: 'jmv 1/21/2015 23:56'!
warpBitsSmoothing: n sourceMap: sourceMap
	| deltaP12 deltaP43 pA pB deltaPAB sp fixedPtOne picker poker pix nSteps |
	<primitive: 'primitiveWarpBits' module: 'BitBltPlugin'>

	(width < 1) | (height < 1) ifTrue: [^ self].
	fixedPtOne := 16384.  "1.0 in fixed-pt representation"
	n > 1 ifTrue:
		[(destForm depth < 16 and: [colorMap == nil])
			ifTrue: ["color map is required to smooth non-RGB dest"
					^ self primitiveFail].
		pix := Array new: n*n].

	nSteps := height-1 max: 1.
	deltaP12 := (self deltaFrom: p1x to: p2x nSteps: nSteps)
			@ (self deltaFrom: p1y to: p2y nSteps: nSteps).
	pA := (self startFrom: p1x to: p2x offset: nSteps*deltaP12 x)
		@ (self startFrom: p1y to: p2y offset: nSteps*deltaP12 y).
	deltaP43 := (self deltaFrom: p4x to: p3x nSteps: nSteps)
			@ (self deltaFrom: p4y to: p3y nSteps: nSteps).
	pB := (self startFrom: p4x to: p3x offset: nSteps*deltaP43 x)
		@ (self startFrom: p4y to: p3y offset: nSteps*deltaP43 y).

	picker := BitBlt bitPeekerFromForm: sourceForm.
	poker := BitBlt bitPokerToForm: destForm.
	poker clipRect: self clipRect.
	nSteps := width-1 max: 1.
	destY to: destY+height-1 do:
		[:y |
		deltaPAB := (self deltaFrom: pA x to: pB x nSteps: nSteps)
				@ (self deltaFrom: pA y to: pB y nSteps: nSteps).
		sp := (self startFrom: pA x to: pB x offset: nSteps*deltaPAB x)
			@ (self startFrom: pA y to: pB y offset: nSteps*deltaPAB x).
		destX to: destX+width-1 do:
			[:x | 
			n = 1
			ifTrue:
				[poker pixelAt: x@y
						put: (picker pixelAt: sp // fixedPtOne asPoint)]
			ifFalse:
				[0 to: n-1 do:
					[:dx | 0 to: n-1 do:
						[:dy |
						pix at: dx*n+dy+1 put:
								(picker pixelAt: sp
									+ (deltaPAB*dx//n)
									+ (deltaP12*dy//n)
										// fixedPtOne asPoint)]].
				poker pixelAt: x@y put: (self mixPix: pix
										sourceMap: sourceMap
										destMap: colorMap)].
			sp := sp + deltaPAB].
		pA := pA + deltaP12.
		pB := pB + deltaP43]! !

!methodRemoval: WarpBlt #primitiveWarpBitsSmoothing:sourceMap: stamp: 'jmv 10/13/2025 17:20:46'!
WarpBlt removeSelector: #primitiveWarpBitsSmoothing:sourceMap:!
!methodRemoval: BitBlt class #accessProtect stamp: 'jmv 10/13/2025 17:20:54'!
BitBlt class removeSelector: #accessProtect!
!methodRemoval: BitBlt #primitiveCopyBits stamp: 'jmv 10/13/2025 17:20:40'!
BitBlt removeSelector: #primitiveCopyBits!
!methodRemoval: BitBlt #primitiveDrawLoopX:Y: stamp: 'jmv 10/13/2025 17:20:10'!
BitBlt removeSelector: #primitiveDrawLoopX:Y:!
!methodRemoval: BitBlt #primitiveCopyBits: stamp: 'jmv 10/13/2025 17:20:00'!
BitBlt removeSelector: #primitiveCopyBits:!
!methodRemoval: Form #primitivePixelValueAt stamp: 'jmv 10/13/2025 17:20:26'!
Form removeSelector: #primitivePixelValueAt!
!classDefinition: #BitBlt category: #'Graphics-Primitives'!
Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight colorMap'
	classVariableNames: 'CachedFontColorMaps ColorConvertingMaps'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
