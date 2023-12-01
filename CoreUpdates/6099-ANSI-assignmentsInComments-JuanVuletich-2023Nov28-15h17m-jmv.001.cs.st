'From Cuis 6.0 [latest update: #6098] on 28 November 2023 at 3:21:53 pm'!

!Object methodsFor: 'casing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
caseOf: aBlockAssociationCollection
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, report an error."

	^ self caseOf: aBlockAssociationCollection otherwise: [self caseError]

"| z | z := {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"| z | z := {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}"! !

!Object methodsFor: 'casing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
caseOf: aBlockAssociationCollection otherwise: aBlock
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, answer the result
	 of evaluating aBlock."

	aBlockAssociationCollection associationsDo:
		[:assoc | (assoc key value = self) ifTrue: [^assoc value value]].
	^ aBlock value

"| z | z := {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"| z | z := {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"! !


!VersionsBrowser methodsFor: 'menu' stamp: 'jmv 12/12/2014 15:54'!
compareToOtherVersion
	"Prompt the user for a reference version, then spawn a window 
	showing the diffs between the older and the newer of the current 
	version and the reference version as text."

	| change1 change2 s1 s2 differDesc diffWords targetChanges labels label1 |
	change1 := changeList at: listIndex ifAbsent: [ ^self ].
	label1 := list at: listIndex.

	labels := list reject: [ :any | any = label1 ].
	targetChanges := changeList reject: [ :any | any = change1 ].
	change2 := ((SelectionMenu
				labels: labels
				selections: targetChanges) startUpMenu) ifNil: [ ^self ].
	
	"compare earlier -> later"
	"change1 timeStamp < change2 timeStamp
		ifFalse: [ | temp | temp := change1. change1 := change2. change2 := temp ]."

	s1 := change1 string.
	s2 := change2 string.
	s1 = s2
		ifTrue: [^ self inform: 'Exact Match'].

	diffWords := self shouldDiffWords.
	differDesc := diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].

	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: s1 to: s2
			tryWords: diffWords
			prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [classOfMethod])))
		openLabel: 
			(('Comparison from {1} to {2}: ', differDesc, 
				(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])) 
					format: { change1 stamp. change2 stamp })! !


!FileList methodsFor: 'user interface' stamp: 'jmv 6/30/2022 15:56:20'!
toggleInitialDirectory
	"Toggle the initial directory setting of the currently selected directory."
	
	
"
	Preferences isInitialFileListDirectory: (directoryEntry := currentDirectorySelected item)::
		ifTrue: [
			Preferences removeInitialFileListDirectory: directoryEntry ]
		ifFalse: [
			Preferences addInitialFileListDirectory: directoryEntry ]
"! !


!FileList class methodsFor: 'file reader registration' stamp: 'pb 2/26/2020 12:43:21'!
itemsForFileEntry: aFileEntry
	"Answer a list of services appropriate for a file of the given name"
	| services suffix classList |
	"
	suffix := (FileIOAccessor default extensionFor: filename) asLowercase.
	"
	suffix := aFileEntry extension asLowercase.
	"Build the list dynamically for all implementers of the appropriate class method... registration no longer required"
	classList := (Smalltalk allClassesImplementing: #fileReaderServicesForFileEntry:suffix:)
		collect: [ :item |
			item class == Metaclass ifTrue: [ item soleInstance ]]
		thenSelect: [ :item |
			item notNil ].
	services := OrderedCollection new.
	classList do: [ :reader |
		services addAll:
			(reader
				fileReaderServicesForFileEntry: aFileEntry 
				suffix: suffix) ].
	services sort: [ :a :b | a sortOrder < b sortOrder ].
	^ services.! !


!Number methodsFor: 'arithmetic' stamp: 'jmv 5/12/2022 16:59:55'!
\\ divisor
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division)
	Answer a Number with the same sign as divisor. 
	9\\4 = 1
	-9\\4 = 3
	9\\ -4 = -3
	0.9\\0.4 = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - (self // divisor * divisor)

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 5/18/2020 15:07:30'!
mod: divisor
	"Modulo operation. Remainder of the integer division #div: (Euclidean division)
	Answer a Number that is never negative (it is positive or zero). 
	(9 mod: 4) = 1
	(-9 mod: 4) = 3
	(9 mod: -4) = 1
	(0.9 mod: 0.4) = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	And 'The Euclidean Definition of the Functions div and mod' by Raymond T. Boute, https://core.ac.uk/download/pdf/55698442.pdf
	"
	"Answer r such that: 
		for some integer q, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	^self \\ divisor abs

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: d ] color: Color green.
	g addFunction: [ :x | x div: d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: d ] color: Color green.
	g addFunction: [ :x | x div: d ] color: Color red.
	g openInWorld'
"
! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 5/18/2020 15:08:09'!
rem:  divisor
	"Modulo operation. Remainder of the integer division #quo: (division with truncation towards zero).
	Answer a Number with the same  sign as dividend (i.e.,  self).
	(9 rem: 4) = 1.
	(-9 rem: 4) = -1.
	(0.9 rem: 0.4) = 0.1.
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - ((self quo: divisor) * divisor)

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x rem: d ] color: Color green.
	g addFunction: [ :x | x quo: d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x rem: d ] color: Color green.
	g addFunction: [ :x | x quo: d ] color: Color red.
	g openInWorld'
"! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/18/2020 15:08:33'!
smoothIsAbsBelow: threshold
	"A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable.
	
	It is a 'smooth' version of an #isAbsBelow: function: x abs < threshold.
	Useful when we need to 'count' stuff, but still want continuous and differentiable stuff."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < Float halfPi ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothIsAbsBelow: Float halfPi ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	| scaled |
	scaled := self / threshold.
	^ scaled abs < 2 ifTrue: [ (scaled * Float halfPi) cos + 1.0 * 0.5 ] ifFalse: [ 0 ]! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/18/2020 15:08:52'!
smoothStep
	"A Function that is
		x <= -1 		-> 		1
		-1 < x < 1 	->			smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	self isNaN ifTrue: [ ^self ].
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5! !


!Integer methodsFor: 'benchmarks' stamp: 'jm 11/20/1998 07:06'!
benchFib  "Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t |
	  t := Time millisecondsToRun: [r := 26 benchFib].
	  (r * 1000) // t"
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1] 
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1]
! !

!Integer methodsFor: 'comparing' stamp: 'jmv 10/18/2018 09:48:12'!
hash
	"Hash is reimplemented because = is implemented.
	| s |
	s := (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	"Can only happen in 64 bits images...
	See Float>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].

	"In 32 bit image it will always go this way"
	^self hashMultiply! !

!Integer methodsFor: 'private' stamp: 'jmv 6/25/2019 17:58:27'!
digitDiv: arg neg: ng 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #digitDiv:neg: arguments: {arg.ng}].
	"TFEI added this line"
	l := self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d := 8 - arg lastDigit highBitOfMagnitude.
	div := arg digitLshift: d.
	div := div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem := self digitLshift: d.
	rem digitLength = self digitLength ifTrue: [rem := rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo := Integer new: l neg: ng.
	dl := div digitLength - 1.
	"Last actual byte of data"
	ql := l.
	dh := div digitAt: dl.
	dnh := dl = 1
				ifTrue: [0]
				ifFalse: [div digitAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j := rem digitLength + 1 - k.
		"r1 := rem digitAt: j."
		(rem digitAt: j)
			= dh
			ifTrue: [qhi := qlo := 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 := (rem digitAt: j - 1)."
				t := ((rem digitAt: j)
							bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitShift: -4).
				qhi := t // dh.
				t := (t \\ dh bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitAnd: 15).
				qlo := t // dh.
				t := t \\ dh.
				"Next compute (hi,lo) := q*dnh"
				hi := qhi * dnh.
				lo := qlo * dnh + ((hi bitAnd: 15)
								bitShift: 4).
				hi := (hi bitShift: -4)
							+ (lo bitShift: -8).
				lo := lo bitAnd: 255.
				"Correct overestimate of q.  
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 := j < 3
							ifTrue: [0]
							ifFalse: [rem digitAt: j - 2].
				[(t < hi
					or: [t = hi and: [r3 < lo]])
					and: 
						["i.e. (t,r3) < (hi,lo)"
						qlo := qlo - 1.
						lo := lo - dnh.
						lo < 0
							ifTrue: 
								[hi := hi - 1.
								lo := lo + 256].
						hi >= dh]]
					whileTrue: [hi := hi - dh].
				qlo < 0
					ifTrue: 
						[qhi := qhi - 1.
						qlo := qlo + 16]].
		"Subtract q*div from rem"
		l := j - dl.
		a := 0.
		1 to: div digitLength do: 
			[:i | 
			hi := (div digitAt: i)
						* qhi.
			lo := a + (rem digitAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div digitAt: i)
							* qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a := lo // 256 - (hi bitShift: -4).
			l := l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo := qlo - 1.
				l := j - dl.
				a := 0.
				1 to: div digitLength do: 
					[:i | 
					a := (a bitShift: -8)
								+ (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l := l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem := rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem! !


!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:32:13'!
int32SlotAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger! !


!Color methodsFor: 'conversions' stamp: 'jmv 5/5/2023 17:31:03'!
pixelValueForDepth: d
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	"Most common case"
	"eight bits per component; top 8 bits set to all ones (opaque alpha)"
	d = 32 ifTrue: [
		^ 16rFF000000 bitOr:
			((((self float32SlotAt: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self float32SlotAt: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self float32SlotAt: 3) * 255.999) truncated))) ].

"Faster in 32 bit systems, but slower in 64 bits"
"	d = 32 ifTrue: [
		val := LargePositiveInteger new: 4.
		val at: 3 put: ((self at: 1) * 255.999) truncated.
		val at: 2 put: ((self at: 2)  * 255.999) truncated.
		val at: 1 put: ((self at: 3)  * 255.999) truncated.
		val at: 4 put: 16rFF.
		^ val normalize]."

	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue !!= 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val := ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val := ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val := ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString! !


!Color class methodsFor: 'examples' stamp: 'jmv 12/17/2018 16:29:53'!
experimentsTowardsANewColorPalette
"
self experimentsTowardsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance |
width := 300.
height := 120.
selectedColor := Color random.
selectedHue := selectedColor hue.
selectedSaturation := selectedColor saturation.
selectedChroma := selectedColor chroma.
selectedV := selectedColor brightness.
selectedLuminance := selectedColor luminance.
Display getCanvas fillRectangle: (0@0 extent: height@height) color: selectedColor.
0 to: height do: [ :y |
	v := 1.0 - (y / height).
	0 to: height do: [ :x |
		s := x / height.
		color := Color new setHue: selectedHue saturation: s brightness: v.
		Display colorAt: x@(y+height) put: color
	].
	DisplayScreen screenUpdateRequired: nil
].
0 to: height do: [ :y | | c |
	v := 1.0 - (y / height).
	s := 1.0 - (y / height).
	c := s.
	0 to: width do: [ :x |
		h := x / width * 360.
		
		color := Color new setHue: h chroma: c luminance: selectedLuminance.
"		color := Color new setHue: h chroma: c brightness: selectedV."
		color ifNil: [ color := Color black ].
"		color := Color new setHue: h saturation: s brightness: selectedV."
		Display colorAt: x+height@y put: color.
		
		color := Color new setHue: h chroma: selectedChroma luminance: v.
"		color := Color new setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color := Color black ].
"		color := Color new setHue: h saturation: selectedSaturation brightness: v."
		Display colorAt: x+height@(y+height) put: color.
		
	].
	DisplayScreen screenUpdateRequired: nil
].! !

!Color class methodsFor: 'color space conversions' stamp: 'jmv 4/24/2013 10:49'!
linearTosRGBGamma: value
	"The sRGB standard specifies a gamma curve that is close to gamma = 2.2, but is actually
	built from one stright line segment, and a curve with exponent = 2.4.
	This method converts value from linearSpace to gammaSpace, i.e. it 'encodes' with gamma.
	This is the operation done when producing an 8bit Form for displaying, or for saving on a JPG, PNG, etc; if source data is in linear space (for example, from raw data a sensor image, or data that was converted previously to linear space for proper image processing).
	value must be in [0.0 .. 1.0].

	See
		http://en.wikipedia.org/wiki/Gamma_correction
		http://www.poynton.com/notes/colour_and_gamma/GammaFAQ.html
		http://entropymine.com/imageworsener/srgbformula
	
	| r v |
	r := Random new.
	1000 timesRepeat: [
		v := r next.
		self assert: ((Color sRGBGammaToLinear: (Color linearTosRGBGamma: v)) closeTo: v) ]

	| r |
	r := 10@10 extent: 600@400.
	Display fill: r fillColor: Color white.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color linearTosRGBGamma: v ]) in: r color: Color black min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color applySimpleGamma: 1/2.2 to: v ]) in: r color: Color blue min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color sRGBGammaToLinear: v ]) in: r color: Color black min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color applySimpleGamma: 2.2 to: v ]) in: r color: Color blue min: 0.0 max: 1.0.
	"

	^ value <= 0.00313066844250063
		ifTrue: [ value * 12.92 ]
		ifFalse: [ (value raisedTo: 1.0/2.4) * 1.055 - 0.055 ]! !


!Categorizer class methodsFor: 'documentation' stamp: 'NS 4/5/2004 17:44'!
documentation
	"Instances consist of an Array of category names (categoryArray), each of 
	which refers to an Array of elements (elementArray). This association is 
	made through an Array of stop indices (categoryStops), each of which is 
	the index in elementArray of the last element (if any) of the 
	corresponding category. For example: categories := Array with: 'firstCat' 
	with: 'secondCat' with: 'thirdCat'. stops := Array with: 1 with: 4 with: 4. 
	elements := Array with: #a with: #b with: #c with: #d. This means that 
	category firstCat has only #a, secondCat has #b, #c, and #d, and 
	thirdCat has no elements. This means that stops at: stops size must be the 
	same as elements size." ! !


!BasicClassOrganizer methodsFor: 'copying' stamp: 'jmv 5/20/2011 11:24'!
postCopy
	super postCopy.

	"I guess not..."
	"subject := subject copy."

	classComment := classComment copy.
	commentStamp := commentStamp copy! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 4/11/2023 12:34:44'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory canonicalCombiningClass characterDecompositionMapping decomposition composition |
	UppercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	CanonicalCombiningClasses := Dictionary new.
	DecompositionMapping := Dictionary new.
	CompositionMapping := Dictionary new.
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields := strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self error: 'Bad UnicodeData.txt file' ].
			codePoint := ('16r', fields first) asNumber.
			codePointName := fields at: 2.
			generalCategory := fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				({ 'Lu'. 'Lt' } includes: generalCategory) ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Ll'. 'Lo' } includes: generalCategory) ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			canonicalCombiningClass := (fields at: 4) asNumber.
			canonicalCombiningClass = 0 ifFalse: [
				CanonicalCombiningClasses at: codePoint put: canonicalCombiningClass ].
			characterDecompositionMapping := fields at: 6.
			(characterDecompositionMapping size > 0 and: [ characterDecompositionMapping first ~= $< ]) ifTrue: [
				decomposition := characterDecompositionMapping substrings collect: [ :s | ('16r', s) asNumber].
				DecompositionMapping at: codePoint put: decomposition.
				decomposition size > 1 ifTrue: [
					composition := CompositionMapping at: decomposition first ifAbsentPut: [Dictionary new].
					composition at: decomposition second put: codePoint.
					decomposition size > 2 ifTrue: [self error: decomposition printString print].
					]].
			"
			uppercaseMapping := fields at: 13. lowercaseMapping := fields at: 14. titlecaseMapping := fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue := fields at: 7. digitValue := fields at: 8. numericValue := fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].! !


!ContextPart methodsFor: 'private' stamp: 'di 10/23/1999 17:31'!
stackp: newStackp
	"Storing into the stack pointer is a potentially dangerous thing.
	This primitive stores nil into any cells that become accessible as a result,
	and it performs the entire operation atomically."
	"Once this primitive is implemented, failure code should cause an error"

	<primitive: 76>
	self error: 'stackp store failure'.
"
	stackp == nil ifTrue: [stackp := 0].
	newStackp > stackp  'effectively checks that it is a number'
		ifTrue: [oldStackp := stackp.
				stackp := newStackp.
				'Nil any newly accessible cells'
				oldStackp + 1 to: stackp do: [:i | self at: i put: nil]]
		ifFalse: [stackp := newStackp]
"! !


!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 5/23/2023 12:33:51'!
fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore := true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd _ Time localMillisecondClock - lastEventPoll.
	"

	eventBuffer := Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 := eventBuffer third.
			u1 := eventBuffer sixth.
			pressType := eventBuffer fourth.
			typeS := 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS := #keystroke].
			dd >100 ifTrue: ['-' print. dd := 0 ].
			{ k1 < 256 ifTrue: [Character numericValue: k1] ifFalse: [ '[]']. UnicodeCodePoint codePoint: k1. k1. k1 hex. '-----'. 
				UnicodeCodePoint codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type := eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ].

	"Remember the last time that I checked for events."
	lastEventPoll := Time localMillisecondClock.! !


!Collection methodsFor: 'testing' stamp: 'jmv 7/3/2010 16:43'!
ifEmpty: aBlock
	"Evaluate the block if I'm empty
	'' ifEmpty: [ :c | c, 'coda' ] 					'coda'
	'' ifEmpty: [ 'replacement' ]					 'replacement'
	'stuff' ifEmpty: [ :c | c, 'coda' ] 				'stuff'
	'stuff' ifEmpty: [ 'replacement' ]				'stuff'
	|c|c := OrderedCollection new. c ifEmpty: [c add: 'stuff']. c			 an OrderedCollection('stuff')
	|c|c := OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff']. c	 an OrderedCollection('stuff')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff']. c		 an OrderedCollection('a')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff']. c  an OrderedCollection('a')
	"

	 self isEmpty ifTrue: [ ^aBlock valueWithPossibleArgument: self ]! !

!Collection methodsFor: 'testing' stamp: 'jmv 7/3/2010 16:46'!
ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	'' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ] 								 'coda'
	'' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ]								 'replacement'
	'stuff' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ]							 'stuffcoda2'
	'stuff' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ]						 'replacement2'
	|c|c := OrderedCollection new. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c						  an OrderedCollection('stuff')
	|c|c := OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c	 		 an OrderedCollection('stuff')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c		 			 an OrderedCollection('a' 'stuff2')
	|c|c := OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c  		 an OrderedCollection('a' 'stuff2')
	"
	^ self isEmpty
		ifTrue: [ emptyBlock valueWithPossibleArgument: self ]
		ifFalse: [ notEmptyBlock valueWithPossibleArgument: self ]! !

!Collection methodsFor: 'testing' stamp: 'jmv 7/3/2010 16:42'!
ifNotEmpty: aBlock
	"Evaluate the block unless I'm empty
	'' ifNotEmpty: [ :c | c, 'coda' ] 						 ''
	'' ifNotEmpty: [ 'replacement' ]					 	 ''
	'stuff' ifNotEmpty: [ :c | c, 'coda' ] 					 'stuffcoda'
	'stuff' ifNotEmpty: [ 'replacement' ]				 	 'replacement'
	|c|c := OrderedCollection new. c ifNotEmpty: [c add: 'stuff']. c			  an OrderedCollection()
	|c|c := OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff']. c	  an OrderedCollection()
	|c|c := OrderedCollection with: 'a'. c ifNotEmpty: [c add: 'stuff']. c		  an OrderedCollection('a' 'stuff')
	|c|c := OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff']. c   an OrderedCollection('a' 'stuff')
	"

	self isEmpty ifFalse: [^aBlock valueWithPossibleArgument: self]! !

!Collection methodsFor: 'testing' stamp: 'jmv 7/3/2010 16:48'!
ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	''  ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ] 									 'coda'
	'' ifNotEmpty: [ 'replacement2' ]ifEmpty: [ 'replacement' 	]								 'replacement'
	'stuff'  ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ]								 'stuffcoda2'
	'stuff' ifNotEmpty: [ 'replacement2' ] ifEmpty: [ 'replacement' ]							 'replacement2'
	|c|c := OrderedCollection new. c  ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c			 		 an OrderedCollection('stuff')
	|c|c := OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c	 	 an OrderedCollection('stuff')
	|c|c := OrderedCollection with: 'a'. c  ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c		 		 an OrderedCollection('a' 'stuff2')
	|c|c := OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c 	an OrderedCollection('a' 'stuff2')
	"

	^self ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock! !


!ByteArray class methodsFor: 'instance creation' stamp: 'jmv 5/20/2015 08:40'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection.
	Argument can also be a LargeInteger!!"
	| newArray |
	newArray := self new: aCollection size.
	1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
	^ newArray

"
	ByteArray newFrom: 1000 factorial
	
	| ba li |
	ba := ByteArray newFrom: 1000 factorial.
	li := LargePositiveInteger new: ba size.
	li replaceFrom: 1 to: ba size with: ba startingAt: 1.
	li / 999 factorial
"! !


!IntegerArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 23:34'!
atAllPut: anInteger
	| word |
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self primFill: word.! !

!IntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:24:33'!
integerAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger! !


!Bitmap methodsFor: 'accessing' stamp: 'ar 3/3/2001 22:42'!
integerAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger! !


!Semaphore methodsFor: 'communication' stamp: 'ST-80 5/31/1983 9:10:35'!
signal
	"Send a signal through the receiver. If one or more processes have been 
	suspended trying to receive a signal, allow the first one to proceed. If no 
	process is waiting, remember the excess signal. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals := excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"! !

!Semaphore methodsFor: 'communication' stamp: 'ST-80 5/31/1983 9:10:35'!
wait
	"The active Process must receive a signal through the receiver before 
	proceeding.  If no signal has been sent, the active Process will be suspended
	until one is sent.  Essential.  See  
	Object whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals := excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/29/2009 16:15'!
renameAndUpdateReferences: oldName as: newName
	"Dangerous. Simple search and replace could break stuff.
	Use only to generate change sets to later checked with care
	
OldMorph withAllSubclasses do: [ :c |
	n := c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

OldBorderStyle withAllSubclasses do: [ :c |
	n := c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

#(#OldKeyboardBuffer #OldLayoutCell #OldLayoutFrame #OldLayoutPolicy #OldLayoutProperties #OldMorphExtension #OldMouseClickState #OldProportionalLayout #OldTableLayout #OldTableLayoutProperties #OldTextAnchor #OldTextComposer #OldTextContainer #OldTextLine #OldTheWorldMenu #OldWorldState #OldWorldTest) do: [ :n |
	Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]
	"

	self renameClassNamed: oldName as: newName.
	(Smalltalk allCallsOn: (Smalltalk associationAt: newName)) do: [ :methodRef |
		methodRef updateReferencesTo: oldName toBe: newName ]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/12/2012 10:37'!
browseUndeclaredReferences
	"
	Smalltalk browseUndeclaredReferences
	"
	| anythingToShow |
	Smalltalk cleanOutUndeclared.
	anythingToShow := false.
	Undeclared keys do: [ :k |
		anythingToShow := true.
		self
			browseMessageList: (Smalltalk allCallsOn: (Undeclared associationAt: k))
			name: 'References to Undeclared: ', k printString ].

	"undeclared not in Undeclared - "
	"This happened in Pharo. See http://lists.gforge.inria.fr/pipermail/pharo-project/2012-March/061270.html
	Maybe do something like this in Cuis too???
	lostUndeclared := Smalltalk allSelect: [:m|
		m literals anySatisfy: [:l|
			l isVariableBinding
				and: [l key isSymbol ""avoid class-side methodClass literals""
				and: [(m methodClass bindingOf: l key) isNil
				and: [(Undeclared includesAssociation: l) not]]]]].
	"

	anythingToShow ifFalse: [
		 (PopUpMenu labels: ' OK ')
				startUpWithCaption: 'There are no Undeclared at all' ]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/21/2022 12:27:00'!
obsoleteClasses   "Smalltalk obsoleteClasses inspect"
	"NOTE:  Also try inspecting comments below"
	| obs c |
	obs := OrderedCollection new.  Smalltalk garbageCollect.
	Metaclass allInstancesDo:
		[:m | c := m soleInstance.
		(c notNil and: ['AnOb*' match: c name asPlainString])
			ifTrue: [obs add: c]].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"
"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found := false.
	1 to: m numLiterals do:
		[:i | (((l := m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found := true]].
	found]
"! !

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 10/20/2023 19:19:14'!
knownInitialsAndNames
	"This list could include people who hasn't contributed code to the Cuis image, but to some optional package."
"
| all ok |
all := Smalltalk allContributors asSet.
ok := (Smalltalk knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
(all copyWithoutAll: ok) print

		initials         				name"
^ #(
	#('ab' 						'Alexandre Bergel')
	#('abc' 						'Colin Putney')
	#('acg' 						'Andrew C. Greenberg')
	#('ads' 						'Adam Spitz')
	#('AFi' 						'Alain Fischer')
	#('ajh' 						'Anthony Hannan')
	#('al' 						'Adrian Lienhard')
	#('and' 						'Alan Dao')
	#('aoy' 						'Andres Otaduy')
	#('apb' 						'Andrew P. Black')
	#('ar' 						'Andreas Raab')
	#('asm' 						'Alejandro Magistrello')
	#('avi' 						'Avi Bryant')
	#('AY'						'Angel Yan')
	#('BenComan' 						'Ben Coman')
	#('bf' 						'Bert Freudenberg')
	#('BG' 						'Boris Gaertner')
	#('bgs' 						'Boris G. Shingarov')
	#('BJP' 						'Bijan Parsia')
	#('bkv' 						'Brent Vukmer')
	#('bolot' 						'Bolot Kerimbaev')
	#('bp' 						'Bernhard Pieber')
	#('BP' 						'Brent Pinkney') 
	#('brp' 						'Brent Pinkney')
	#('cbc' 						'Chris Cunningham')
	#('cbr'						'Casey Ransberger')
	#('ccn' 						'Chris Norton')
	#('cmm' 						'Chris Muller')
	#('crl' 						'Craig Latta')
	#('cwp' 						'Colin Putney')
	#('das' 						'David A Smith')
	#('db' 						'Douglas Brebner')
	#('dc' 						'Damien Cassou')
	#('dew' 						'Doug Way')
	#('DF'						'David Faitelson')
	#('dgd' 						'Diego Gomez Deck')
	#('dkh'						'Dale Henrichs')
	#('dhn'	 					'Dan Norton')
	#('dhhi' 						'Dan Ingalls')
	#('di' 						'Dan Ingalls')
	#('djp' 						'David J. Pennell')
	#('DKL'						'Daniel K Lyons')
	#('DM' 						'Duncan Mak')
	#('DSM' 						'Duane Maxwell')
	#('DSG'						'David Graham')
	#('dtl' 						'Dave Lewis')
	#('dvf'	 					'Daniel Vainsencher')
	#('eat' 						'Eric Arseneau Tremblay')
	#('EB'						'Eric Brandwein')
	#('eem'						'Eliot Emilio Miranda')
	#('eliot'						'Eliot Emilio Miranda')
	#('efc' 						'Eddie Cottongim')
	#('em' 						'Ernest Micklei?')
	#('emm' 						'Ernest Micklei')
	#('fbs' 						'Frank Shearar')
	#('FBS' 						'Frank Shearar')
	#('fc' 						'Frank Caggiano')
	#('fcs' 						'Frank Sergeant')
	#('FernandoOlivero' 						'Fernando Olivero')
	#('FernanodOlivero' 						'Fernando Olivero')
	#('FGJ'						'Fernando Gasperi Jabalera')
	#('FJG'				 		'Facundo Javier Gelatti')
	#('GabrielOmarCotelli' 						'Gabriel Omar Cotelli')
	#('GC' 						'Gastón Caruso')
	#('gera' 						'Gerardo Richarte')
	#('gh' 						'Goran Krampe (nee Hultgren)')
	#('gk' 						'Goran Krampe (nee Hultgren)')
	#('gm' 						'German Morales')
	#('go' 						'Georg Gollmann')
	#('gsa' 						'German Arduino')
	#('HAW' 						'Hernan Wilkinson')
	#('HB' 						'Hari Balaraman')
	#('hjh' 						'Hannes Hirzel')
	#('hlsf' 						'Hilaire Fernandes')
	#('hmm' 						'Hans-Martin Mosner')
	#('hsj' 						'Henrik Sperre Johansen')
	#('Igor.Stasenko' 						'Igor Stasenko')
	#('ikp' 						'Ian Piumarta')
	#('jar' 						'Jaromir Matas')
	#('Jb' 						'Jean Baptiste Arnaud')
	#('jcg' 						'Joshua Gargus')
	#('jdr' 						'Javier Diaz-Reinoso')
	#('je' 						'Joern Eyrich')
	#('jf' 						'Julian Fitzell')
	#('JF' 						'Julian Fitzell')
	#('jhm' 						'John Maloney')
	#('jk'						'Jonathan Kelly')
	#('jlb' 						'Jim Benson')
	#('jm' 						'John Maloney')
	#('jmb' 						'Hans Baveco')
	#('JMG'						'Jeff Gonis')
	#('JMM' 						'John McIntosh')
	#('jmv' 						'Juan Vuletich')
	#('JMV' 						'Juan Vuletich')
	#('JO'						'Javier Olaechea')
	#('jp' 						'Joseph Pelrine')
	#('jrm' 						'John-Reed Maffeo')
	#('jrp' 						'John Pierce')
	#('jsp' 						'Jeff Pierce')
	#('KenD' 						'Ken Dickey')
	#('kfr' 						'Karl Ramberg')
	#('KLC'			 			'Ken Causey')
	#('KLG'			 			'Gerald Klix')
	#('kph'						'Keith Hodges')
	#('KTT' 						'Kurt Thams')
	#('laza' 						'Alexander Lazarevic')
	#('LC' 						'Leandro Caniglia')
	#('len' 						'Luciano Esteban Notarfrancesco')
	#('lpc'						'Laura Perez Cerrato')
	#('lr' 						'Lukas Renggli')
	#('Lukas Renggli' 						'Lukas Renggli')
	#('ls' 						'Lex Spoon')
	#('md' 						'Marcus Denker')
	#('MarcusDenker' 						'Marcus Denker')
	#('marcus.denker' 						'Marcus Denker')
	#('mdr' 						'Mike Rutenberg')
	#('MEGL' 						'MarceloLamperti')
	#('mga' 						'Markus Galli')
	#('MGD'						'Matías Dinota')
	#('MH'						'Matías Halperin')
	#('mha' 						'Michael Haupt')
	#('mir' 						'Michael Rueger')
	#('mjg' 						'Mark Guzdial')
	#('mk'	 					'Matej Kosik')
	#('MM'	 					'Mariano Montone')
	#('mnqpr'	 					'Szabolcs Komaromi')
	#('MPH'	 					'Michael Hewner')
	#('mpw' 						'Marcel Weiher')
	#('MPW' 						'Marcel Weiher')
	#('mrm' 						'Martin McClure')
	#('mtf' 						'Matthew Fulmer')
	#('mu' 						'Masashi Umezawa')
	#('nb' 						'Naala Brewer')
	#('nice'					 	'Nicolas Cellier')
	#('nk' 						'Ned Konz')
	#('NM' 						'Nicola Mingotti')
	#('NMS'						'Nicolás Sarfati')
	#('nop' 						'Jay Carlson')
	#('NS' 						'Nathanael Schaerli')
	#('panda' 						'Michael Rueger')
	#('pau'						'Paula Perez Bianchi')
	#('pb'						'Phil Bellalouna')
	#('PB'						'Paolo Belforte')
	#('pmon'						'Paolo Montrasi')
	#('PHK' 						'Peter Keeler')
	#('Pmm' 						'Philippe Marschall')
	#('pnm' 						'Paul McDonough')
	#('r++' 						'Gerardo Richarte')
	#('raa' 						'Bob Arning')
	#('RAA' 						'Bob Arning')
	#('raok' 						'Richard A. O''Keefe')
	#('rca' 						'Russell Allen')
	#('reThink'				 		'Paul McDonough')
	#('rew' 						'Roger Whitney')
	#('rhi' 						'Robert Hirschfeld')
	#('RJT' 						'Ron Teitelbaum')
	#('RNG' 						'Nahuel Garbezza')
	#('rr' 						'Romain Robbes')
	#('rss' 						'Ron Spengler')
	#('rw' 						'Robert Withers')
	#('rww' 						'Robert Withers')
	#('Sames' 						'Samuel S. Shuster')
	#('sbw' 						'Stephan B. Wessels')
	#('sd' 						'Stephane Ducasse')
	#('SD' 						'Stephane Ducasse')
	#('sge' 						'Steve Elkins')
	#('sjd' 						'Santiago Jose Dandois')
	#('SLD'						'Steve Davies')
	#('sma' 						'Stefan Matthias Aust')
	#('sps' 						'Steven Swerling')
	#('SqR' 						'Andres Valloud')
	#('sqr' 						'Andres Valloud')
	#('Squeak1.0'						'Squeak 1.0, September 20, 1996')
	#('Squeak1.1'						'Squeak 1.1, September 23, 1996')
	#('Squeak1.2'						'Squeak 1.2, June 29, 1997')
	#('Squeak1.3'						'Squeak 1.3, January 16, 1998')
	#('sr' 						'Stephan Rudlof')
	#('ss'						'Sebastian Sujarchuk')
	#('SSC' 						'Santiago Salvador Cubino')
	#('SSS' 						'Samuel S. Shuster')
	#('ST-80'						'Smalltalk-80, version 2, of April 1, 1983')
	#('stephane.ducasse' 						'Stephane Ducasse')
	#('stephaneducasse'	 					'Stephane Ducasse')
	#('stp' 						'Stephen Travis Pope')
	#('sumim' 						'Masato Sumi')
	#('svc' 						'Sven Van Caekenberghe')
	#('svp' 						'Stephen Vincent Pair')
	#('sw' 						'Scott Wallace')
	#('TAG' 						'Travis Griggs')
	#('tak' 						'Takashi Yamamiya')
	#('tao' 						'Tim Olson')
	#('TBn' 						'Torsten Bergmann')
	#('tfei' 						'The Fourth Estate, Inc.')
	#('tfel' 						'Tim Felgentreff')
	#('th' 						'Torge Husfeldt')
	#('tk' 						'Ted Kaehler')
	#('tlk' 						'Tom Koenig')
	#('tpr' 						'Tim Rowledge')
	#('TPR' 						'Tim Rowledge')
	#('TS' 						'Tomás Spognardi')
	#('tsl' 						'Thiago da Silva Lino')
	#('tween' 						'Andy Tween')
	#('ul' 						'Levente Uzonyi')
	#('vb' 						'Vassili Bykov')
	#('ward' 						'Ward Cunningham')
	#('wiz' 						'Jerome Peace')
	#('wod' 						'Bill Dargel')
	#('yo' 						'Yoshiki Ohshima')
	#('zz' 						'Serge Stinckwich'))! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'hlsf 6/23/2022 11:25:12'!
askConfirmationOnQuit
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk askConfirmationOnQuit
	"
	| baseCSdirty dirtyPackages |
	baseCSdirty := ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	"dirtyPackages := CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ]."
	dirtyPackages := ChangeSet allChangeSets anySatisfy: [ :any | any codePackage notNil and: [ any hasUnsavedChanges ]].
	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, all unsaved changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, their changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	(Preferences at: #askConfirmationOnQuit) ifTrue: [
		^self confirm: 'Do you really want to exit Cuis without saving the image?' ].
	^true! !


!ReadWriteStream methodsFor: 'accessing' stamp: 'jmv 8/22/2021 13:55:58'!
truncateAtPosition
	"Truncate the receiver at current position.
	For example, this should evaluate to true:
		| s |
		s := ReadWriteStream on: #().
		s nextPutAll: 'abcdefg'.
		s reset.
		s next; next.
		s nextPut: $z.
		s truncateAtPosition.
		s atEnd
	"
	readLimit := position! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 5/5/2023 20:16:39'!
readMethod
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize refPosn newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	className := self next.
	newClass := Smalltalk at: className asSymbol.

	xxHeader := self next.
		"nArgs := (xxHeader >> 24) bitAnd: 16rF."
		"nTemps := (xxHeader >> 18) bitAnd: 16r3F."
		"largeBit := (xxHeader >> 17) bitAnd: 1."
	nLits := (xxHeader >> 9) bitAnd: 16rFF.
		"primBits := ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer := instSize - (newClass instSize "0") - (nLits + 1 * Smalltalk wordSize).

	newMethod := newClass 
		newMethod: byteCodeSizePlusTrailer
		header: xxHeader.

	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	self beginReference: newMethod.
	lits := newMethod numLiterals + 1.	"counting header"
	2 to: lits do:
		[:ii | newMethod objectAt: ii put: self next].
	lits*Smalltalk wordSize+1 to: newMethod basicSize do:
		[:ii | newMethod basicAt: ii put: byteStream next].
			"Get raw bytes directly from the file"
	self setCurrentReference: refPosn.  "before returning to next"
	^ newMethod! !


!ReferenceStream methodsFor: 'writing' stamp: 'tk 8/18/1998 08:38'!
reset
	"PRIVATE -- Reset my internal state.
	   11/15-17/92 jhm: Added transients and fwdRefEnds.
	   7/11/93 sw: Give substantial initial sizes to avoid huge time spent growing.
	   9/3/93 sw: monster version for Sasha"

	super reset.
	references := IdentityDictionary new: 4096 * 5.
"	objects := IdentityDictionary new: 4096 * 5.
	fwdRefEnds := IdentityDictionary new.
"
	blockers ifNil: [blockers := IdentityDictionary new].
 ! !


!ReferenceStream class methodsFor: 'as yet unclassified' stamp: 'jmv 3/10/2018 21:54:43'!
example2
"Here is the way to use DataStream and ReferenceStream:
	rr := ReferenceStream fileNamed: ''test.obj''.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr := ReferenceStream fileNamed: ''test.obj''.
	<your object> := rr next.
	rr close.
"
"An example and test of DataStream/ReferenceStream.
	 11/19/92 jhm: Use self testWith:."
	"ReferenceStream example2"
	| input sharedPoint |

	"Construct the test data."
	input := Array new: 9.
	input at: 1 put: nil.
	input at: 2 put: true.
	input at: 3 put: false.
	input at: 4 put: #(-4 -4.0 'four' four).
	input at: 5 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 5) fillWithColor: `Color lightOrange`.
	input at: 6 put: 1024 @ -2048.
	input at: 7 put: input. "a cycle"
	input at: 8 put: (Array with: (sharedPoint := 0 @ -30000)).
	input at: 9 put: sharedPoint.

	"Write it out, read it back, and return it for inspection."
	^ self testWith: input
! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 4/21/2022 14:50:49'!
versionSymbol: instVarList
	"Create the symbolic code (like a version number) for this class in some older version.  First initials of all the inst vars, followed by the class version number.  Returns a string, caller makes it into a compound selector.  "

	| str |
	str := instVarList size = 1 ifFalse: [''] ifTrue: ['x'].		"at least one letter"
	2 to: instVarList size do: [:ind |
		str := str, (instVarList at: ind) first asPlainString].
	str := str, instVarList first printString.	"the number"
	^ str

" | list | list := (Array with: Paragraph classVersion), (Paragraph alistInstVarNames).
(SmartRefStream  on: (DummyStream on: nil)) versionSymbol: list
"! !


!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 13:24'!
isReady
	"True if not running right now, but might be scheduled anytime.

	Useful to play with:

	| s |	
	s := Semaphore new.
	s inspect.
	([ s wait . 10000 timesRepeat: [ 10000 factorial. (Delay forSeconds: 1) wait. 7 print ]] newProcess name: 'test5') inspect
	
	{ self isRunning . self isReady . self isSuspended . self isBlocked . self isTerminated } print
	"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == LinkedList! !


!Scanner methodsFor: 'public access' stamp: 'jmv 5/23/2022 17:15:50'!
scanTokenPositionsIn: textOrString into: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."

	self initScannerForTokenization.
	source := textOrString asPlainString readStream.
	buffer := source collectionSpecies writeStream.
	self step.
	self step.
	self scanAllTokenPositionsInto: aBlock

	"| code |
	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).
	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"

	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: String crString with: '')"

	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable := ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable := ST80ColorTable]'"! !


!Form methodsFor: 'analyzing' stamp: 'jmv 4/2/2016 23:06'!
cgForPixelValue: pv orNot: not
	"Return the center of gravity for all pixels of value pv.
	Note:  If orNot is true, then produce the center of gravity for all pixels
	that are DIFFERENT from the supplied (background) value"
	| pixCount weighted xAndY |
	xAndY := (Array with: (self xTallyPixelValue: pv orNot: not)
					with: (self yTallyPixelValue: pv orNot: not)) collect: [ :profile |	"For both x and y profiles..."
		pixCount := 0.  weighted := 0.
		profile withIndexDo: [ :t :i |
			pixCount := pixCount + t.
			weighted := weighted + (t*i)].
		pixCount = 0  "Produce average of nPixels weighted by coordinate"
			ifTrue: [0.0]
			ifFalse: [weighted asFloat / pixCount asFloat - 1.0]].

	^ xAndY first @ xAndY last
"
| r f cg |
[Sensor isAnyButtonPressed] whileFalse: [
	r := Sensor mousePoint extent: 50@50.
	f := Form extent: r extent depth: 16.
	f fromDisplay: r.
	cg := f cgForPixelValue: (Color black pixelValueForDepth: f depth) orNot: false.
	f displayAt: 0@0.
	Display fill: (cg extent: 4) fillColor: Color red ]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 6/3/2016 18:13'!
pixelCompare: aRect with: otherForm at: otherLoc
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	"Just use 32 bits..."
	| pixPerWord temp |
	otherForm nativeDepth = 32 ifFalse: [
		^ self pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc ].
	self nativeDepth = 32 ifFalse: [
		^ (self asFormOfDepth: 32) pixelCompare: aRect with: otherForm at: otherLoc ].
	
	pixPerWord := 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue: [
		"If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp := self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
 | f diff |
f := Form fromUser.
[ Sensor isAnyButtonPressed ] whileFalse: [
	diff := f
		pixelCompare: f boundingBox
		with: Display
		at: Sensor mousePoint.
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	diff printString , '        ' displayAt: 0@0 ]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 3/12/2018 15:50:49'!
tallyPixelValuesInRect: destRect into: valueTable
	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."

	(BitBlt toForm: self)
		sourceForm: self;  "src must be given for color map ops"
		sourceOrigin: `0@0`;
		colorMap: valueTable;
		combinationRule: 33;
		destRect: destRect;
		copyBits.
	^ valueTable

"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r := Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies := (Display copy: r) tallyPixelValues.
	nonZero := (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'converting' stamp: 'jmv 12/14/2014 00:37'!
convertAlphaToZeroValueTransparency
	"For a 32bpp Form, for each pixel, take the alpha value, and if less than 128
	make the pixel value zero (i.e. transparent for 2, 4, 8 and 16 bpp)
	Side effect: make value of alpha = 0 or 128 for all pixels. Not a problem if we're converting to lower bpp anyway...
	This method also loses the lsb of the color components. Again, not a problem for going to lower bpp.
	"
	"
	| bananas1 |
	bananas1 := Form fromFileNamed: 'bananas1.png'.
	bananas1 convertAlphaToZeroValueTransparency..
	bananas1 convertAlphaToZeroValueTransparency.
	(bananas1 asFormOfDepth: 16) display
	"
	| bitBlt map multiplier |
	self depth = 32 ifFalse: [
		^self ].
	
	"Prepare multiplier to multiply each pixel by 0.5 or 0 (according to alpha)"
	multiplier := Form extent: self extent depth: 32.
	map := ColorMap
		masks: { 16r80000000. 16r80000000. 16r80000000. 16r80000000}
		shifts:  { 0.  -8. -16. -24 }.
	bitBlt := BitBlt new.
	bitBlt
		setDestForm: multiplier;
		sourceForm: self;
		colorMap: map;
		combinationRule: Form over;
		copyBits.
	"Now fix for the fact that using a ColorMap turns pixelValues 0's into 1's"
	bitBlt
		sourceForm: nil;
		colorMap: nil;
		fillBitmap: (Bitmap with: 16rFFFFFFFE);
		combinationRule: Form and;
		copyBits.
	"Apply multiplier to ourselves"
	bitBlt
		fillBitmap: nil;
		setDestForm: self;
		sourceForm: multiplier;
		combinationRule: Form rgbMul;
		copyBits.
	"Now correct for the fact that the multiplier had 0.5's and 0's, not 1's and 0's"
	bitBlt
		sourceForm: self;
		combinationRule: 20; "Form rgbAdd"
		copyBits! !

!Form methodsFor: 'copying' stamp: 'jmv 3/12/2018 15:49:34'!
copyBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: 30
		destOrigin: destOrigin
		sourceOrigin: `0@0`
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f := Form fromUser. f2 := Form fromDisplay: (0@0 extent: f extent). f3 := f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 := f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'displaying' stamp: 'jmv 3/12/2018 15:50:23'!
paintBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt destForm: self
		sourceForm: sourceForm
		combinationRule: 31
		destOrigin: destOrigin
		sourceOrigin: `0@0`
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f := Form fromUser. f replaceColor: f dominantColor withColor: Color transparent.
f2 := Form fromDisplay: (0@0 extent: f extent). f3 := f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 := f2 copy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 5/31/2021 09:14:11'!
displayOn: aForm at: aDisplayPoint
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	| toBeDrawn rule |
	"Rule Form paint treats pixels with a value of zero as transparent"
	toBeDrawn := self.
	(aForm depth = 32 and: [ self depth = 32 ]) 
		ifTrue: [ rule := Form blend ] 	"Handle translucent pixels correctly. Requires both source and dest of 32bpp"
		ifFalse: [
			"Warning. Using 'Form paint' with a 32bpp source that includes
			traslucent or transparent alphas will give incorrect results (alpha values will be ignored).
			Doing what follows might be terribly slow. It is best to convert to lower depth on image load."
			"self depth = 32 ifTrue: [
				toBeDrawn := self asFormOfDepth: aForm depth ]."
			rule := Form paint ].
	toBeDrawn displayOn: aForm
		at: aDisplayPoint
		clippingBox: aForm boundingBox
		rule: rule! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 5/31/2016 11:24'!
writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."

	fileName asFileEntry forceWriteStreamDo: [ :file |
		file binary.
		file nextPut: 2.  "file code = 2"
		self writeOn: file ].

"
 | f |
[(f := Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !

!Form methodsFor: 'filling' stamp: 'jmv 9/23/2012 21:39'!
fillFromXYColorBlock: colorBlock
	"General Gradient Fill.
	Supply relative x and y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| poker yRel xRel |
	poker := BitBlt bitPokerToForm: self.
	0 to: height-1 do:
		[:y | yRel := y asFloat / (height-1) asFloat.
		0 to: width-1 do:
			[:x |  xRel := x asFloat / (width-1) asFloat.
			poker pixelAt: x@y
				put: ((colorBlock value: xRel value: yRel) pixelWordForDepth: self depth)]]
"
 | d |
((Form extent: 100@20 depth: Display depth)
	fillFromXYColorBlock:
	[:x :y | d := 1.0 - (x - 0.5) abs - (y - 0.5) abs.
	Color r: d g: 0 b: 1.0-d]) display
"! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 10/8/2014 10:36'!
tallyPixelValues
	"Return a Bitmap with tallies in it of the number of pixels in this Form that have each pixel value. Note that several Forms may be tallied into the same table by tallyPixelValuesPrimitive:into: with the same table. Also Forms of depth 16 or 32 can be tallied into a tables of size 512, 4096, or 32768 entries by making a direct call with a Bitmap of the given size."

	^ self tallyPixelValuesInRect: self boundingBox
		into: (Bitmap new: (1 bitShift: (self depth min: 15)))
"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r := Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies := (Display copy: r) tallyPixelValues.
	nonZero := (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/25/2023 14:23:56'!
flippedBy: direction
	"Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation)
	ImageMorph defaultForm display.
	(ImageMorph defaultForm flippedBy: #vertical) display.
	(ImageMorph defaultForm flippedBy: #horizontal) display.
	(ImageMorph defaultForm flippedBy: #both) display.
	"
	| newForm quad |
	newForm := self class extent: self extent depth: depth.
	quad := self boundingBox innerCorners.
	quad := (
		direction caseOf: {
			[ #vertical ] 		-> [#(2 1 4 3)].
			[ #horizontal ] 	-> [#(4 3 2 1)].
			[ #both ] 			-> [#(3 4 1 2)]})
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f := Form fromDisplay: ((p := Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/6/2014 10:44'!
magnify: aRectangle by: scale 
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."
	^ self magnify: aRectangle by: scale smoothing: 1

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
	[(Display magnify: (Sensor mousePoint extent: 31@41) by: 5@3) display]
"
"Scaling test...
| f cp | f := Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
	[cp := Sensor mousePoint.
	(f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent) display]]
"
"Consistency test...
 | f f2 p | [Sensor isAnyButtonPressed] whileFalse:
	[f := Form fromDisplay: ((p := Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f magnify: f boundingBox by: 5@3.
	(f2 shrink: f2 boundingBox by: 5@3) displayAt: p]
"
! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/6/2014 10:44'!
magnify: aRectangle by: scale smoothing: cellSize
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm := self blankCopyOf: aRectangle scaledBy: scale.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) by: 0.5 smoothing: 2) display]
"
"Scaling test...
| f cp | f := Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
        [cp := Sensor mousePoint.
        (f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent smoothing: 2) display]]
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 1/7/2015 08:13'!
rotatedBy: deg smoothing: cellSize
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."
	| side rot warp r1 pts p center |
	side := 1 + ((width*width) + (height*height)) asFloat sqrt asInteger.
	rot := Form extent: side@side depth: self depth.
	center := rot extent // 2.

	"Now compute the sin and cos constants for the rotation angle." 
	warp := (WarpBlt toForm: rot)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: rot);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form over.
	r1 := rot boundingBox aligned: center with: self boundingBox center.

	pts := r1 innerCorners collect: [ :pt |
		p := pt - r1 center.
		(r1 center x asFloat + (p x asFloat*deg degreeCos) + (p y asFloat*deg degreeSin)) @
		(r1 center y asFloat - (p x asFloat*deg degreeSin) + (p y asFloat*deg degreeCos))].
	warp copyQuad: pts toRect: rot boundingBox.
	^ rot
"
 | a f |  f := Form fromDisplay: (0@0 extent: 200@200).  a := 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedBy: (a := a+0.1) smoothing: 2) display].
f display
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 1/7/2015 08:13'!
rotatedByDegrees: deg
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."

	^ self rotatedBy: deg smoothing: 1
"
 | a f |  f := Form fromDisplay: (0@0 extent: 200@200).  a := 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedByDegrees: (a := a+0.1)) display].
f display
"! !


!Cursor class methodsFor: 'instance creation' stamp: 'jmv 4/29/2023 19:57:15'!
normalCursorWithMask
	"Cursor initNormalWithMask.  Cursor normalCursor show"
	"Next two lines work simply for any cursor..."
	"
	self initNormal.
	NormalCursor := CursorWithMask derivedFrom: NormalCursor.
	"
	"But for a good looking cursor, you have to tweak things..."
	^ ((CursorWithMask
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000000000000000
			2r0100000000000000
			2r0110000000000000
			2r0111000000000000
			2r0111100000000000
			2r0111110000000000
			2r0111111000000000
			2r0111111100000000
			2r0111111110000000
			2r0111110000000000
			2r0110110000000000
			2r0100011000000000
			2r0000011000000000
			2r0000001100000000
			2r0000001100000000
			2r0000000000000000)
		offset: `-1@ -1`)
	setMaskForm: 
		(Form
			small1BitExtent: `16@16`
			pixelBits: #(
				2r1100000000000000
				2r1110000000000000
				2r1111000000000000
				2r1111100000000000
				2r1111110000000000
				2r1111111000000000
				2r1111111100000000
				2r1111111110000000
				2r1111111111000000
				2r1111111111100000
				2r1111111000000000
				2r1110111100000000
				2r1100111100000000
				2r1000011110000000
				2r0000011110000000
				2r0000001110000000)
			offset: `0@0`))! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 6/13/2016 12:10'!
nextImage
	"Decode and answer a Form from my stream. 
	We can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!!)
		* -16-bit Forms (with or without dithering!!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!!)
		* -16-bit Forms (with or without dithering!!)
		* 8-bit GrayForms
		* -8-bit GrayForms"

	| bytes |
"	bytes := stream upToEnd."
	bytes := stream contents.
	^self uncompress: bytes into: nil! !


!BitBlt methodsFor: 'copying' stamp: 'jmv 5/9/2016 16:51'!
pixelAt: aPoint put: pixelValue
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX := aPoint x.
	destY := aPoint y.
	sourceForm bits at: 1 put: pixelValue.
	self copyBits
"
| bb |
bb := (BitBlt bitPokerToForm: Display).
[Sensor isAnyButtonPressed] whileFalse:
	[bb pixelAt: Sensor mousePoint put: 55. Display forceToScreen]
"! !

!BitBlt methodsFor: 'private' stamp: 'jmv 4/11/2023 12:20:43'!
oldPaintBits
	"Perform the old paint operation, which requires two calls to BitBlt."
	| requestedFill oldMap requestedRule |
	sourceForm depth = 1 ifFalse: [
		^ self error: 'paint operation is only defined for 1-bit deep sourceForms'].

	requestedRule := combinationRule.
	requestedFill := halftoneForm.
	oldMap := colorMap.

	halftoneForm := nil.
	colorMap := Bitmap with: 0 with: 16rFFFFFFFF.	"Map 1's to ALL ones, not just one"
	combinationRule := Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"

	halftoneForm := requestedFill.
	combinationRule := Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap := oldMap.
	combinationRule := requestedRule

" | dot |
dot := Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor mousePoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"! !

!BitBlt methodsFor: 'private' stamp: 'jmv 5/22/2015 13:39'!
setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin
	"Set up a BitBlt for copying a Rectangle as large as possible, with sourceOrigin and destOrigin. Bound by sourceForm and destForm extents. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf := FloatImage lena asForm.
	sourceOrigin := 30@30.
	df := Form extent: 156@156 depth: 32.
	df fillColor: Color red.
	destOrigin := 10@20.
	blitter := BitBlt new setSourceForm: sf sourceOrigin: sourceOrigin destForm: df destOrigin: destOrigin.
	blitter copyBits.
	df display
	"
	self
		setDestForm: df sourceForm: sf
		destOrigin: destOrigin sourceOrigin: sourceOrigin
		extent: (sf width - sourceOrigin x) @ (sf height - sourceOrigin y)! !

!BitBlt methodsFor: 'private' stamp: 'jmv 5/22/2015 12:00'!
setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent
	"Set up a BitBlt for copying a Rectangle of extent w@h , with sourceOrigin and destOrigin. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf := FloatImage lena asForm.
	sourceOrigin := 30@30.
	sourceOrigin := 0@0.
	df := Form extent: 256@256 depth: 32.
	df fillColor: Color red.
	destOrigin := 0@0.
	blitter := BitBlt new
		setSourceForm: sf sourceOrigin: sourceOrigin
		destForm: df destOrigin: destOrigin
		width: 256 height: 256.
	blitter copyBits.
	df display
	"
	self setDestForm: df.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceForm := sf.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := extent x.
	height := extent y.
	halftoneForm := nil.
	combinationRule := 3. 	"Form over"
	colorMap := nil! !


!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/21/2015 23:03'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	"
	Faster approach: Keep 2 instances, to be able to call prim.
	Cons: who cares? This is just for M3, that inlines most calls... Besides, immediate floats makes this bogus.
t := AffineTransformation withRadians: -3 scale: 12 position: 4.5@3.
[ 100000 timesRepeat: [ t transform: 2@3 ]] timeToRun.
[ 100000 timesRepeat: [ 2 ]] timeToRun.
z1 := AffineTransformation withTranslation: 2@3.
z2 := AffineTransformation new.
[ 100000 timesRepeat: [ t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
[ 100000 timesRepeat: [ z1 := AffineTransformation withTranslation: 2@3.z2 := AffineTransformation new.t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
"

	| x y |
	x := (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y := (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !


!Point methodsFor: 'point functions' stamp: 'jmv 8/25/2014 13:07'!
nearestPointAlongLineFrom: p1 to: p2
	"Note this will give points beyond the endpoints.
	Streamlined by Gerardo Richarte 11/3/97"
	| x21 y21 t x1 y1 |
	p1 x = p2 x ifTrue: [^ p1 x @ y].
	p1 y = p2 y ifTrue: [^ x @ p1 y].
	x1 := p1 x asFloat.
	y1 := p1 y asFloat.
	x21 := p2 x asFloat - x1.
	y21 := p2 y asFloat - y1.
	t := ((y asFloat - y1 / x21) + (x asFloat - x1 / y21))
			/ ((x21 / y21) + (y21 / x21)).
	^ (x1 + (t * x21)) @ (y1 + (t * y21))
"
	| old new |
	Pen new place: 200@100; goto: (old := 500@300).
	Display reverse: (old extent: 10@10).
	[Sensor isAnyButtonPressed] whileFalse:
		[(new := (Sensor mousePoint nearestPointAlongLineFrom: 200@100 to: 500@300) )
			= old ifFalse:
				[Display reverse: (old extent: 10@10).
				Display reverse: ((old := new) extent: 10@10)]]
"
! !


!Morph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 18:49:29'!
stepAt: millisecondSinceLast
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. 
	The desired time between steps is specified by this morph's answer to the stepTime message.
	The millisecondSinceLast parameter gives the time elapsed since the previous step."
	"
	m := BoxedMorph new.
	m color: Color random.
	m openInWorld.
	m morphPosition: 10@10.
	t := 0.
	m when: #morphicStep evaluate: [ :delta |
		t := t + delta.
		t < 10000
			ifTrue: [
				(m owner is: #HandMorph) ifFalse: [
					m morphPosition: 3@2 * t // 100 ]]
			ifFalse: [ m stopStepping ]].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondSinceLast! !


!PlacedMorph methodsFor: 'private' stamp: 'jmv 9/14/2021 18:26:53'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner := owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition := self morphPositionInWorld ].
	owner := aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					location := location withTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent := owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]].
	self fixYAxisDirection.! !


!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
deleteBalloonTarget: aMorph
	"Delete any existing balloon help.  This is now done unconditionally, whether or not the morph supplied is the same as the current balloon target"
	
	self balloonHelp: nil

"	| h |
	h := self balloonHelp ifNil: [^ self].
	h balloonOwner == aMorph ifTrue: [self balloonHelp: nil]"! !


!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/30/2021 13:11:25'!
backgroundImageData: aByteArray
	"
	| filename |
	filename := 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename := 'bg/free-desktop-wallpaper.jpg'.
	filename := 'bg/jellyfish-thumb.jpg'.
	filename := 'bg/splash_by_beefpepsi.jpg'.
	filename := 'bg/gray ocean and pier.jpg'.
	filename := 'bg/newyork.jpg'.
	filename := 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename := 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	filename := 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	self runningWorld backgroundImageData: (FileStream readOnlyFileNamed: filename) binary contentsOfEntireFile.
	"
	backgroundImageData := aByteArray.
	self buildMagnifiedBackgroundImage! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/10/2023 16:36:31'!
doOneCycleOn: aMorphicCanvas delay: delay
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta := Time localMillisecondClock - lastCycleTime.
	r := 15@1515 extent: 60@30.
	Delta := Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	self doOneCycleNowOn: aMorphicCanvas.

	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause := 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait := 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause := 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause := pause * 21//20 ] ].
			waitUntil := lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil := waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil := waitUntil min: alarms first scheduledTime ]].
			wait := waitUntil - Time localMillisecondClock max: 0 ].
	(Preferences at: #serverMode)
		ifTrue: [ wait := wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			delay setDelay: wait; wait ].! !


!IndentingListItemMorph methodsFor: 'private-container protocol' stamp: 'jmv 9/4/2012 13:36'!
toggleExpandedState
	| newChildren toDelete c |
	isExpanded := isExpanded not.
	toDelete := OrderedCollection new.
	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode |
			aNode recursiveAddTo: toDelete ]].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [ complexContents hasContents ]) ifFalse: [
		firstChild := nil.
"	 	nextSibling := firstChild := nil."
		^ self redrawNeeded ].
	(c := complexContents contents) isEmpty ifTrue: [ ^ self redrawNeeded ].
	newChildren := container
		addSubmorphsAfter: self
		fromCollection: c
		allowSorting: true.
	firstChild := newChildren first! !


!MenuMorph methodsFor: 'modal control' stamp: 'hlsf 6/24/2022 09:35:29'!
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: (Preferences at: #menuKeyboardControl)

	"
	| menu sub entry |
	menu := MenuMorph new.
	1 to: 3 do: [:i |
		entry := 'Line', i printString.
		sub := MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				action: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
	"! !


!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 12/7/2022 09:13:42'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	^ aString asUnicodeString displayOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !


!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:47:19'!
experiment1
	"
	Not perfect, but can be compared with
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	BTW, this is so much faster...
	VectorCanvas experiment1
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 e f |
canvas := self onForm: Display.
h := 300.
h := 490.
h := 280.
bottom := 200.
bottom := 0.
centerX := 400.
centerX := 250.
w0 := 120.0.
hf := 1.0.
hf := 1.73.
hf := 1.71.
y := nil.
delta := nil.
	e := 500@500.
	f := Form extent: e depth: 32.
	f fillColor: Color transparent..
"	canvas := self on: f.
	canvas useComponentAlphas."
1 to: "400" 150 do: [ :i |
	i print.
	prevDelta := delta.
	delta := h / (w0 * i + h).
	prevY := y.
	y := bottom + (delta * hf * h).
	prevY ifNotNil: [
"		-40 to: 40 do: [ :ii |"
		-3 // delta to: 3 // delta do: [ :ii |
			x1p := centerX - (prevDelta * w0 * (ii-1)).
			x2p := centerX - (prevDelta * w0 * ii).
			x1 := centerX - (delta * w0 * (ii-1)).
			x2 := centerX - (delta * w0 * ii).
			color := i + ii \\ 2 = 0 ifTrue: [ Color black ] ifFalse: [ Color white ].
"			color := color alpha: (((prevY-y) min: (x1-x2)) min: 1)."
"			color := color alpha: 0.5."
			canvas fillColor: color do: [
				canvas
					moveTo: x1 @ y;
					lineTo: x2 @ y;
					lineTo: x2p @ prevY;
					lineTo: x1p @ prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
"
	canvas removeSpuriousTranslucencyFloatAndBlendOver: Color red.
	f writePNGfileNamed: 'pp17.png'
"! !

!AbstractVectorCanvas class methodsFor: 'samples' stamp: 'jmv 10/8/2021 09:47:21'!
experiment2
	"
	VectorCanvas experiment2
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 |
canvas := self onForm: Display.
h := 300.
h := 600.
bottom := 200.
centerX := 400.
w0 := 60.0.
hf := 1.0.
y := nil.
delta := nil.
1 to: 400 do: [ :i |
	i print.
	prevDelta := delta.
	delta := h / (w0 * i + h).
	prevY := y.
	y := bottom + (delta * hf * h).
	prevY ifNotNil: [
"		-40 to: 40 do: [ :ii |"
		-6 // delta to: 6 // delta do: [ :ii |
			x1p := centerX - (prevDelta * w0 * (ii-1)).
			x2p := centerX - (prevDelta * w0 * ii).
			x1 := centerX - (delta * w0 * (ii-1)).
			x2 := centerX - (delta * w0 * ii).
			color := i + ii \\ 2 = 0 ifTrue: [ Color white ] ifFalse: [ Color black ].
			color := color alpha: (((prevY-y) min: (x1-x2)) "* 0.3" min: 1).
"			color := color alpha: 0.5."
			canvas fillColor: color do: [
				canvas
					moveTo: x1 @ y;
					lineTo: x2 @ y;
					lineTo: x2p @ prevY;
					lineTo: x1p @ prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
! !


!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/7/2022 09:13:45'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	| p1 answer |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	currentTransformation isPureTranslation ifTrue: [
		(Preferences at: #cacheTrueTypeGlyphs) ifTrue: [
			p1 := currentTransformation transform: aPoint roundedHAFZ.
			p1 := p1 + (0@(0 - (aTrueTypeFont ascent + aTrueTypeFont lineGap-1))).
			p1 := p1 roundedHAFZ.
			answer := aString asUnicodeString displayOnBitBltCanvasEngine: auxBitBltEngine
				from: firstIndex to: lastIndex at: p1 font: aTrueTypeFont color: aColor.
			answer ifNotNil: [
				engine updateMorphBoundsLeft: p1 x top: p1 y
					right: answer x bottom: answer y ].
			^answer ]].

	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 5/4/2023 16:20:14'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	
	tallyRoot := QSystemTally new class: thisContext receiver class method: thisContext method nesting: 0.
	semaphore := Semaphore new.
	"Try to get 10 samples per msec... Not really sure how this parameter is used, nor the meaning and relevance of #interruptChecksPerMSec"
	"ticks := Time highResTimerTicksPerMillisecond // Smalltalk interruptChecksPerMSec."
	ticks := Time highResTimerTicksPerMillisecond // 10.
	vmStats := Smalltalk getVMParameters.
	startTime := Time localMillisecondClock.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !


!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'NPM 3/16/2020 23:09:03'!
computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: aCollection

	| selectorsToShow |
	selectorsToShow := OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
				selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow.
				(Object canUnderstand: selector) ifFalse: [ aCollection add: selector ]]].
				
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow :=  self sortByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries := selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	^ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	
! !

UnicodeCodePoint initialize!
