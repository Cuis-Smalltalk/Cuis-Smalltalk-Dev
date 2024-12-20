'From Cuis7.3 [latest update: #6938] on 20 December 2024 at 4:50:15 pm'!

!PathLineToCommand methodsFor: 'building' stamp: 'jmv 12/19/2024 11:44:34'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream.
	points := Array streamContents: [ :strm | | px py |
		[px := aStream nextNumber.
		py := aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]! !


!PathLineToHCommand methodsFor: 'building' stamp: 'jmv 12/19/2024 11:44:43'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream.
	xs := Array streamContents: [ :strm | | px |
		[px := aStream nextNumber.
		px notNil ] whileTrue:
			[ strm nextPut: px ]]! !


!PathLineToVCommand methodsFor: 'building' stamp: 'jmv 12/19/2024 11:45:11'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream.
	ys := Array streamContents: [ :strm | | py |
		[py := aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: py ]]! !


!PathMoveToCommand methodsFor: 'building' stamp: 'jmv 12/19/2024 11:45:17'!
buildFrom: commandChar stream: aStream

	super buildFrom: commandChar stream: aStream.
	points := Array streamContents: [ :strm | | px py |
		[px := aStream nextNumber.
		py := aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 12/20/2024 16:37:37'!
scanCharactersFrom: startIndex to: stopIndex in: anUnicodeString rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	lastIndex := startIndex.
	anUnicodeString from: startIndex to: stopIndex doWithUtf8Bytes: [ :byte1 :byte2 :byte3 :byte4 |
		| nextDestX |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil).
		It is OK to just use the first byte in UTF-8"
		(stops at: byte1 + 1) ifNotNil: [ :stop | ^stop ].
		nextDestX := destX + (font widthOfUtf8Byte1: byte1 b2: byte2 b3: byte3 b4: byte4).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX := nextDestX.
			lastIndex := lastIndex + 1 ].
	lastIndex := stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!TTContourConstruction methodsFor: 'primitive format' stamp: 'jmv 12/20/2024 16:38:48'!
addContourDataTo: stream tx: aGeometryTransformationOrNil
	| bezierCount firstPoint prevEndPoint |
	self isEmpty ifTrue: [
		^self ].

	"This bogus iteration could perhaps be avoided at the cost of extra memory."
	bezierCount := 0.
	self
		startPointDo: [ :firstBezierStart | ]
		quadraticBeziersDo: [ :controlPoint :endPoint | bezierCount := bezierCount + 1 ].
	stream nextPut: bezierCount.

	self
		startPointDo: [ :firstBezierStart |
			firstPoint := firstBezierStart.
			aGeometryTransformationOrNil notNil ifTrue: [
				firstPoint := aGeometryTransformationOrNil transform: firstPoint ].
			stream nextPut: firstPoint x; nextPut: firstPoint y.
			prevEndPoint := firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			| cp ep cpr epr |
			cp := controlPoint.
			ep := endPoint.
			aGeometryTransformationOrNil notNil ifTrue: [
				cp := aGeometryTransformationOrNil transform: cp.
				ep := aGeometryTransformationOrNil transform: ep ].
			cpr := cp - prevEndPoint.
			epr := ep - prevEndPoint.
			"Make control point distict from start and end points for the benefit of VectorEnginePlugin"
			(cpr = `0.0@0.0` or: [ cpr = epr ]) ifTrue: [
				cpr := epr * 0.5 ].
			stream nextPut: epr x; nextPut: epr y.
			stream nextPut: cpr x; nextPut: cpr y.
			prevEndPoint := ep ].! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 12/19/2024 11:51:50'!
stepAt: millisecondSinceLast

	"If there's anything in my monitor list, see if the strings have changed."
	| changes |
	changes := false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [ | string |
			k refresh.
			(string := k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes := true ]]].
	changes ifTrue: [ | sel |
		sel := currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 12/19/2024 11:52:50'!
dayOfYear
	"This code was contributed by Dan Ingalls. It is equivalent to the terser
		^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker."

	^ self dayMonthYearDo:
		[ :d :m :y | | monthStart |
			monthStart := #(1 32 60 91 121 152 182 213 244 274 305 335) at: m.
			(m > 2 and: [ Year isLeapYear: y ])
				ifTrue: [ monthStart + d ]
				ifFalse: [ monthStart + d - 1 ]]! !


!Float methodsFor: 'converting' stamp: 'jmv 12/20/2024 16:41:27'!
partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits |
		| sign exponent mantissa |

		"Extract the sign"
		sign := signBit = 0 ifTrue: [1] ifFalse: [-1].

		"Special cases: infinites and NaN"
		exponentBits = 16r7FF ifTrue: [
			^mantissaBits = 0
				ifTrue: [ infinityBlock valueWithPossibleArgument: self ]
				ifFalse: [ nanBlock valueWithPossibleArgument: self and: mantissaBits ]].

		"Unbias exponent: 16r3FF is bias"
		exponent := exponentBits - 16r3FF.
	
		exponentBits ~= 0
			ifTrue: [
				"Add back implicit leading 1 in fraction."
				mantissa := 16r0010000000000000 bitOr: mantissaBits ]
			ifFalse: [
				"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa."
				mantissa := mantissaBits.
				exponent := exponent + 1 ].

		"Evaluate the block"
		aThreeArgumentBlock value: sign value: exponent value: mantissa
	]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/19/2024 11:54:47'!
floatsAwayFrom: aFloat

	| count2 count1 |
	(self isNaN or: [ aFloat isNaN ]) ifTrue: [ ^ Float nan ].
	count2 := self partBits: [:s :e :m | (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	count1 := aFloat partBits: [:s :e :m | (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	^count2 - count1! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/20/2024 16:44:16'!
nextAwayFromZero
	"Answer the Float with smallest magnitude but larger than ours, with the same sign
	Only for finite numbers."

	^ self partValues: [ :sign :myExponent :myMantissa |
		| exponent mantissa |
		myMantissa = 16r1FFFFFFFFFFFFF
			ifTrue: [
				mantissa := 16r10000000000000.
				exponent := myExponent +1 ]
			ifFalse: [
				mantissa := myMantissa+1.
				exponent := myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/20/2024 16:45:10'!
nextTowardsZero
	"Answer the Float with largest magnitude but smaller than ours, with the same sign.
	Only for finite, non zero numbers."

	^ self partValues: [ :sign :myExponent :myMantissa |
		| exponent mantissa |
		myMantissa isZero ifTrue: [ self error: 'Not for zero values' ].
		(myMantissa = 16r10000000000000 and: [myExponent > Float emin])
			ifTrue: [
				mantissa := 16r1FFFFFFFFFFFFF.
				exponent := myExponent -1 ]
			ifFalse: [
				mantissa := myMantissa-1.
				exponent := myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !


!Fraction methodsFor: 'printing' stamp: 'jmv 12/20/2024 16:46:07'!
storeOn: aStream
	
	"If possible, store as a literal of the form 9999e-99
	If denominator's prime factors are just 2 and 5, then we can be printed as a literal
	of the form 999999e-99"
	"
	123e-12 storeString
	123e-12 printString
	123e-12000 storeString
	123e-12000 printString
	(3/4) storeString
	(3/4) printString
	(-3/4) storeString
	(-3/4) printString
	(1/3) storeString
	(1/3) printString
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			| e f |
			exponent2 > exponent5
				ifTrue: [
					e := exponent2.
					f := 5 raisedToInteger: e-exponent5 ]
				ifFalse: [
					e := exponent5.
					f := 2 bitShift: e-exponent2-1 ].
			numerator*f storeOn: aStream base: 10.
			aStream nextPut: $e; nextPut: $-.
			e storeOn: aStream base: 10 ]
		otherwise: [
			super storeOn: aStream ]
! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 12/20/2024 16:48:50'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount widths widthToAllocate leftOver x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	widthToAllocate := boundsForLayout width - (gapCount * gap).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * self axisEdgeWeight).

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		| height y |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	].! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 12/20/2024 16:49:11'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount heights heightToAllocate leftOver y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	heightToAllocate := boundsForLayout height - (gapCount * gap).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * self axisEdgeWeight).

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		| width x |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	].! !


!TextComposition methodsFor: 'editing' stamp: 'jmv 12/20/2024 16:31:46'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| action startBlock t |
	action := false.
	startBlock := self characterBlockAtPoint: clickPoint.
	t := model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue: [ | target range boxes box |
				(target := model) ifNil: [ target := editor morph].
				range := t rangeOf: att startingAt: startBlock stringIndex.
				boxes := self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box := boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box := editor morph displayBounds.
					editor morph allOwnersDo: [ :m | box := box intersect: (m displayBounds) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action := true]].
				]]].
	^ action! !


!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 12/20/2024 16:32:24'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in senders for more details"

	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		| baseName version i j |
		baseName := b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) = $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]! !


!CodePackage methodsFor: 'listing' stamp: 'jmv 12/19/2024 12:47:06'!
sortedExtensionMethodsDo: aBlock displayingProgress: aString
	"Include both class and instance methods we define, for classes we don't define."
	| externalClasses |
	externalClasses := self externalClasses.
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: externalClasses size
		during: [ :barBlock |
			externalClasses withIndexDo: [ :classOrMetaClass :i | | methods |
				barBlock value: i.
				methods := Array streamContents: [ :stream |
					(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
						self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
							stream nextPut: m ]]].
				methods sort: [ :a :b |
					a methodSymbol < b methodSymbol ].
				methods do: aBlock.
				]
			]! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 12/19/2024 12:46:23'!
named: aString createIfAbsent: neverAnswerNil registerIfNew: doRegisterIfJustCreated
	"Answer the instance with name aString.
	If there's no package named aString, answer nil or a new instance as requested.
	If a new instance is created, register it or not as requested.
	CodePackage named: 'Morphic' createIfAbsent: false registerIfNew: false
	CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false
	CodePackage named: 'XXXXXTest' createIfAbsent: true registerIfNew: true
	"

	| newInstance |
	^self installedPackages
		at: aString
		ifAbsent: [
			neverAnswerNil ifTrue: [ 
				newInstance := self new packageName: aString.
				doRegisterIfJustCreated ifTrue: [ self register: newInstance ].
				newInstance ]].! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 12/19/2024 12:47:11'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName |
	pathName ifNotNil: [
		pathName asFullFileEntry exists ifTrue: [ ^ true ]].
	packageFileName := self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory | | entry |
		entry := directory // packageFileName.
		entry exists ifTrue: [
			"Try this one. If success, keep it."
			self pathName: entry pathName.
			self checkRequirement ifTrue: [ ^true ].
			"Nope. Don't keep it."
			self pathName: nil ]].
	^ false! !


!Clipboard methodsFor: 'private' stamp: 'jmv 12/19/2024 12:47:24'!
extendedClipboardInterface
	"Answer a subinstance of ExtendedClipboardInterface, if present and operational.
	Clipboard default extendedClipboardInterface
	"
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |
		| interface |
		interface := clipboardInterface current.
		interface isOperational ifTrue: [
			^ interface ]].
	"No operational extended clipboard."
	^nil! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 12/19/2024 12:47:33'!
installHaloPreferencesWith: anArray
	^ self sysPreferences 
		name: #haloSpecifications 
		description: 'Halo specifications describing which halos are to be used, what they should look like, and where they should be situated'
		category: #gui
		type: Array
		value: (anArray collect: [ :each | | aColor |
			aColor := Color.
			each fourth do: [ :sel | aColor := aColor perform: sel].
			HaloSpec new 
				horizontalPlacement: each second
				verticalPlacement: each third 
				color: aColor
				iconSymbol: each fifth
				addHandleSelector: each first
				hoverHelp: each sixth])! !

!PreferenceSet class methodsFor: 'sys fonts' stamp: 'jmv 12/19/2024 12:47:47'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec

	defaultFontsSpec do: [ :triplet | | font |
		font := FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font := FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [			font := font emphasized: triplet third ].
		self sysPreferences at: triplet first put: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/19/2024 12:47:39'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [ | line parts |
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[ | pref value |
					pref := Preferences instanceAt: parts first asSymbol.
					value := parts second.
					value first = $# ifTrue: [ value := (value withoutPrefix: '#') asSymbol ].
					pref value: value.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !

