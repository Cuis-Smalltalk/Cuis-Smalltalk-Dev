'From Cuis7.7 [latest update: #7970] on 29 May 2026 at 1:11:35 pm'!

!Parser methodsFor: 'error correction' stamp: 'HAW 12/Dec/2019 17:44:22'!
newRangeRemovingTempsDeclarationFrom: currentSource 
	startingAt: start
	to: originalEnd
	
	| end |
	
	end := self filterSeparatorsToTheRightOn: currentSource startingAt: originalEnd.
	
	(currentSource at: end+1) == $| ifTrue: [ | possibleNewStart |
		possibleNewStart := (self filterSeparatorsToTheLeftOn: currentSource startingAt: start) - 1.
		(currentSource at: possibleNewStart) == $| ifTrue: [ ^ {possibleNewStart. end + 1} ]]. 
	
	 ^ {start. end}! !


!MessageNode methodsFor: 'code generation' stamp: 'nice 20/Feb/2012 20:49:00'!
sizeCodeForToDo: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt test block incStmt blockSize initSize limitInit |
	block := arguments at: 3.
	initStmt := arguments at: 4.
	test := arguments at: 5.
	incStmt := arguments at: 6.
	limitInit := arguments at: 7.
	initSize := forValue
		ifTrue: [initStmt sizeCodeForValue: encoder.]
		ifFalse: [initStmt sizeCodeForEffect: encoder].
	limitInit == nil ifFalse:
		[initSize := initSize + (limitInit sizeCodeForEffect: encoder)].
	blockSize := (block sizeCodeForEvaluatedEffect: encoder)
			+ (incStmt sizeCodeForEffect: encoder)
			+ (encoder sizeJumpLong: -1).
	loopSize := (test sizeCodeForValue: encoder)
			+ (self sizeCode: encoder forBranchOn: false dist: blockSize)
			+ blockSize.
	sizes := {blockSize. loopSize}.
	^initSize
	+ loopSize! !

!MessageNode methodsFor: 'code generation' stamp: 'eem 5/Nov/2012 16:02:00'!
sizeCodeForWhile: encoder value: forValue 
	"L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only);
	justStmt, wholeLoop, justJump."
	| cond stmt stmtSize loopSize branchSize |
	cond := receiver.
	stmt := arguments at: 1.
	"We assume long backward branches are always maximal size branches."
	stmtSize := (stmt sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	branchSize := self
					sizeCode: encoder
					forBranchOn: selector key == #whileFalse:  "Btp for whileFalse"
					dist: stmtSize.
	loopSize := (cond sizeCodeForEvaluatedValue: encoder) + branchSize + stmtSize.
	sizes := {stmtSize. loopSize}.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])! !

!MessageNode methodsFor: 'code generation (new scheme)' stamp: 'eem 14/May/2008 17:15:00'!
sizeCodeForIf: encoder value: forValue
	| thenExpr elseExpr branchSize thenSize elseSize |
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	(forValue
	 or: [(thenExpr isJust: NodeNil)
	 or: [elseExpr isJust: NodeNil]]) not
			"(...not ifTrue: avoids using ifFalse: alone during this compile)"
		ifTrue:  "Two-armed IFs forEffect share a single pop"
			[^super sizeCodeForEffect: encoder].
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[elseSize := elseExpr sizeCodeForEvaluatedValue: encoder.
			thenSize := (thenExpr sizeCodeForEvaluatedValue: encoder)
					+ (thenExpr returns
						ifTrue: [0]  "Elide jump over else after a return"
						ifFalse: [self sizeCode: encoder forJump: elseSize]).
			branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[(elseExpr isJust: NodeNil)
				ifTrue:
					[elseSize := 0.
					thenSize := thenExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
				ifFalse:
					[thenSize := 0.
					elseSize := elseExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: true dist: elseSize]].
	sizes := {thenSize. elseSize}.
	^(receiver sizeCodeForValue: encoder)
	+ branchSize + thenSize + elseSize! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 10/Mar/2018 22:17:39'!
readColorMap
	"Read colorCount BMP color map entries from the given binary stream. Answer an array of Colors."
	| colorCount colors maxLevel b g r ccStream |
	colorCount := (bfOffBits - 54) // 4.
	"Note: some programs (e.g. Photoshop 4.0) apparently do not set colorCount; assume that any data between the end of the header and the start of the pixel data is the color map"
	biBitCount >= 16 ifTrue:[^nil].
	colorCount = 0 ifTrue: [ "this BMP file does not have a color map"
		"default monochrome color map"
		biBitCount = 1 ifTrue: [^ `{Color white. Color black}`].
		"default gray-scale color map"
		maxLevel := (2 raisedTo: biBitCount) - 1.
		^ (0 to: maxLevel) collect: [:level | Color gray: (level asFloat / maxLevel)]].
	ccStream := ReadStream on: (stream next: colorCount*4).
	colors := Array new: colorCount.
	1 to: colorCount do: [:i |
		b := ccStream next.
		g := ccStream next.
		r := ccStream next.
		ccStream next. "skip reserved"
		colors at: i put: (Color r: r g: g b: b range: 255)].
	^ colors
! !


!WarpBlt class methodsFor: 'form rotation' stamp: 'jmv 29/Oct/2021 10:52:41'!
rotate: srcForm degrees: angleInDegrees center: aPoint scaleBy: scalePoint smoothing: cellSize
	"Rotate the given Form the given number of degrees about the given center and scale its width and height by x and y of the given scale point. Smooth using the given cell size, an integer between 1 and 3, where 1 means no smoothing. Return a pair where the first element is the rotated Form and the second is the position offset required to align the center of the rotated Form with that of the original. Note that the dimensions of the resulting Form generally differ from those of the original."

	| srcRect center radians dstOrigin dstCorner p dstRect inverseScale quad dstForm newCenter warpSrc |
	srcRect := srcForm boundingBox.
	center := srcRect center.
	radians := angleInDegrees degreesToRadians.
	dstOrigin := dstCorner := center.
	srcRect corners do: [:corner |
		"find the limits of a rectangle that just encloses the rotated
		 original; in general, this rectangle will be larger than the
		 original (e.g., consider a square rotated by 45 degrees)"
		p := ((corner - center) scaledBy: scalePoint) + center.
		p := (p inverseRotatedBy: radians about: center) roundedHAFZ.
		dstOrigin := dstOrigin min: p.
		dstCorner := dstCorner max: p].

	"rotate the enclosing rectangle back to get the source quadrilateral"
	dstRect := dstOrigin corner: dstCorner.
	inverseScale := (1.0 / scalePoint x)@(1.0 / scalePoint y).
	quad := dstRect innerCorners collect: [:corner |
		p := corner inverseRotatedBy: radians negated about: center.
		((p - center) scaledBy: inverseScale) + center].

	"make a Form to hold the result and do the rotation"
	warpSrc := srcForm.
	(srcForm is: #ColorForm)
		ifTrue: [
			cellSize > 1 | true "ar 12/27/2001: Always enable - else sketches won't work"
				ifTrue: [
					warpSrc := Form extent: srcForm extent depth: 16.
					srcForm displayOn: warpSrc.
					dstForm := Form extent: dstRect extent depth: 16]  "use 16-bit depth to allow smoothing"
				ifFalse: [
					dstForm := srcForm class extent: dstRect extent depth: srcForm depth]]
		ifFalse: [
			dstForm := srcForm class extent: dstRect extent depth: srcForm depth].

	(WarpBlt toForm: dstForm)
		sourceForm: warpSrc;
		colorMap: (warpSrc colormapIfNeededFor: dstForm);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form paint;
		copyQuad: quad toRect: dstForm boundingBox.

	(dstForm is: #ColorForm) ifTrue: [ dstForm colors: srcForm colors copy ].
	newCenter := (center inverseRotatedBy: radians about: aPoint) truncated.
	^ {dstForm. dstRect origin + (newCenter - center)}! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'di 17/Feb/2000 22:17:00'!
induceMDFault
	"Stache a copy of the methodDict in the organization slot (hack!!),
	and set the methodDict to nil.  This will induce an MD fault on any message send.
	See: ClassDescription>>recoverFromMDFault
	and ImageSegment>>discoverActiveClasses."

	organization := {methodDict. organization}.
	methodDict := nil.
	self flushCache! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 24/May/2014 10:59:00'!
changeFromCategorySpecs: categorySpecs 
	"Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane."

	| oldElements newElements newCategories newStops currentStop temp cc catSpec |
	oldElements := elementArray asSet.
	newCategories := Array new: categorySpecs size.
	newStops := Array new: categorySpecs size.
	currentStop := 0.
	newElements := WriteStream on: (Array new: 16).
	1 to: categorySpecs size do: [ :i | 
		catSpec := categorySpecs at: i.
		newCategories at: i put: catSpec first asSymbol.
		catSpec allButFirst asArray sort do:
			[:elem |
			(oldElements remove: elem ifAbsent: nil) ifNotNil: [
				newElements nextPut: elem.
				currentStop := currentStop+1]].
		newStops at: i put: currentStop].

	"Ignore extra elements but don't lose any existing elements!!"
	oldElements := oldElements collect: [ :elem | {self categoryOfElement: elem. elem}].
	newElements := newElements contents.
	categoryArray := newCategories.
	(cc := categoryArray asSet) size = categoryArray size ifFalse: [ "has duplicate element"
		temp := categoryArray asOrderedCollection.
		temp removeAll: categoryArray asSet asOrderedCollection.
		temp do: [ :each | | dup ii |
			dup := each.
			ii := categoryArray indexOf: dup.
			[ dup := (dup,' #2') asSymbol.  cc includes: dup ] whileTrue.
			cc add: dup.
			categoryArray at: ii put: dup]].
	categoryStops := newStops.
	elementArray := newElements.
	oldElements do: [:pair | self classify: pair last under: pair first]! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 22/Feb/2024 13:49:25'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems.
	Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes entry msg lastQuitLogPosition snapshotChunk entryPathName |

	"Do not open source files if internalized (i.e. notNil)"
	sources := SourceFiles at: 1.
	sources ifNil: [
		entry := self defaultSourcesName asFullFileEntry.
		entry exists ifFalse: [
			entry := self alternativeSourcesName asFullFileEntry ].
		entry exists ifTrue: [
			sources := [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
				msg := 'Cuis cannot locate the sources file named ' , entry pathName, '.', String newLineString,
					'Please check that the file is properly named and is in the same directory as this image.'.
			self logStartupError: msg.
		].

	"Do not open source files if internalized (i.e. notNil)"
	changes := SourceFiles at: 2.
	changes ifNil: [
		entry := self defaultChangesName asFullFileEntry.
		entryPathName := entry pathName.
		"Read the last snapshot chunk. Validate that the .changes file is appropriate for this image."
		entry exists
			ifTrue: [
				lastQuitLogPosition := self lastQuitLogPosition.
				lastQuitLogPosition > 0 ifTrue: [
					entry readStreamDo: [ :changesFile |
						changesFile position: lastQuitLogPosition.
						snapshotChunk := changesFile nextChunk ].
					((snapshotChunk beginsWith: self tagHeader) and: [
						snapshotChunk includesSubString: 'priorSource: ']) ifFalse: [
							(Preferences at: #warnIfNoChangesFile) ifTrue: [
								self logStartupError:
									'Incorrect changes file: ', entryPathName, String newLineString,
									'Missing code will be decompiled', String newLineString,
									'New source code will not be saved' ].
							entry := nil ]]]
			ifFalse: [
				(Preferences at: #warnIfNoChangesFile) ifTrue: [
					self logStartupError:
						'Could not find changes file: ', entryPathName, String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved' ].
				entry := nil ].
		entry ifNotNil: [
			changes := [ entry appendStream ]
				on: FileWriteError
				do: [
					self logStartupError:
						'Could not write to changes file: ', entryPathName, String newLineString,
						'Changes file will not be used.', String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved'.
					nil ]]].
	ChangesInitialFileSize := changes ifNotNil: [ changes position ].

	SourceFiles := {sources. changes}.! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 11/Jul/2015 16:12:00'!
unusedMethods
	| classes unsent messageList cls |

	(cls := self selectedClass) ifNil: [^ nil].
	classes := {cls. cls class}.
	unsent := Set new.
	classes do: [:c | unsent addAll: c selectors].
	unsent := Smalltalk allUnSentMessagesIn: unsent.
	messageList := OrderedCollection new.
	classes do: [:c | (c selectors select: [:s | unsent includes: s]) asArray sort
					do: [:sel | messageList add: 
						(MethodReference class: c selector: sel) ]].
	^messageList! !


!Integer methodsFor: 'private' stamp: 'jmv 25/Jun/2019 17:58:27'!
digitDiv: arg neg: ng 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #digitDiv:neg: arguments: {arg.ng}].
	"TFEI added this line"
	l := self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ {0. self}].
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
	^ {quo. rem}! !


!TrieNode methodsFor: 'private' stamp: 'jmv 6/Jun/2022 09:33:06'!
childFor1: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have exactly one child."
	| nextSegmentStart nextSegmentFirst answer |

	nextSegmentStart := i + segmentSize.
	nextSegmentFirst := nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars = nextSegmentFirst ifTrue: [ ^ children].

	doAdd ifFalse: [ ^ nil ].
	
	answer := nextSegmentStart > aString size
		ifTrue: [ TrieLeaf new ]
		ifFalse: [ TrieNode someKey: aString segmentStart: nextSegmentStart ].

	"Just one child, if adding, convert references to a collection"
	nextSegmentFirst < childrenFirstChars
		ifTrue: [
			children := {answer . children }.
			childrenFirstChars := {nextSegmentFirst. childrenFirstChars} ]
		ifFalse: [
			children := {children . answer}.
			childrenFirstChars := {childrenFirstChars. nextSegmentFirst} ].
	^answer! !


!TextAction methodsFor: 'as yet unclassified' stamp: 'tk 16/Dec/1997 16:44:00'!
analyze: aString
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param := self validate: aString.
		^ {param. param size = 0 ifTrue: [nil] ifFalse: [param]}].
	"Two parts"
	trim := aString withBlanksTrimmed.
	trim first == $< 
		ifTrue: [ trim last == $>
			ifTrue: ["only instructions" 
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] ifFalse: [param]]
			ifFalse: ["at the front"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [ trim last == $>
			ifTrue: ["at the end"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show := nil]].
	^ {param. show}! !


!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 17/Apr/2015 16:00:00'!
computeLcsAt: i at: j
	| mij cij pair left up |
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		cij := self lcsAt: i - 1 at: j - 1.
		pair := {i. j}.
		^cij collect: [:s | s copyWith: pair]].
	mij = self class u ifTrue: [^self lcsAt: i - 1 at: j].
	mij = self class l ifTrue: [^self lcsAt: i at: j - 1].
	mij = self class ul ifTrue: [
		left := self lcsAt: i at: j - 1.
		up := self lcsAt: i - 1 at: j.
		^left copy addAll: up; yourself].
	self assert: false! !

