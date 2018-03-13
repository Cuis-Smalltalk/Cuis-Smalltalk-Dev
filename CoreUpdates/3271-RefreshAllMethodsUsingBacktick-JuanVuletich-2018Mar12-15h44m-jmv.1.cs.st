'From Cuis 5.0 of 7 November 2016 [latest update: #3269] on 12 March 2018 at 3:58:27 pm'!

!Object methodsFor: 'private' stamp: 'jmv 3/12/2018 15:54:19'!
primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context emergencyEvaluator lines r |
	r _ `10@10` extent: (Display extent -20 min: `700@1000`).
	lines _ r height // AbstractFont default height.
	emergencyEvaluator _ Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	context _ thisContext sender sender.
	(30 min: lines - 10) timesRepeat: [context ifNotNil: [emergencyEvaluator print: (context _ context sender); newLine]].
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint! !


!Character methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:47:49'!
withDiacriticalMark: anUnicodeCodePoint
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-15, answer the receiver.
	Supported diacritical marks are:
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
		$a withDiacriticalMark: 16r301
		$N withDiacriticalMark: $~
		$Z withDiacriticalMark: $v
		
		invalid:
		$9 withDiacriticalMark:$v
		$A withDiacriticalMark: $v
		$¡ withDiacriticalMark: $v
		$A withDiacriticalMark: 1244
		"
	| answer i |
	i _ ((anUnicodeCodePoint isNumber
			ifTrue: [#(16r300 16r301 16r302 16r303 16r308 16r30A 16r30C)]
			ifFalse: [#($` $' $^ $~ $" $∞ $v)]) indexOf: anUnicodeCodePoint
				) + 1.
	answer _ (Character accentedLetters detect: [ :group | group first = self ] ifNone: [ ^self ]) at: i.
	^answer = $- ifFalse: [answer] ifTrue: [self]! !


!InputSensor methodsFor: 'private' stamp: 'jmv 3/12/2018 15:52:20'!
primMousePt
	"Primitive. Poll the mouse to find out its position. Return a Point. Fail if
	event-driven tracking is used instead of polling. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 90>
	^ `0@0`! !


!EventSensor methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:48:41'!
initialize
	"Run the I/O process"
	mouseButtons _ 0.
	mousePosition _ `0@0`.
	self setInterruptKey: (interruptKey ifNil: [$. numericValue bitOr: 16r0800 ]). 	"cmd-."
	interruptSemaphore _ (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	self flushAllButDandDEvents.
	inputSemaphore _ Semaphore new.
	hasInputSemaphore _ false.

	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	self installInterruptWatcher.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore _ false.
	inputSemaphore initSignals! !


!String methodsFor: 'displaying' stamp: 'jmv 3/12/2018 15:56:28'!
displayOn: aDisplayMedium
	"Display the receiver on the given DisplayMedium.  5/16/96 sw"

	self displayOn: aDisplayMedium at: `0 @ 0`! !


!Bitmap methodsFor: 'as yet unclassified' stamp: 'jmv 3/12/2018 15:47:42'!
asByteArray
	"Faster way to make a byte array from me.
	copyFromByteArray:, if receiver is BigEndian makes equal Bitmap.
	Assume receiver bytes-in-word mapping is BigEndian:
		Most significant bye of first word in self goes to first position in result.
	This means that for a BigEndian 8bpp Form, pixels are in the right order in the ByteArray
	
	Form lena asGrayForm bits asByteArray copyFrom: 1 to: 4.
	(Form lena asGrayForm asFormOfDepth: 8) bits asByteArray copyFrom: 1 to: 4.
	(0 to: 3) collect: [ :x | ((Form lena asGrayForm colorAt: x@0) luminance * 255) rounded ].
	"
	| f bytes hack |
	f _ Form extent: 4@self size depth: 8 bits: self.
	bytes _ ByteArray new: self size * 4.
	hack _ Form new hackBits: bytes.
	Smalltalk isLittleEndian ifTrue: [hack swapEndianness].
	hack copyBits: f boundingBox
		from: f
		at: `0@0`
		clippingBox: hack boundingBox
		rule: Form over.

	"f displayOn: hack."
	^ bytes! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 3/12/2018 15:57:40'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			`0@0` + (0@(d+AbstractFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (`4@4` + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue.
	DisplayScreen screenUpdateRequired: nil! !


!Transcripter class methodsFor: 'utilities' stamp: 'jmv 3/12/2018 15:57:42'!
emergencyEvaluator
	(Transcripter newInFrame: `0@0 corner: 320@200`)
		show: 'Type ''exit'' to exit the emergency evaluator.';
		readEvalPrint! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 3/12/2018 15:55:35'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 prevContents |
	f _ AbstractFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ `0@0` extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	prevContents _ Form fromDisplay: outerFrame.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + `2@2` extent: filledWidth@17).
			filledWidth -200 > oldFilledWidth2
				ifFalse: [
					"Usually just request an update, to be done asynchronously."
					DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [
					"Once in a while, force a real screen update (warning: really slow on MacOS if done too often)"
					Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	prevContents displayAt: outerFrame topLeft.
	self resume: result! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 3/12/2018 15:56:15'!
xBacktick
	"Smalltalk code evaluated at compile time as a literal."

	self step.
	buffer reset.
	[hereChar == $`
		and: [aheadChar == $`
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = Scanner doItCharacterValue asCharacter and: [source atEnd])
				ifTrue: [^self offEnd: 'Unmatched back quote']].
	self step.
	[ | compiler method methodNode |
		compiler _ Compiler new.
		methodNode _ compiler compileNoPattern: buffer contents in: UndefinedObject context: nil notifying: nil ifFail: [].
		method _ methodNode generate.
		"Grab all messages sent while evaluating literal, so the main method will be shown as senders of them"
		sentInLiterals addAll: method messages.
		"Evaluate now."
		token _ nil withArgs: #() executeMethod: method.
	] 
		on: SyntaxErrorNotification, UndeclaredVariableReference, Error
		do: [ :ex |
			ex class caseOf: {
				[ SyntaxErrorNotification ] -> [
						self notify: 'Can not compile: ', ex errorMessage at: mark].
				[ UndeclaredVariableReference ] -> [ 
						self notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: mark ]
			} otherwise: [
						self notify: 'Can not evaluate code: ', ex description at: mark ]].
	tokenType _ #literal! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 3/12/2018 15:56:19'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable atAll:  '!!%&+-*/\±◊˜¨≠Ø,<=>îïñóòôöõúù´ª?@~ÄÅÇÉéèêëíìûü∞∑' asByteArray put: #xBinary.

	newTable at: Scanner doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!LiteralNode methodsFor: 'printing' stamp: 'jmv 3/12/2018 15:52:43'!
printOn: aStream indent: level
	key isVariableBinding
		ifTrue: [
			key key isNil
				ifTrue: [
					aStream
						nextPutAll: '###';
						nextPutAll: key value soleInstance name ]
				ifFalse: [
					aStream
						nextPutAll: '##';
						nextPutAll: key key ]]
		ifFalse: [
			key isLiteral
				ifTrue: [ | isComplex |
					isComplex := false.
					key isArray ifTrue: [
						isComplex := key anySatisfy: [ :ea |
							ea isArray ]].
					"Is it complex? (i.e. array of arrays)"
					isComplex
						ifTrue: [
							aStream
								nextPut: $#;
								nextPut: $(.
							key do: [ :ea |
								aStream newLineTab: (1 max: level + 1).
								ea storeOn: aStream ].
							aStream newLineTab: (1 max: level).
							aStream nextPut: $) ]
						ifFalse: [ key storeOn: aStream ]]
				ifFalse: [
					"Need to generate code for stuff that is in a CompiledMethod literal
					but is not understood as a literal by the Compiler.
					Well, then it is because it was generated using backticks!!"
					aStream nextPut: $`.
					key storeOn: aStream.
					aStream nextPut: $` ]]! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 3/12/2018 15:57:29'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''`' at: which.
	right _ ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (String with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (String with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 3/12/2018 15:57:33'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	^#(
		#(		$( 	#enclose:					'Enclose within ( and ), or remove enclosing ( and )')
		#(		$[ 	#enclose:					'Enclose within [ and ], or remove enclosing [ and ]')
		#(		${ 	#enclose:					'Enclose within { and }, or remove enclosing { and }')
		#(		$' 	#enclose:					'Enclose within single quotes, or remove enclosing single quotes')
		#(		$" 	#enclose:					'Enclose within double quotes, or remove enclosing double quotes')
		#(		$` 	#enclose:					'Enclose within backticks, or remove enclosing backticks')
		#(		$< #enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$f 	#find:						'Find')
		#(		$g 	#findAgain:				'Find again')
		#(		$h 	#help:						'Open this help')
		#(		$j 	#setSearchString:		'Set selection as search string for find again')

		#(		$z 	#undo:						'Undo (multiple levels)')
		#(		$x 	#cut:							'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:						'Paste Clipboard contents')

		#(		$R	#indent:						'Indent (move selection one tab-stap right)')
		#(		$Y	#makeUppercase:		'Force selection to uppercase')
		#(		$U	#changeLineEndsToLf:	'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H	#cursorTopHome:		'Move cursor to start of text')
		#(		$L	#outdent:					'Outdent (move selection one tab-stop left)')

		#(		$Z	#redo:						'Redo (multiple levels)')
		#(		$X	#makeLowercase:		'Force selection to lowercase')
		#(		$C	#compareToClipboard:	'Compare argument to clipboard')

		#(		$ 	#selectWord:				'Select the current word as with double clicking')
	)! !


!SmalltalkEditor methodsFor: 'new selection' stamp: 'jmv 3/12/2018 15:56:25'!
selectWord
	"Select delimited text or word--the result of double-clicking."

	| leftDelimiters rightDelimiters |
	"Warning. Once me (jmv) added Character crCharacter to the delimiters, to make double-click at and of line select whole line.
	This had the bad effect that if a class name is the last word of a line, double-click would correctly select it, but after that,
	doing ctrl-b to browse it would select the whole line..."
	leftDelimiters _ '([{<|''"`'.
	rightDelimiters _ ')]}>|''"`'.
	^self selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 3/12/2018 15:55:41'!
staggerOffset
	^`6 @ 20`! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 3/12/2018 15:55:48'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| effectiveExtent width strips height grid allowedArea maxLevel |
	effectiveExtent _ self maximumUsableArea extent
					- (self scrollBarSetback @ self screenTopSetback).
	Preferences reverseWindowStagger ifTrue:
		["NOTE: following copied from strictlyStaggeredInitialFrameFor:"
		allowedArea _ self maximumUsableArea insetBy: (
			self scrollBarSetback @ self screenTopSetback extent: `0@0`
		).
		"Number to be staggered at each corner (less on small screens)"
		maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
		"Amount by which to stagger (less on small screens)"
		grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
		^ (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) min: `52@40` * Preferences standardCodeFont height ].
	width _ (strips _ self windowColumnsDesired) > 1
		ifTrue:
			[effectiveExtent x // strips]
		ifFalse:
			[(3 * effectiveExtent x) // 4].
	height _ (strips _ self windowRowsDesired) > 1
		ifTrue:
			[effectiveExtent y // strips]
		ifFalse:
			[(3 * effectiveExtent y) //4].
	^ width @ height

"
RealEstateAgent standardWindowExtent
"! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 3/12/2018 15:55:52'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: `0 @ 0`).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := `0 @ 0` extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w morphBoundsInWorld].
	otherFrames _ otherFrames reject: [ :f | f isNil ].
	0 to: maxLevel do: [ :level | 
			1 to: 4 do: [:ci | 
					cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
					corner := allowedArea perform: cornerSel.
					"The extra grid//2 in delta helps to keep title tabs distinct"
					delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
					1 to: ci - 1 do: [ :i | delta _ delta y negated @ delta x ].	"slow way"
					putativeCorner := corner + delta.
					free := true.
					otherFrames do: [ :w |
						free := free & ((w perform: cornerSel) ~= putativeCorner)].
					free 
						ifTrue: [
							^(initialFrame aligned: (initialFrame perform: cornerSel)
								with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea! !


!Form methodsFor: 'accessing' stamp: 'jmv 3/12/2018 15:50:15'!
offset
	^offset ifNil: [`0@0`]! !

!Form methodsFor: 'analyzing' stamp: 'jmv 3/12/2018 15:50:32'!
primCountBits
	"Count the non-zero pixels of this form."
	self depth > 8 ifTrue: [
		^(self asFormOfDepth: 8) primCountBits].
	^ (BitBlt toForm: self)
		fillColor: (Bitmap with: 0);
		destRect: (`0@0` extent: width@height);
		combinationRule: 32;
		copyBits! !

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
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"! !

!Form methodsFor: 'analyzing' stamp: 'jmv 3/12/2018 15:50:58'!
xTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by x-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice countBlt copyBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: 1@height.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: 1 @ slice height
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	^ (0 to: width-1) collect: [ :x |
		copyBlt sourceOrigin: x@0; copyBits.
		countBlt copyBits]! !

!Form methodsFor: 'analyzing' stamp: 'jmv 3/12/2018 15:51:05'!
yTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice copyBlt countBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: width@1.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	^ (0 to: height-1) collect: [ :y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits]! !

!Form methodsFor: 'coloring' stamp: 'jmv 3/12/2018 15:49:54'!
fillShape: aShapeForm fillColor: aColor
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	^ self fillShape: aShapeForm fillColor: aColor at: `0@0`! !

!Form methodsFor: 'coloring' stamp: 'jmv 3/12/2018 15:49:58'!
fillShape: aShapeForm fillColor: aColor at: location
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits! !

!Form methodsFor: 'color mapping' stamp: 'jmv 3/12/2018 15:50:08'!
mapColor: oldColor to: newColor
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	map _ (Color cachedColormapFrom: self depth to: self depth) copy.
	map at: (oldColor indexInMap: map) put: (newColor pixelWordForDepth: self depth).
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: `0@0`;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits! !

!Form methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:49:08'!
asFormOfDepth: d
	| newForm source |
	d = depth ifTrue: [ ^self ].
	source _ (self depth = 32 and: [ d abs < 32 ])
		ifTrue: [ self copy convertAlphaToZeroValueTransparency ]
		ifFalse: [ self ].
	newForm _ Form extent: source extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (source colormapIfNeededFor: newForm);
		copy: source boundingBox
		from: `0@0` in: source
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!!)"
	(newForm depth = 32 and: [self depth < 32]) ifTrue: [
		newForm fixAlpha ].
	^ newForm! !

!Form methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:49:11'!
asGrayForm
	| answer map |

	"For lower bit depths, the 15 bit ColorMap loses no information,
	and answers the real #luminance of each pixel."
	self depth < 32 ifTrue: [
		answer _ GrayForm extent: width@height.
		map _ self colormapIfNeededForGray8bpp.
		(BitBlt toForm: answer)
			colorMap: map;
			copy: self boundingBox
			from: `0@0` in: self
			fillColor: nil rule: Form over.
		answer offset: self offset.
		^ answer ].

	"For 32bpp, the approach below would use just 5bits per component.
	Generally it is best to extract a component in full 8bpp and keep full dynamic range.
	Green usually is a good choice."
	^ self asGrayForm: 3! !

!Form methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:49:14'!
asGrayForm: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a GrayForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit GrayForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayForm: componentIndex ].
	
	result _ GrayForm extent: width@height.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: (`0@0` extent: width@height);
		destOrigin: `0@0`;
		copyBits.
	^ result! !

!Form methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:49:21'!
asGrayScaleAndTransparent: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayScaleAndTransparent: componentIndex ].

	result _ ColorForm grayScaleAndTransparentExtent: width@height.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: (`0@0` extent: width@height);
		destOrigin: `0@0`;
		copyBits.

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	^ result! !

!Form methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:50:03'!
icon
	"Answer a 16 x 16 icon of myself"
	
	^self magnifyTo: `16 @ 16`! !

!Form methodsFor: 'copying' stamp: 'jmv 3/12/2018 15:49:28'!
contentsOfArea: aRect into: newForm
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	^ newForm copyBits: aRect from: self at: `0@0`
		clippingBox: newForm boundingBox rule: Form over! !

!Form methodsFor: 'copying' stamp: 'jmv 3/12/2018 15:49:31'!
copy: aRect
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	^ newForm copyBits: aRect from: self at: `0@0`
		clippingBox: newForm boundingBox rule: Form over! !

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
 | f f2 f3 | f _ Form fromUser. f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'display box access' stamp: 'jmv 3/12/2018 15:49:24'!
boundingBox
	^ Rectangle
		origin: `0 @ 0`
		corner: width @ height! !

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
 | f f2 f3 | f _ Form fromUser. f replaceColor: f dominantColor withColor: Color transparent.
f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 3/12/2018 15:49:40'!
displayOn: aDisplayMedium
	"Simple default display in order to see the receiver in the upper left 
	corner of screen."

	self displayOn: aDisplayMedium at: `0 @ 0`! !

!Form methodsFor: 'filling' stamp: 'jmv 3/12/2018 15:49:45'!
eraseShape: bwForm
	"use bwForm as a mask to clear all pixels where bwForm has 1's"
	((BitBlt destForm: self sourceForm: bwForm
		combinationRule: Form erase1bitShape	"Cut a hole in the picture with my mask"
		destOrigin: bwForm offset 
		sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits! !

!Form methodsFor: 'filling' stamp: 'jmv 3/12/2018 15:49:51'!
fill: aRectangle rule: anInteger fillColor: aForm 
	"Replace a rectangular area of the receiver with the pattern described by aForm 
	according to the rule anInteger."
	(BitBlt toForm: self)
		copy: aRectangle
		from: `0@0` in: nil
		fillColor: aForm rule: anInteger! !

!Form methodsFor: 'image manipulation' stamp: 'jmv 3/12/2018 15:50:44'!
smear: dir distance: dist
	"Smear any black pixels in this form in the direction dir in Log N steps"
	| skew bb |
	bb _ BitBlt destForm: self sourceForm: self
		combinationRule: Form under destOrigin: `0@0` sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox.
	skew _ 1.
	[skew < dist] whileTrue: [
		bb destOrigin: dir*skew; copyBits.
		skew _ skew+skew]! !

!Form methodsFor: 'transitions' stamp: 'jmv 3/12/2018 15:50:19'!
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect _ otherImage boundingBox.
	oldPage _ self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: Color black.
	oldBottom _ self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps _ 8.
	buffer _ Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d _ pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
					p _ pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: `0@0` rule: Form over.
					p _ pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect _ pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad _ Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp _ (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceToScreen.
		].

	buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceToScreen.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"! !

!Form methodsFor: 'private' stamp: 'jmv 3/12/2018 15:49:37'!
copyFromByteArray: bigEndianByteArray
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray2:to:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: `0 @ 0` rule: Form over! !

!Form methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:50:00'!
fromDisplay: aRectangle 
	"Create a virtual bit map from a user specified rectangular area on the 
	display screen. Reallocates bitmap only if aRectangle ~= the receiver's 
	extent."

	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].
	self
		copyBits: (aRectangle origin extent: self extent)
		from: Display
		at: `0 @ 0`
		clippingBox: self boundingBox
		rule: Form over! !

!Form methodsFor: 'encoding' stamp: 'jmv 3/12/2018 15:49:03'!
addDeltasFrom: previousForm

	(BitBlt 
		destForm: self 
		sourceForm: previousForm 
		fillColor: nil 
		combinationRule: Form reverse
		destOrigin: `0@0`
		sourceOrigin: `0@0`
		extent: self extent 
		clipRect: self boundingBox) copyBits.
	^self! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 15:51:12'!
dotOfSize: diameter
	"Create a form which contains a round black dot."
	| radius form bb rect centerX centerY centerYBias centerXBias radiusSquared xOverY maxy dx |
	radius _ diameter//2.
	form _ self extent: diameter@diameter offset: `0@0` - radius.
	bb _ (BitBlt toForm: form)
		sourceX: 0; sourceY: 0;
		combinationRule: Form over;
		fillColor: Color black.
	rect _ form boundingBox.
	centerX _ rect center x.
	centerY _ rect center y.
	centerYBias _ rect height odd ifTrue: [0] ifFalse: [1].
	centerXBias _ rect width odd ifTrue: [0] ifFalse: [1].
	radiusSquared _ (rect height asFloat / 2.0) squared - 0.01.
	xOverY _ rect width asFloat / rect height asFloat.
	maxy _ rect height - 1 // 2.

	"First do the inner fill, and collect x values"
	0 to: maxy do: [ :dy |
		dx _ ((radiusSquared - (dy * dy) asFloat) sqrt * xOverY) truncated.
		bb	destX: centerX - centerXBias - dx
			destY: centerY - centerYBias - dy
			width: dx + dx + centerXBias + 1
			height: 1;
			copyBits.
		bb	destY: centerY + dy;
			copyBits].
	^ form
"
Time millisecondsToRun:
	[1 to: 20 do: [:i | (Form dotOfSize: i) displayAt: (i*20)@(i*20)]]
"! !

!Form class methodsFor: 'examples' stamp: 'jmv 3/12/2018 15:51:17'!
toothpaste: diam	
	"
	Display restoreAfter: [Form toothpaste: 30]
	"
	"Not completely unlike the ST-80 implementation :)
	Original comment:
	Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| point queue color q colors colr colr2 |
	colors _ Color red wheel: 12.
	color _ 8.
	[ true ] whileTrue: [
		queue _ OrderedCollection new: 32.
		16 timesRepeat: [queue addLast: `-40@-40`].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		point _ Sensor mousePoint.
		colr _ colors atWrap: (color _ color + 5).  "choose increment relatively prime to colors size"
		colr2 _ colr alpha: 0.3.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue: [
			point _ point * 4 + Sensor mousePoint // 5.
			point _ point.
			Form fakeSphereAt: point diameter: diam color: colr.
			(q _ queue removeFirst) ifNil: [^ self].	"exit"
			Form fakeSphereAt: q diameter: diam color: colr2.
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]]! !


!ColorForm methodsFor: 'color manipulation' stamp: 'jmv 3/12/2018 15:47:55'!
asGrayForm
	"Build an optimal GrayForm,
	for any color palette in the receiver."
	| answer map |
	answer _ GrayForm extent: width@height.
	map _ self colormapIfNeededForGray8bpp.
	(BitBlt toForm: answer)
		colorMap: map;
		copy: self boundingBox
		from: `0@0` in: self
		fillColor: nil rule: Form over.
	^ answer! !

!ColorForm methodsFor: 'copying' stamp: 'jmv 3/12/2018 15:47:59'!
copy: aRect
 	"Return a new ColorForm containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	colors ifNotNil: [newForm colors: colors copy].
	^ newForm! !


!ColorForm class methodsFor: 'as yet unclassified' stamp: 'jmv 3/12/2018 15:48:04'!
mappingWhiteToTransparentFrom: aFormOrCursor
	"Return a ColorForm copied from the given Form or Cursor with white mapped to transparent."

	| f map |
	aFormOrCursor depth <= 8 ifFalse: [
		^ self error: 'argument depth must be 8-bits per pixel or less'].
	(aFormOrCursor is: #ColorForm) ifTrue: [
		f _ aFormOrCursor copy.
		map _ aFormOrCursor colors.
	] ifFalse: [
		f _ ColorForm extent: aFormOrCursor extent depth: aFormOrCursor depth.
		f copyBits: aFormOrCursor boundingBox
			from: aFormOrCursor
			at: `0@0`
			clippingBox: aFormOrCursor boundingBox
			rule: Form over.
		map _ Color indexedColors copyFrom: 1 to: (1 bitShift: aFormOrCursor depth)].
	map _ map collect: [:c |
		c = Color white ifTrue: [Color transparent] ifFalse: [c]].
	f colors: map.
	^ f! !


!Cursor methodsFor: 'converting' stamp: 'jmv 3/12/2018 15:48:08'!
enlargedBy: scale
	"Big cursors are 32 bits deep (ARGB premultiplied)"
	| big |
	scale = 1 ifTrue: [^self].
	big := CursorWithAlpha extent: self extent * scale depth: 32.
	(self asCursorForm magnifyBy: scale) displayOn: big.
	big offset: (self offset - 0.5 * scale min: `0@0` max: big extent negated) asIntegerPoint.
	big fallback: self.
	^ big! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 3/12/2018 15:48:15'!
actualScreenSize
	<primitive: 106>
	^ `640@480`! !

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 3/12/2018 15:48:18'!
shutDown 
	"Minimize Display memory saved in image"
	Display setExtent: `240@120` depth: Display nativeDepth.
	ScreenUpdaterProcess ifNotNil: [
		ScreenUpdaterProcess terminate.
		ScreenUpdaterProcess _ nil ]! !


!GrayForm methodsFor: 'copying' stamp: 'jmv 3/12/2018 15:51:21'!
copy: aRect
 	"Return a new instance containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	^ newForm! !


!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 15:47:23'!
bitPeekerFromForm: sourceForm
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord answer |
	pixPerWord _ sourceForm pixelsPerWord.
	answer _ self destForm: (Form extent: pixPerWord@1 depth: sourceForm depth)
	 	sourceForm: nil 			"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: (pixPerWord - 1)@0
		sourceOrigin: `0@0`
		extent: `1@1`
		clipRect: (`0@0` extent: pixPerWord@1).
	"To ensure no colormap set"
	answer sourceForm: sourceForm.
	^ answer! !

!BitBlt class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 15:47:27'!
bitPokerToForm: destForm
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord answer |
	pixPerWord _ 32//destForm depth.
	answer _ self destForm: destForm
	 	sourceForm: nil 		"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: (pixPerWord-1)@0
		extent: `1@1`
		clipRect: (`0@0` extent: destForm extent).
	"To ensure no colormap set"
	answer sourceForm: (Form extent: pixPerWord@1 depth: destForm depth).
	^ answer! !


!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 3/12/2018 15:46:56'!
internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x _ aPoint x.
	y _ aPoint y.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^`0@0` ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^ (detX * det) @ (detY * det)! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 3/12/2018 15:47:00'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	| x y det a11 a12 a21 a22 detX detY |

	x _ aPoint x - self a13.
	y _ aPoint y - self a23.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^`0@0` ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^ (detX * det) @ (detY * det)! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 3/12/2018 15:47:03'!
inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 _ self inverseTransform: `0@0`.
	r1 _ (self inverseTransform: `1@0`) - r3.
	r2 _ (self inverseTransform: `0@1`) - r3.
	m _ self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^ m! !

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 3/12/2018 15:46:50'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
		and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	| dstRect |
	dstRect _ Rectangle new.
	(self primDisplayBoundsOfTransformOf: aRectangle into: dstRect) ifNotNil: [
		dstRect topLeft > `0@0` ifTrue: [ ^dstRect  ]].
	^Rectangle encompassing: (aRectangle corners collect: [ :pt |
		(self transform: pt) rounded ])! !


!Point methodsFor: 'point functions' stamp: 'jmv 3/12/2018 15:55:24'!
eightNeighbors
	^ (Array with: self + `1@0`
		with: self + `1@1`
		with: self + `0@1`
		with: self + `-1@1`) ,
	(Array with: self + `-1@0`
		with: self + `-1@-1`
		with: self + `0@-1`
		with: self + `1@-1`)! !

!Point methodsFor: 'point functions' stamp: 'jmv 3/12/2018 15:55:28'!
fourNeighbors
	^ Array with: self + `1@0`
		with: self + `0@1`
		with: self + `-1@0`
		with: self + `0@-1`! !


!Rectangle methodsFor: 'accessing' stamp: 'jmv 3/12/2018 15:55:55'!
innerCorners
	"Return an array of inner corner points,
	ie, the most extreme pixels included,
	in the order of a quadrilateral spec for WarpBlt"
	| r1 |
	r1 _ self topLeft corner: self bottomRight - `1@1`.
	^ Array with: r1 topLeft with: r1 bottomLeft with: r1 bottomRight with: r1 topRight! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 3/12/2018 15:53:54'!
processUnknownEvent: aMorphicEvent localPosition: localEventPosition
	"An event of an unknown type was sent to the receiver. What shall we do?!!"

	Smalltalk beep. 
	aMorphicEvent printString displayAt: `0@0`.
	aMorphicEvent wasHandled: true! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:53:40'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.  Not honored in too many places yet, but respected by the resizeToFit feature, at least"

	self flag: #jmvVer2.	"in owner's coordinates?"
	^self valueOfProperty: #minimumExtent ifAbsent: [`1@1`]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:53:42'!
morphExtent
	"In our own coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^`50 @ 40`! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:53:45'!
morphPositionInWorld

	self flag: #jmvVer2.
	"Most likely we don't want to use global coordinates...
	In fact, we could be in many frames of reference at the same time...
	This method makes no sense at all!!"

	^self externalizeToWorld: `0@0`! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:53:48'!
morphTopLeft
	"By default, morphs occupy a rectangle specified by #morphTopLef and #morphExtent"
	^`0@0`! !

!Morph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:53:51'!
openInWorld: aWorld
	"Add this morph to the requested World."
	(location = MorphicTranslation new)
		ifTrue: [ aWorld addMorph: self position: `50@50` ]
		ifFalse: [ aWorld addMorph: self ]! !


!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:55:58'!
initialize
	super initialize.
	extent _ `50@40`.
	color _ self defaultColor! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 3/12/2018 15:54:34'!
invalidateDisplayRect: damageRect from: aMorph
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: ( `0@0` extent: extent) ) ]
                ifFalse: [ super invalidateDisplayRect: damageRect from: aMorph ]! !

!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:54:27'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: `0@0` ]
		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [ aCanvas drawsOnDisplay ] and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:54:38'!
morphPositionInWorld

	self flag: #jmvVer2. "Solo para evitar los warning por falta de owner... pensar despues este caso"
	self isWorldMorph ifTrue: [ ^ `0@0` ].
	^ super morphPositionInWorld! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 3/12/2018 15:54:43'!
viewBox

	^ worldState
		ifNotNil: [
			 `0@0` extent: extent ]
		ifNil: [
			self world viewBox ]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 3/12/2018 15:54:30'!
fillRects: rectangleList
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: `0@0`;
		clipRect: self viewBox;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt fillColor: Color random.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 50) wait! !


!PasteUpMorph class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 15:54:48'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: PasteUpMorph newWorld
] fork.
	"
	| w ws |
	w _ self new.
	ws _ WorldState new.
	w worldState: ws.
	w morphPosition: `0@0` extent: Display extent.
	ws setCanvas: Display getCanvas.
	w borderWidth: 0.
	ws handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 3/12/2018 15:48:22'!
morphContainsPoint: aLocalPoint

	| radius other delta xOverY |
	((`0@0` extent: extent) containsPoint: aLocalPoint) ifFalse: [^ false].  "quick elimination"
	extent > `1@1`
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ extent y asFloat / 2.
	other _ extent x asFloat / 2.
	delta _ aLocalPoint - (other@radius).
	xOverY _ extent x asFloat / extent y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!HandleMorph methodsFor: 'events' stamp: 'jmv 3/12/2018 15:51:54'!
keyStroke: aKeyboardEvent
	"Check for cursor keys"
	| keyValue |
	(owner is: #HandMorph) ifFalse: [ ^self ].
	keyValue _ aKeyboardEvent keyValue.
	keyValue = 28 ifTrue: [ ^self morphPosition: self morphPosition - `1@0` ].
	keyValue = 29 ifTrue: [ ^self morphPosition: self morphPosition + `1@0` ].
	keyValue = 30 ifTrue: [ ^self morphPosition: self morphPosition - `0@1` ].
	keyValue = 31 ifTrue: [ ^self morphPosition: self morphPosition + `0@1` ].
	"Special case for return"
	aKeyboardEvent isReturnKey ifTrue:[
		"Drop the receiver and be done"
	self flag: #arNote. "Probably unnecessary"
		owner releaseKeyboardFocus: self.
		self delete ]! !

!HandleMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:51:51'!
initialize
	"initialize the state of the receiver"
	super initialize.
	extent _ `12@12`! !


!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:55:13'!
initialize
	super initialize.
	extent _ `200@100`! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:54:51'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: (`0@0` extent: extent)
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:54:54'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ (`0@0` extent: extent) insetBy: `1@3`.
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ (`0@0` extent: extent) insetBy: `1@3`.
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:54:57'!
initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle _ nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model _ nil.
	getStateSelector _ nil.
	actionSelector _ nil.
	isPressed _ false.
	mouseIsOver _ false.
	actWhen _ #buttonUp.
	extent _  `20 @ 15`! !

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 3/12/2018 15:55:00'!
morphContainsPoint: aLocalPoint

	| iconOrigin |
	((`0@0` extent: extent) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ extent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: (aLocalPoint - iconOrigin) rounded) not ]]! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'jmv 3/12/2018 15:55:05'!
example
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 _ Switch new.
	s2 _ Switch new turnOn.
	s3 _ Switch new.
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 _ (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 _ (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 _ (PluggableButtonMorph model: s3 stateGetter: #isOn action: #turnOn) label: 'S3'.
	b1 color: Color lightRed.
	b2 color: Color lightRed.
	b3 color: Color lightRed.
	row _ LayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: `120@35`.
	^ row! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 3/12/2018 15:55:16'!
addToScroller: aMorph

	scroller
		addMorph: aMorph position: `0@0`;
		morphExtent: aMorph morphExtent! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:55:18'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ false.

	"initialize the receiver's scrollBars"
	scrollBar _ self scrollBarClass new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ self scrollBarClass new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ true.

	scroller _ self innerMorphClass new.
	self addMorph: scroller.
	self scrollerOffset: `0@ 0`.
	self addMorph: scrollBar.
	self addMorph: hScrollBar! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 3/12/2018 15:55:21'!
scrollToShow: aRectangle
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| delta |
	(aRectangle top >= 0 and: [
		aRectangle bottom <= self viewableHeight ])
		ifTrue: [
			"already visible"
			^self ].

	"Scroll end of selection into view if necessary"
	delta _ aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent).
	delta y ~= 0 ifTrue: [
		self scrollBy: 0@delta y ]! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 3/12/2018 15:55:10'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (localEventPosition y < 0 and: [ scrollBar scrollValue > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: `0@0`) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ scrollBar scrollValue < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [ self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self setSelectionIndex: row ]
		ifFalse: [ self setSelectionIndex: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !


!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 3/12/2018 15:56:57'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateLocalRect: (`0@0` extent: extent x @ (self labelHeight + borderWidth))! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:56:43'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: (`0@0` extent: extent) color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: titleColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:56:47'!
drawRoundedFrameOn: aCanvas color: widgetsColor
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	aCanvas
		windowFrame: (`0@0` extent: extent)
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: color! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:57:04'!
makeMeFullyVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-self morphExtent)]) ifTrue: [
		^ self "OK -- visible"].

	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self morphExtentInWorld world: self world) topLeft! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:57:07'!
makeMeVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:57:09'!
minimumExtent

	^`160@80`! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:57:13'!
rescaleButtons
	"boxExtent changed.  Update my buttons."
	| buttonExtent buttonPos buttonDelta |
	buttonExtent := self boxExtent.
	buttonPos := `2@2`.
	buttonDelta := self boxExtent x + 2.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos := (buttonPos x  + buttonDelta) @ 2.
		].
	]! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:56:39'!
boxExtent
	"answer the extent to use in all the buttons. 
	 
	the label height is used to be proportional to the fonts preferences"
	| e |
	Theme current minimalWindows ifTrue: [^`0@0`].
	e _ Preferences windowTitleFont height.
	^e@e! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:56:51'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	
	self initializeLabelArea.
	extent _ `300 @ 200`.

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorphFront: m ].

	"by default"
	self beColumn! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:56:54'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing |
	spacing _ self boxExtent x + 2.
	self addMorph: self createCloseBox position: `2@2`.
	self addMorph: self createCollapseBox position: spacing+2@2.
	self addMorph: self createExpandBox position: spacing*2+2@2.
	self addMorph: self createMenuBox position: spacing*3+2@2! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 3/12/2018 15:57:00'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@0 extent: w@thickness.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-thickness) extent: w@thickness.
	(adjusters at: #leftAdjuster) morphPosition: 0@cornerExtent extent: thickness@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-thickness@cornerExtent extent: thickness@h.
	(adjusters at: #topLeftAdjuster) morphPosition: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos _ borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			morphPosition: pos
			extent: extent - pos - borderWidth ].
	
	layoutNeeded _ false! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 3/12/2018 15:57:17'!
resize
	| resizeMorph |
	resizeMorph _ ResizeMorph new morphExtent: `200@150`.
	resizeMorph action: [self resize: (resizeMorph selectionRectangle: Display extent)].
	resizeMorph morphPosition: self world activeHand morphPosition.
	resizeMorph openInWorld! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:47:52'!
initialExtent

	^`540@400` * Preferences standardCodeFont height // 14! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:47:45'!
initialExtent
	^`540@300` * Preferences standardCodeFont height // 14! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:55:30'!
initialExtent
	^ `640 @ 320` * Preferences standardCodeFont height // 14! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:52:24'!
initialExtent

	^`600@325` * Preferences standardCodeFont height // 14! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:54:22'!
initialExtent

	^`300@500` * Preferences standardCodeFont height // 14! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:57:20'!
buildMorphicWindow

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'SUnit Test Runner'.
	self refreshWindow.
	self morphExtent: `460 @ 400`! !


!ScrollBar methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:56:22'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (`0@0` extent: extent)
		color: (color alphaMixed: 0.3 with: Theme current scrollbarColor)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 3/12/2018 15:53:14'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 3/12/2018 15:53:30'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = `0@0` ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 3/12/2018 15:53:34'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 3/12/2018 15:53:37'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:53:27'!
initialize
	super initialize.
	extent _ `40@10`.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 3/12/2018 15:53:21'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock position: `0@ -20`.
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 3/12/2018 15:53:17'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: each minItemWidth].

	w _ w + 1.
	p _ `5 @ 5`.
	submorphs do: [ :m |
		h _ m morphHeight.
		m morphPosition: p extent: w@h.
		p _ p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 3/12/2018 15:53:24'!
fitInWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| delta |
	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self morphPosition: ((self morphPosition x + 10 - extent x) @ self morphPosition y) ].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight:
			((owner world morphExtentInWorld y) max: (self morphPosition y) + 1)).
	delta = `0 @ 0` ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:47:08'!
downButtonPosition
	^ `0@0` + (extent - ScrollBar scrollbarThickness)! !

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:47:14'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas frameAndFillRectangle: (`0@0` extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			frameRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: self upButtonPosition.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: self downButtonPosition.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			color: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].
			aCanvas
				drawString: (completer entries at: index) asString
				at: rectangle topLeft
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!AutoCompleterMorph class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 15:47:19'!
initializedInstance
	| completer m |
	completer _ SmalltalkCompleter withModel: (TextModel withText: 'Small').
	completer
		instVarNamed: 'position'
		put: 5.
	completer computeEntries.
	m _ AutoCompleterMorph
		completer: completer
		position: `200 @ 200`.
	completer instVarNamed: 'menuMorph' put: m.
	^m! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:51:41'!
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	 aCanvas
		stencil: Cursor move
		at: `0@0`
		color: Color black! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:51:48'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds ifNil: [ ^self ].

	fullBounds _ fullBounds intersect: (`0@0` extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds! !

!HandMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:51:44'!
initForEvents
	mouseOverHandler _ nil.
	lastMouseEvent _ MouseEvent new setType: #mouseMove position: `0@0` buttons: 0 hand: self.
	lastMouseEventTime _ Time localMillisecondClock.
	lastEventBuffer _ {1. 0. 0. 0. 0. 0. nil. nil}.
	self dontWaitForMoreClicks! !


!ImageMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:51:57'!
drawOn: aCanvas

	aCanvas image: image at: `0@0`! !


!StringMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:56:33'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: `0@0`
		font: self fontToUse
		color: color! !

!StringMorph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:56:36'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: `0@0`! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:00'!
drawOn: aCanvas

	| tRect colorToUse sLeft aForm centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: (`0@0` extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) duller ].
	isSelected ifTrue: [
		aCanvas
			fillRectangle: (`0@0` extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].
			
	tRect _ self toggleRectangle.
	aForm _ isExpanded 
		ifTrue: [ container expandedForm ]
		ifFalse: [ container notExpandedForm ].
	centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.

	complexContents hasContents ifTrue: [
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	icon isNil
	ifFalse: [
		 aCanvas 
			image: icon
			at:  (tRect topLeft translatedBy:  icon width  @ centeringOffset).
		sLeft _ indentLevel * 12 + 16 + (icon width + 2).
	]
	ifTrue: [
		sLeft _ indentLevel * 12 + 16.
	].
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		at: sLeft@0
		font: self fontToUse
		color: colorToUse! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 3/12/2018 15:52:47'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: `0@2`! !

!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:51'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (`0@0` extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScaleAndTransparent ].
			aCanvas image: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]! !

!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:52:54'!
initialize
	"initialize the state of the receiver"
	super initialize.
	""
	extent _ `10@10`.
	contents _ ''.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preferences standardMenuFont! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 3/12/2018 15:52:57'!
measureContents
	| e |
	e _ super measureContents.
	^e y > 12
		ifTrue: [e+`2@2`]
		ifFalse: [e+`2@1`]! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 3/12/2018 15:52:59'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + `10@0`
											with: self morphBoundsInWorld topLeft)
			from: self.
		subMenu selectItem: nil ]! !


!MenuItemMorph class methodsFor: 'cached state access' stamp: 'jmv 3/12/2018 15:53:02'!
subMenuMarker

	| f |
	SubMenuMarker ifNotNil: [ ^SubMenuMarker ].
	f _ Form
		extent: `5@9`
		fromArray: #(2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
		offset: `0@0`.
	SubMenuMarker _ ColorForm mappingWhiteToTransparentFrom: f.
	^SubMenuMarker! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:28'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (`0@0` extent: extent)
		color: color! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 3/12/2018 15:52:31'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^`0@0` extent: extent! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 3/12/2018 15:52:35'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		fractionalWidths integerWidths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom alternativeWidths count diff i |
	
	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := usableWidth - sumOfFixed max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	fractionalWidths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerWidths _ fractionalWidths collect: [ :w | w rounded ].
	diff _ integerWidths sum - fractionalWidths sum rounded.
	alternativeWidths _ diff > 0 ifTrue: [ fractionalWidths collect: [ :w | w floor ]] ifFalse: [ fractionalWidths collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerWidths at: i) = (alternativeWidths at: i) ifFalse: [
			integerWidths at: i put: (alternativeWidths at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minWidth := integerWidths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) max: 0) + boundsForLayout left + xSep.
	usableHeight := boundsForLayout height - (ySep * 2) max: 0.
	boundsTop    := boundsForLayout top + ySep.
	boundsRight  := boundsForLayout right - xSep.
	boundsBottom := boundsForLayout bottom - ySep.

	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := integerWidths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := ((usableHeight - ht) * ls minorDirectionPadding) floor + boundsTop.
		theBottom := (theTop + ht) ceiling min: boundsBottom.
		theRight := (theLeft + (wd min: minWidth)) "ceiling "min: boundsRight.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theLeft := theRight + xSep
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 3/12/2018 15:52:39'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		fractionalHeights integerHeights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight alternativeHeights count diff i |

	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed max: 0.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerHeights _ fractionalHeights collect: [ :w | w rounded ].
	diff _ integerHeights sum - fractionalHeights sum rounded.
	alternativeHeights _ diff > 0 ifTrue: [ fractionalHeights collect: [ :w | w floor ]] ifFalse: [ fractionalHeights collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerHeights at: i) = (alternativeHeights at: i) ifFalse: [
			integerHeights at: i put: (alternativeHeights at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minHeight := integerHeights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) max: 0) + boundsForLayout top + ySep.
	usableWidth     := boundsForLayout width - (xSep * 2) max: 0.
	boundsLeft      := boundsForLayout left + xSep.
	boundsBottom := boundsForLayout bottom - ySep.
	boundsRight := boundsForLayout right - xSep.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := integerHeights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := ((usableWidth - wd) * ls minorDirectionPadding) floor + boundsLeft.
		theRight := (theLeft + wd) ceiling min: boundsRight.
		theBottom := (theTop + (ht min: minHeight)) "ceiling" min: boundsBottom.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theTop := theBottom + ySep
	]! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:55:38'!
initialize
	super initialize.
	self separation: 0.
	labelMorph _ StringMorph contents: '' font: AbstractFont default.
	subLabelMorph _ StringMorph contents: '' font: AbstractFont default.
	progress _ ProgressBarMorph new.
	progress morphExtent: `200 @ 15`.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: 15! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:51:24'!
drawOn: aCanvas

	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: color
		at: `0@0`! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 3/12/2018 15:51:29'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint colorToUse form icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: self class handleSize asPoint).
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 3/12/2018 15:51:35'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self class handleSize.
	anExtent _ ((extent x + self class handleSize + 8) max: minSide) @
				((extent y + self class handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target world ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: `8@8`) ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 3/12/2018 15:51:38'!
startGrow: evt with: growHandle
	"Initialize resizing of my target.  Launch a command representing it, to support Undo"

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: growHandle.  "remove all other handles"
	botRt _ target morphPositionInWorld + target morphExtentInWorld.
	positionOffset _ (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [`0@0`]! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:52:04'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > `0@0` and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPosition y > ptY ifTrue: [ ^nil ].
	last _ self lastSubmorph.
	last morphPosition y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPosition y <= ptY and: [ m morphPosition y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPosition y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:08'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: (`0@0` extent: extent).
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds color: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:11'!
drawHighlightOn: aCanvas
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: highlightedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (`0@0` extent: extent).
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus) duller! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:14'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (`0@0` extent: extent).
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:52:17'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		textComposition: self textComposition
		bounds: (`0@0` extent: extent)
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!ResizeMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:56:01'!
initialize
	super initialize.
	extent _ `400@300`.
	color _ Color white.
	grid _ `8@6`.
	gridLineWidth _ 2.
	gridColor _ Color black.
	selectionColor _ Color red! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:48:45'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	result morphExtent: `6@2` * self sizeUnit.
	self addMorph: result position: `2@7.5` * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:48:48'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	result morphExtent: `6@2` * self sizeUnit.
	self addMorph: result position: `12@7.5` * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:48:51'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	result morphExtent: `24@2` * self sizeUnit.
	self addMorph: result position: `2@0.5` * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:48:54'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	result morphExtent: `18@5` * self sizeUnit.
	self addMorph: result position: `1@2` * self sizeUnit.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:49:00'!
initialize

	super initialize.
	extent _ `20@10` * self sizeUnit.
	responseUponCancel _ ''! !

!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:48:57'!
drawOn: aCanvas

	Theme current roundWindowCorners
		ifTrue: [
			aCanvas
				roundRect: (`0@0` extent: extent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:57:36'!
drawOn: aCanvas
	Transcript
		showOnDisplay: true;
		displayOn: form in: (`0@0` extent: extent).
	aCanvas image: form at: `0@0`.
	Transcript
		bounds: self morphBoundsInWorld;
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 3/12/2018 15:53:05'!
drawOn: aCanvas 
	| baseColor |
	baseColor _ owner color.
	aCanvas
		fillRectangle: (`0@0` corner: extent x @ (extent y / 2))
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (0 @ (extent y / 2) corner: extent)
		color: baseColor twiceLighter! !

!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:53:08'!
initialize
	super initialize.
	extent _ `50 @ 2`! !

!MenuLineMorph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:53:10'!
minimumExtent

	^`10@2`! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/12/2018 15:57:46'!
tryDeferredUpdatingAndSetCanvas
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"As this is the only sender of #deferUpdates: , this could be done in Morphic or image startup, and never efterwards..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		world morphPosition: `0@0` extent: Display extent.
		self setCanvas: Display getCanvas.
	].
	^ true! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 3/12/2018 15:54:16'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"
	"An event of an unknown type was sent. What shall we do?!!"

	Smalltalk beep. 
	self printString displayAt: `0@0`.
	self wasHandled: true! !


!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 3/12/2018 15:54:04'!
newClipRect: aRectangleOrNil
	"aRectangle is in world coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ `0@0` corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ])! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/12/2018 15:53:57'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBounds.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	self line: r topLeft to: r bottomRight-w width: w color: Color yellow.
	self line: r topRight -(w@0) to: r bottomLeft -(0@w)width: w color: Color yellow! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/12/2018 15:54:00'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	aColor = Color black ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: Color black.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor ].
	aColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: Color white.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + `0@2`
			font: fontOrNil
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + `0@1`
		font: fontOrNil
		color: aColor! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/12/2018 15:54:10'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (`0@0` max: (clipLeft@clipTop)- boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: (clipRight@clipBottom+1) - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun ]! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 15:54:13'!
onForm: aForm

	^ self basicNew
		initializeWith: aForm origin: `0@0`! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 3/12/2018 15:47:31'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule
	"aRectangle is in form coordinates, no transformation is done."
	| displayRectangle  additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset _ `0@0`.
	clippedPort _ port clippedBy: displayRectangle.
	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedPort clipRect.
	savedMap _ clippedPort colorMap.
	clippedPort sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedPort destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedPort destOrigin: x@y; copyBits]].
	clippedPort colorMap: savedMap! !


!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 3/12/2018 15:47:38'!
steButtonForm: extent
	^self cachedForms
		at: { #steButton . extent }
		ifAbsentPut: [
			| form canvas |
			form _ Form extent: extent depth: 32.
			canvas _ form getCanvas.
			canvas
				roundRect: (`0@0` extent: extent)
				color: (Color gray: 0.4)
				radius: 4.
			canvas
				roundRect: (`1@1` extent: extent-2)
				color: Color white
				radius: 4
				gradientTop: 1.0
				gradientCenter: 0.73
				gradientBottom: 0.94
				gradient1Height: (extent y-8+1 max: extent y//2).
			form]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 3/12/2018 15:47:35'!
buildArrowWith: insideForm borderForm: borderForm
	| extent translucentForm color insideSpec borderSpec border background alpha|
	"
	Display getCanvas
		image: (BitBltCanvas buildArrowWith: BitBltCanvas downInsideForm borderForm: BitBltCanvas downBorderForm)
		at: 20@20
	Display getCanvas
		image: (BitBltCanvas buildArrowWith:BitBltCanvas upInsideForm borderForm: BitBltCanvas upBorderForm)
		at: 40@20
	"
	extent _ insideForm extent - `1@2`.
	translucentForm _ Form extent: insideForm extent depth: 32.
	0 to: extent x-1 do: [ :x |
		0 to: extent y-1 do: [ :y |
			insideSpec _ insideForm colorAt: x@(y+1).
			borderSpec _ borderForm colorAt: x@(y+1).
			insideSpec = (Color r: 0.0 g: 0.0 b: 1.0)
				ifTrue: [ color _ Color transparent ]
				ifFalse: [
					borderSpec = (Color r: 1.0 g: 0.0 b: 0.0)
						ifTrue: [ color _ insideSpec ]
						ifFalse: [
							border _ x < (extent x//2)
								ifTrue: [Color white]
								ifFalse: [Color black].
							background _ borderForm colorAt: extent x@(y+1).
							alpha _ borderSpec red asFloat - background red / (border red - background red).
							color _ border alpha: (alpha min: 1 max: 0) ]].
			translucentForm colorAt: x@y put: color ]].
	^translucentForm! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 3/12/2018 15:57:22'!
defaultCharacterBlock
	^ CharacterBlock
		stringIndex: 1
		text: model actualContents
		topLeft: lines first topLeft
		extent: `0 @ 0`
		textLine: lines first! !


!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 3/12/2018 15:56:04'!
isBinarySelectorCharacter: aCharacter

	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].

	('"#$'':().;[]{}_`'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter numericValue = Scanner doItCharacterValue ifTrue: [^false "the doIt char"].
	aCharacter numericValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^ true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 3/12/2018 15:56:08'!
parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self scanPast: #backtick.
	self leaveBlock! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 3/12/2018 15:56:12'!
parseTerm
	self failWhen: currentToken isNil.
	currentTokenFirst == $( 
		ifTrue: [
			bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			bracketDepth := bracketDepth - 1.
			^self ].
	currentTokenFirst == $[ ifTrue: [^self parseBlock].
	currentTokenFirst == $` ifTrue: [^self parseBacktick].
	currentTokenFirst == ${ 
		ifTrue: [
			self scanPast: #leftBrace.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 3/12/2018 15:48:12'!
maxLengthPoints
	| max points |
	max := self maxLength.
	max = 0 ifTrue: [^Array with: `0 @ 0`].
	points := OrderedCollection new.
	tally withIndexesDo: [:i :j :t | t = max ifTrue: [points add: i @ j]].
	^ points! !

