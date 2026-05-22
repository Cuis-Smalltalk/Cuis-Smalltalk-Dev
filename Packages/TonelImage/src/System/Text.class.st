"
I represent a character string that has been marked with abstract changes in character appearance. Text associates a set of TextAttributes with each character in its character string.  These attributes may be fonts, emphases such as bold or italic, or hyperling actions. Since most characters have the same attributes as their neighbors, the attributes are stored in a RunArray for efficiency.  Each of my instances has
	string		a String
	runs		a RunArray
			
From the comment at #deepCopy, written by Dan on 11/9/97:
"Both string and runs are assumed to be read-only"
"
Class {
	#name : 'Text',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'string',
		'runs'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'private' }
Text class >> addAttribute: newTextAttribute toArray: existingAttributes [
	"Add a new text attribute to an existing set"

	| includeNew |
	includeNew := true.
	^ Array streamContents: [ :strm |
		existingAttributes do: [ :existing |
			(newTextAttribute dominates: existing) ifFalse: [
				"Keep existing unless new dominates it."
				strm nextPut: existing.
				"If new doesn't dominate existing, perhaps existing dominates new."
				(existing dominates: newTextAttribute) ifTrue: [
					"Don't add new if an existing attribute dominates it."
					includeNew := false ]
				]].
		includeNew ifTrue: [ strm nextPut: newTextAttribute ]].
]

{ #category : 'instance creation' }
Text class >> fromString: aString  [
	"Answer an instance of me whose characters are those of the argument, aString."

	^ self string: aString attributes: #()
]

{ #category : 'instance creation' }
Text class >> initialFont: aFont string: aString attribute: aTextAttribute [
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	^self
		string: aString
		attributes: {
			TextFontFamily familyName: aFont familyName.
			TextFontSize pointSize: aFont pointSize.
			aTextAttribute}
]

{ #category : 'instance creation' }
Text class >> initialFont: aFont stringOrText: aStringOrText [
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	(aStringOrText is: #Text) ifTrue: [
		^aStringOrText initialFont: aFont ].
	
	^self string: aStringOrText attributes: {
			TextFontFamily familyName: aFont familyName.
			TextFontSize pointSize: aFont pointSize }
]

{ #category : 'instance creation' }
Text class >> new [

	^self fromString: ''
]

{ #category : 'instance creation' }
Text class >> new: stringSize  [
	^self fromString: (String new: stringSize)
]

{ #category : 'private' }
Text class >> setParagraphAttributes: paragraphAttributes toArray: others [
	"Make paragraphAttributes the only paragraph attributes,
	but keep characterAttributes to an existing set"
	
	^ Array streamContents: [:strm |
		paragraphAttributes do: [ :att |
			strm nextPut: att ].
		others do: [ :other | 
			other isParagraphAttribute ifFalse: [ strm nextPut: other ]]]
]

{ #category : 'instance creation' }
Text class >> string: aString  [
	"Answer an instance of me whose characters are those of the argument, aString."

	^ self string: aString attributes: #()
]

{ #category : 'instance creation' }
Text class >> string: aString attribute: att [
	"Answer an instance of me whose characters are aString.
	att is a TextAttribute."

	^self string: aString attributes: (Array with: att)
]

{ #category : 'instance creation' }
Text class >> string: aString attributes: atts [
	"Answer an instance of me whose characters are those of aString.
	atts is an array of TextAttributes."

	^self string: aString runs: (RunArray new: aString size withAll: atts)
]

{ #category : 'private' }
Text class >> string: aString runs: anArray   [
	^self basicNew setString: aString setRuns: anArray
]

{ #category : 'instance creation' }
Text class >> withForm: aForm [
	"
	('Hello', (Text withForm: ImageMorph defaultForm), 'world') edit
	"
	^' ', (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: aForm)), ' '
]

{ #category : 'copying' }
Text >> , aStringOrText [

	^ aStringOrText appendToText: self
]

{ #category : 'comparing' }
Text >> = other [
	"Am I equal to the other Text or String?  
	***** Warning ***** Two Texts are considered equal if they have the same characters in them.  They might have completely different emphasis, fonts, sizes, text actions, or embedded morphs.  If you need to find out if one is a true copy of the other, you must do (text1 = text2 and: [text1 runs = text2 runs])."

	self == other ifTrue: [ ^ true ].
	(other is: #Text) ifTrue: [ "This is designed to run fast even for megabytes"
		^ string == other string or: [string = other string]].
	other isString ifTrue: [^ string == other or: [string = other]].
	^ false
]

{ #category : 'adding' }
Text >> add: newObject  [
	"Adding to an Interval is not allowed."

	self shouldNotImplement
]

{ #category : 'emphasis' }
Text >> addAttribute: att  [
	self isEmpty ifTrue: [ ^self ].
	^ self addAttribute: att from: 1 to: self size
]

{ #category : 'emphasis' }
Text >> addAttribute: aTextAttribute from: requestedStart to: requestedStop [
	"Set the attribute for characters in the interval start to stop.
	Turned into a command to enable reuse by undo / redo"
	(self
		commandForAddAttribute: aTextAttribute
		from: requestedStart
		to: requestedStop) doOn: self
]

{ #category : 'emphasis' }
Text >> alignmentAt: characterIndex [

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ alignment ]
]

{ #category : 'adding emphasis' }
Text >> allBold [
	"prefer shorter selector"
	self bold
]

{ #category : 'accessing' }
Text >> append: stringOrText [

	^ stringOrText appendToText: self
]

{ #category : 'copying' }
Text >> appendToString: aString [

	| stringSize |
	stringSize := aString size.
	^ (aString asText) 
			replaceFrom: stringSize + 1
			to: stringSize 
			with: self
]

{ #category : 'copying' }
Text >> appendToText: aText [

	| start textResult |
	start := aText size + 1.
	textResult := Text fromString: aText string , self string.
	textResult privateSetRuns: (aText runs copyReplaceFrom: start to: textResult size with: self runs).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	textResult fixParagraphAttributesFrom: start to: textResult size.
	^ textResult
	
]

{ #category : 'converting' }
Text >> asNumber [
	"Answer the number created by interpreting the receiver as the textual
	representation of a number."

	^string asNumber
]

{ #category : 'converting' }
Text >> asPlainString [
	"Answer a plain String, not a Text."
	^string
]

{ #category : 'converting' }
Text >> asStreamResult [
	"See senders.
	Also see #thatCanHoldCodePoints"
	self setString: string asStreamResult setRuns: runs.
	^self
]

{ #category : 'converting' }
Text >> asString [
	"Answer a String representation of the textual receiver."
	^string
]

{ #category : 'converting' }
Text >> asText	 [
	"Answer the receiver itself."
	^self
]

{ #category : 'converting' }
Text >> asUnicodeStringOrText [
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	string isUnicodeString ifTrue: [ ^self ].
	^Text string: string asUnicodeString runs: runs
]

{ #category : 'accessing' }
Text >> asUtf8Bytes [
	"Answer UTF-8 bytes for the string representation of the receiver."
	^string asUtf8Bytes
]

{ #category : 'accessing' }
Text >> at: index  [
	^string at: index
]

{ #category : 'accessing' }
Text >> at: index put: character [

	| answer prevChar |
	prevChar := string at: index.
	answer := string at: index put: character.

	"Only fix ParagraphAttributes if there is real danger of breaking the invariant"
	(prevChar isLineSeparator and: [
			(self attributesAt: index) anySatisfy: [ :attr | attr isParagraphAttribute]]) ifTrue: [
		self fixParagraphAttributesFrom: index to: index ].
	
	^answer
]

{ #category : 'emphasis' }
Text >> attributesAt: characterIndex [
	"Answer the code for characters in the run beginning at characterIndex."

	self isEmpty
		ifTrue: [^ #()].  "null text tolerates access"
	^runs at: characterIndex
]

{ #category : 'emphasis' }
Text >> attributesFrom: start to: stop do: aBlock [
	"evaluate aBlock for each attribute in the specified range.
	Warning: aBlock might be evaluated several times for each attribute, but not as many as the characters that it applies to!."
	runs runsFrom: start to: stop do: [ :attributes |
		attributes do: [ :attribute |
			aBlock value: attribute ]]
]

{ #category : 'private' }
Text >> basicReplaceAttributesFrom: start to: stop with: replacement [
	"Private. Does not enforce invariants.
	replacement size = (stop-start-1) "
	
	self privateSetRuns: (runs basicReplaceAttributesFrom: start to: stop with: replacement)
]

{ #category : 'adding color' }
Text >> black [
	"Stuff like
	'Hello world' black edit
	"
	self color: Color black
]

{ #category : 'adding color' }
Text >> blue [
	"Stuff like
	'Hello world' blue edit
	"
	self color: Color blue
]

{ #category : 'adding emphasis' }
Text >> bold [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis bold
]

{ #category : 'testing' }
Text >> canHoldAnyCodePoint [
	"Ask our string"
	^string canHoldAnyCodePoint
]

{ #category : 'attributes' }
Text >> canJoin: attributes1 and: attributes2 [
	| s |
	s := attributes1 size.
	^s = attributes2 size and: [
		(1 to: s) allSatisfy: [ :i |
			(attributes1 at: i) canBeJoinedWith: (attributes2 at: i) ]]
]

{ #category : 'adding attributes' }
Text >> centered [
	"Stuff like
	('Hello world' centered ) edit
	"
	self addAttribute: TextAlignment centered
]

{ #category : 'emphasis' }
Text >> characterStyleOrNilAt: characterIndex [

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ characterStyle ]
]

{ #category : 'emphasis' }
Text >> characterStyleOrNilIfApplying: textAttributes [
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
		^characterStyle ]
]

{ #category : 'adding color' }
Text >> color: aColor [
	"Stuff like
	'Hello world' color: (Color fromHexString: '#941100') :: edit
	'Hello world' color: (Color fromHexString: '#5A9F2D') :: edit
	"
	self addAttribute: (TextColor color: aColor)
]

{ #category : 'emphasis' }
Text >> colorAt: characterIndex [
	"Answer the color for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ color ]
]

{ #category : 'emphasis' }
Text >> commandForAddAttribute: aTextAttribute from: requestedStart to: requestedStop [

	"Set the attribute for characters in the interval start to stop."
	| intervalToFix start stop new old |
	start := requestedStart.
	stop := requestedStop.
	
	"If aTextAttribute must be applied to whole paragraphs, do so."
	aTextAttribute isParagraphAttribute ifTrue: [
		intervalToFix := self encompassParagraph: (start to: stop).
		start := intervalToFix first.
		stop := intervalToFix last ].

	old := runs copyFrom: start to: stop.
	new := old copy mapValues: [ :attributes | 
			Text addAttribute: aTextAttribute toArray: attributes ].
	^AttributesReplaceCommand
		old: old
		new: new
		start: start
		stop: stop
]

{ #category : 'emphasis' }
Text >> commandForRemoveAttributes: attributesToRemove from: requestedStart to: requestedStop [
	"Remove the attribute over the interval start to stop."

	| intervalToFix start stop new old |
	start := requestedStart.
	stop := requestedStop.
	
	"If att must be applied to whole paragraphs, do so."
	(attributesToRemove anySatisfy: [ :att | att isParagraphAttribute ]) ifTrue: [
		intervalToFix := self encompassParagraph: (start to: stop).
		start := intervalToFix first.
		stop := intervalToFix last ].

	old := runs copyFrom: start to: stop.
	new := old copy mapValues: [ :attributes | 
			attributes copyWithoutAll: attributesToRemove].
	^AttributesReplaceCommand
		old: old
		new: new
		start: start
		stop: stop
]

{ #category : 'accessing' }
Text >> commandForReplaceFrom: start to: stop with: replacement [

	^TextReplaceCommand
		old: (self copyFrom: start to: stop)
		new: replacement
		at: start
]

{ #category : 'copying' }
Text >> copyBlank: requestedSize [
	"Answer an instance like me, of size n, but without content.
	The underlying string of the new instance matches the type of the receiver's string (for example String or UnicodeString)."
	^ Text fromString: (string class new: requestedSize)
]

{ #category : 'copying' }
Text >> copyFrom: start to: stop  [
	"Answer with a copied subrange of this text"

	| realStart realStop |
	stop > self size
		ifTrue: [realStop := self size]		"handle selection at end of string"
		ifFalse: [realStop := stop].
	start < 1
		ifTrue: [realStart := 1]			"handle selection before start of string"
		ifFalse: [realStart := start].
	^Text 
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)
]

{ #category : 'copying' }
Text >> copyReplaceFrom: start to: stop with: replacement [
	| rep newRuns newString answer |
	rep := replacement asText.	"might be a string"
	newString := string copyReplaceFrom: start to: stop with: rep string.
	newRuns := runs copyReplaceFrom: start to: stop with: rep runs.
	answer := self class string: newString runs: newRuns.
	answer fixParagraphAttributesFrom: start to: start + replacement size - 1.
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	^ answer
]

{ #category : 'inspecting' }
Text >> customizeExplorerContents [

	^ false
]

{ #category : 'adding color' }
Text >> cyan [
	"Stuff like
	'Hello world' cyan edit
	"
	self color: Color cyan
]

{ #category : 'converting' }
Text >> displayStringOrText [
	"To be used in the UI"
	"Answer the receiver itself."

	^self
]

{ #category : 'user interface' }
Text >> edit [

	self editLabel: 'Text Editor'
]

{ #category : 'user interface' }
Text >> editLabel: labelString [

	(TextModel withText: self) openLabel: labelString
]

{ #category : 'accessing' }
Text >> embeddedMorphs [
	"return the list of morphs embedded in me"
	^self embeddedMorphsFrom: 1 to: self size
]

{ #category : 'accessing' }
Text >> embeddedMorphsFrom: start to: stop  [
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	self attributesFrom: start to: stop do: [:attr |
		attr anchoredFormOrMorph ifNotNil: [ :m |
			(m is: #Morph) ifTrue: [
				morphs add: m]]].
	^morphs
]

{ #category : 'emphasis' }
Text >> emphasisAt: characterIndex [
	"Answer the emphasis for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ emphasis ]
]

{ #category : 'paragraph support' }
Text >> encompassLine: anInterval [
	^string encompassLine: anInterval
]

{ #category : 'paragraph support' }
Text >> encompassParagraph: anInterval [
	^string encompassParagraph: anInterval
]

{ #category : 'emphasis' }
Text >> find: attribute [
	"Return the first interval over which this attribute applies"
	^ runs find: attribute
]

{ #category : 'accessing' }
Text >> findString: aString startingAt: start  [
	"Answer the index of subString within the receiver, starting at position start.
	If the receiver does not contain subString, answer 0."

	^string findString: aString asPlainString startingAt: start
]

{ #category : 'accessing' }
Text >> findString: aString startingAt: start caseSensitive: caseSensitive [
	"Answer the index of subString within the receiver, starting at index 
	start. If the receiver does not contain subString, answer 0."

	^string findString: aString asPlainString startingAt: start caseSensitive: caseSensitive
]

{ #category : 'accessing' }
Text >> findStringCaseInsensitive: aString startingAt: start [
	"Answer the index of subString within the receiver, starting at position start.
	If the receiver does not contain subString, answer 0."

	^string findStringCaseInsensitive: aString asPlainString startingAt: start
]

{ #category : 'private' }
Text >> fixParagraphAttributesFrom: start to: end [
	"Helper method to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	See senders"
	
	| paragraphEnd paragraphInterval paragraphStart paragraphAttributes|
	self hasAnyParagraphAttribute ifFalse: [ ^self ].
	paragraphEnd := end max: start.	"end could be start-1 when new text is empty, for example, when backspacing"
	[
		paragraphInterval := self encompassParagraph: (paragraphEnd to: paragraphEnd).
		paragraphStart := paragraphInterval first.
		paragraphEnd := paragraphInterval last.
	
		"We must honor the paragraph attributes as defined in the newline (Lf) Character that ends the paragraph"
		paragraphAttributes := (self attributesAt: paragraphEnd) select: [ :attr | attr isParagraphAttribute ].

		"if paragraphEnd is inside the interval just modified, and it doesn't bring any paragraph attributes (i.e., it doesn't end in CR),
		then try to keep the paragraph attributes previously in use in this paragraph..
		This is needed, for example, when pasting an image  or a plain text at the end of the document"
		(paragraphEnd = end and: [ paragraphStart < start and: [ end > 0 and: [ (string at: end) isLineSeparator not ]]]) ifTrue: [
			paragraphAttributes := (self attributesAt: paragraphStart) select: [ :attr | attr isParagraphAttribute ]].

		self privateSetParagraphAttributes: paragraphAttributes from: paragraphStart to: paragraphEnd.
		paragraphEnd := paragraphStart - 1.
		paragraphStart > start ] whileTrue.
	runs coalesce
]

{ #category : 'emphasis' }
Text >> font: aFont [
	"Apply aFont to the entire contents.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	self addAttribute: (TextFontFamily
			familyName: aFont familyName).
	self addAttribute: (TextFontSize
			pointSize: aFont pointSize).
]

{ #category : 'emphasis' }
Text >> fontAt: characterIndex default: defaultFont [
	"Answer the font for characters in the run beginning at characterIndex."

	^self fontIfApplying: (self attributesAt: characterIndex) default: defaultFont
]

{ #category : 'emphasis' }
Text >> fontIfApplying: textAttributes default: defaultFont [
	"Answer the font for characters as specified by the argument."

	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
		| font |
		font := defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].
		familyName notNil | pointSize notNil ifTrue: [
			| fn ps |
			fn := familyName ifNil: [ font familyName ].
			ps := pointSize ifNil: [ font pointSize ].
			(FontFamily familyName: fn pointSize: ps) ifNotNil: [ :f |
				font := f emphasized: font emphasis]].
		^font emphasized: (font emphasis bitOr: emphasis) ].
]

{ #category : 'adding color' }
Text >> gray [
	"Stuff like
	'Hello world' gray edit
	"
	self color: Color gray
]

{ #category : 'adding color' }
Text >> green [
	"Stuff like
	'Hello world' green edit
	"
	self color: Color green
]

{ #category : 'attributes' }
Text >> hasAnyAttribute [
	"Return false if there are no emphasis (i.e., a String would not make a difference)"

	^runs values anySatisfy: [ :emphArray | emphArray notEmpty ]
]

{ #category : 'attributes' }
Text >> hasAnyCenteredOrRightFlush [
	"Return false if there are no Text Alignment attributes for center or right flush"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isCenteredOrRightFlush ]]
]

{ #category : 'attributes' }
Text >> hasAnyParagraphAttribute [
	"Return false if there are no paragraph attributes"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isParagraphAttribute ]]
]

{ #category : 'attributes' }
Text >> hasAnyTextAlignment [
	"Return false if there are no Text Alignment attributes"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isTextAlignment ]]
]

{ #category : 'emphasis testing' }
Text >> hasShoutBackgroundColorAt: index [
	^(self attributesAt: index)
		anySatisfy: [ :each | each isBackgroundColor & each isForShout].
]

{ #category : 'comparing' }
Text >> hash [
	"#hash is implemented, because #= is implemented.  We are now equal to a string with the same characters.  Hash must reflect that."

	^ string hash
]

{ #category : 'emphasis' }
Text >> initialFont: aFont [
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttributes |
	fontAttributes := {TextFontFamily			familyName: aFont familyName.
			TextFontSize pointSize: aFont pointSize}.
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes, fontAttributes ]]
]

{ #category : 'emphasis' }
Text >> initialFontFrom: someAttributes [
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttr |
	fontAttr := someAttributes detect: [ :any | any isFont ] ifNone: [ ^ self ].
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes copyWith: fontAttr ]]
]

{ #category : 'testing' }
Text >> is: aSymbol [
	^ aSymbol == #Text or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Text >> isEmpty [
	^self size = 0
]

{ #category : 'accessing' }
Text >> isLastLineSeparator [
	^string isLastLineSeparator
]

{ #category : 'accessing' }
Text >> isLastSeparator [
	^string isLastSeparator
]

{ #category : 'testing' }
Text >> isLineEndAt: anIndex [
	"Answer true if a line ends at position anIndex.
	Tolerates one position behind last character: 	"
	^string isLineEndAt: anIndex
]

{ #category : 'testing' }
Text >> isRemote [
	^false
]

{ #category : 'testing' }
Text >> isSeparators [
	"Answer true if the receiver contains only separators (i.e. whitespace)."
	^string isSeparators
]

{ #category : 'adding emphasis' }
Text >> italic [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis italic
]

{ #category : 'adding attributes' }
Text >> justified [
	"Stuff like
	('Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. ' justified ) edit
	"
	self addAttribute: TextAlignment justified
]

{ #category : 'adding attributes' }
Text >> leftFlush [
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	self addAttribute: TextAlignment leftFlush
]

{ #category : 'accessing' }
Text >> lineCount [

	^ string lineCount
]

{ #category : 'accessing' }
Text >> linesDo: aBlock [
	"execute aBlock with each line in this Text.  The line endings are not included in what is passed to aBlock"
	
	string lineIndicesDo: [ :start :endWithoutDelimiters :end |
		aBlock value: (self copyFrom: start  to: endWithoutDelimiters) ]
]

{ #category : 'adding color' }
Text >> magenta [
	"Stuff like
	'Hello world' magenta edit
	"
	self color: Color magenta
]

{ #category : 'converting' }
Text >> optimizedForMutationSpeed [
	"Do not use RunArray. Optimized for extensive attribute modification."
	
	(runs is: #RunArray) ifTrue: [
		^Text string: string runs: (RunNotArray withAll: runs) ]
]

{ #category : 'paragraph support' }
Text >> paragraphStyleChunksDo: aBlock [
	"Evaluate aBlock over each chunk (sequence of paragraphs) that have the same paragraphStyle"
	| start nextStart style |
	start := 1.
	nextStart := 1.
	[ start <= self size ] whileTrue: [
		style := self paragraphStyleOrNilAt: start.
		[ nextStart <= self size and: [ (self paragraphStyleOrNilAt: nextStart) = style ]] whileTrue: [
			nextStart := nextStart + 1 ].
		aBlock value: (start to: nextStart-1) value: style.
		start := nextStart ]
]

{ #category : 'emphasis' }
Text >> paragraphStyleOrNilAt: characterIndex [

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^ paragraphStyle ]
]

{ #category : 'emphasis' }
Text >> paragraphStyleOrNilIfApplying: textAttributes [
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self
		withAttributeValues: textAttributes
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor :lineSpacingFactor |
			^paragraphStyle ]
]

{ #category : 'adding emphasis' }
Text >> pointSize: pointSize [
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	self addAttribute: (TextFontSize pointSize: pointSize)
]

{ #category : 'copying' }
Text >> postCopy [

	string := string copy.
	runs := runs copy.
	runs mapValues: [ :attributes | attributes collect: [ :attr | attr copy ]]
]

{ #category : 'accessing' }
Text >> prepend: stringOrText [

	self replaceFrom: 1 to: 0 with: stringOrText
]

{ #category : 'printing' }
Text >> printOn: aStream [
	aStream isText
		ifTrue: [aStream nextPutAll: self. ^ self].
	self printNameOn: aStream.
	aStream nextPutAll: ' for '; print: string
]

{ #category : 'private' }
Text >> privateSetParagraphAttributes: paragraphAttributes from: start to: stop [

	self privateSetRuns: (runs
		copyReplaceFrom: start
		to: stop
		with: ((runs copyFrom: start to: stop) mapValues: [ :attributes |
			Text setParagraphAttributes: paragraphAttributes toArray: attributes]))
]

{ #category : 'private' }
Text >> privateSetRuns: anArray [
	"Warning. No attempt is done to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	Use with care. Currently only used for Shout, that seems to know what it does.
	Also used for private use, replacing asignment to the ivar, to ensure that the RunArray is set to properly compare TextAttributes"

	runs := anArray.
	runs canJoinMessage: (MessageSend receiver: self selector: #canJoin:and:)
]

{ #category : 'accessing' }
Text >> rangeOf: attribute startingAt: index [
	"Answer an interval that gives the range of attribute at index position  index. An empty interval with start value index is returned when the attribute is not present at position index."
	^ self isEmpty
		ifTrue: [ index to: index - 1 ]
		ifFalse: [
			runs
				rangeOf: attribute
				startingAt: index ].
]

{ #category : 'adding color' }
Text >> red [
	"Stuff like
	'Hello world' red edit
	"
	self color: Color red
]

{ #category : 'emphasis' }
Text >> removeAttributes: attributesToRemove from: requestedStart to: requestedStop [
	"Remove the attribute over the interval start to stop.
	Turned into a command to enable reuse by undo / redo"
	
	(self
		commandForRemoveAttributes: attributesToRemove
		from: requestedStart
		to: requestedStop) doOn: self
]

{ #category : 'emphasis' }
Text >> removeAttributesThat: aBlock [
	runs mapValues: [ :attributes |
		attributes reject: aBlock ]
]

{ #category : 'accessing' }
Text >> replaceFrom: start to: stop with: replacement [
	"newSize = oldSize - (stop-start-1) + aText size"
	
	| rep |
	rep := replacement asText.	"might be a string"
	string := string copyReplaceFrom: start to: stop with: rep string.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: rep runs).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacement size - 1
]

{ #category : 'converting' }
Text >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| rep newRepRuns |
	rep := replacement asText.	"might be a string"
	string replaceFrom: start to: stop with: rep string startingAt: repStart.
	newRepRuns := rep runs copyFrom: repStart to: repStart + stop - start.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacement size - 1
]

{ #category : 'converting' }
Text >> replaceFrom: start to: stop withString: replacementString attributes: attributesArray startingAt: repStart  [
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| newRepRuns |
	string := string replaceFrom: start to: stop with: replacementString startingAt: repStart.
	newRepRuns := RunArray new: stop-start+1 withAll: attributesArray.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacementString size - 1
]

{ #category : 'converting' }
Text >> reversed [
	"Answer a copy of the receiver with element order reversed."
	
	| answer |
	answer := self class string: string reversed runs: runs reversed.
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected.
	The way it is done could be considered to be wrong. In this case, instead of making the text to take
	the ParagraphAttributes from the cr characters, it could be done the other way, making the cr's take 
	the ParagraphAttributes of the preceeding char. This way, the attributes would be mostrly preserved.
	I don't know if this is of much use anyway"
	answer fixParagraphAttributesFrom: 1 to: answer size.
	^answer

  "  It is assumed that  self size = runs size  holds. "
]

{ #category : 'adding attributes' }
Text >> rightFlush [
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	self addAttribute: TextAlignment rightFlush
]

{ #category : 'emphasis' }
Text >> runLengthFor: characterIndex  [
	"Answer the count of characters remaining in run beginning with 
	characterIndex."

	^runs runLengthAt: characterIndex
]

{ #category : 'private' }
Text >> runs [
	^runs
]

{ #category : 'private' }
Text >> setString: aString setRuns: anArray [
	"Warning. No attempt is done to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	Use with care. "

	string := aString.
	self privateSetRuns: anArray
]

{ #category : 'adding emphasis' }
Text >> shoutDisableEmphasis [

	self addAttribute: ShoutDisable new
]

{ #category : 'accessing' }
Text >> size [
	^runs size
]

{ #category : 'printing' }
Text >> storeOn: aStream  [
	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)
]

{ #category : 'accessing' }
Text >> string [
	"Answer the string representation of the receiver."
	^string
]

{ #category : 'adding emphasis' }
Text >> struck [
	"Stuff like
	('Hello world' struck ) edit
	"
	self addAttribute: TextEmphasis struckThrough
]

{ #category : 'adding emphasis' }
Text >> sub [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis subscript
]

{ #category : 'adding emphasis' }
Text >> super [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis superscript
]

{ #category : 'converting' }
Text >> thatCanHoldAnyCodePoint [
	"See senders.
	Also see #asStreamResult	"
	string := string thatCanHoldAnyCodePoint
]

{ #category : 'converting' }
Text >> translated [
	"In Squeak, #translated does localization of Strings. This method helps with compatibility.
	Additionally, applications written in Cuis could do the same.
	This method should be redefined in a Localization package."
]

{ #category : 'adding emphasis' }
Text >> under [
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ℵ' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis underlined
]

{ #category : 'emphasis' }
Text >> withAttributeValues: attributes do: aBlock [
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor lineSpacingFactor |
	paragraphStyle := nil.
	characterStyle := nil.
	familyName := nil.
	pointSize := nil.
	emphasis := 0.
	alignment := nil.
	color := nil.
	backgroundColor := nil.
	lineSpacingFactor := Preferences at: #defaultLineSpacingFactor.
	
	"ParagraphStyle is the first to set several values"
	attributes do: [ :attribute |
		attribute forParagraphStyleReferenceDo: [ :s |
			paragraphStyle := s.
			familyName := s familyName.
			pointSize := s pointSize.
			emphasis := s emphasis.
			alignment := s alignment.
			s color ifNotNil: [ :c | color := c ]]].

	"CharacterStyle, if present, can override font and color"
	attributes do: [ :attribute |
		attribute forCharacterStyleReferenceDo: [ :s |
			characterStyle := s.
			familyName := s familyName.
			pointSize := s pointSize.
			emphasis := s emphasis.
			s color ifNotNil: [ :c | color := c ]]].

	"These will not interfere with each other, and all of them take precedence over previous values"
	attributes do: [ :attribute |
		attribute forFontFamilyDo: [ :fn | familyName := fn ].
		attribute forFontSizeDo: [ :ps | pointSize := ps ].
		attribute forTextEmphasisDo: [ :e | emphasis := emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color := c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor := c ].
		attribute forTextAlignmentDo: [ :a | alignment := a ].
		attribute forLineSpacingDo: [ :f | lineSpacingFactor := f ].
	].
	
	"Done. Now evaluate the block."
	^ aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor. lineSpacingFactor }
]

{ #category : 'adding color' }
Text >> yellow [
	"Stuff like
	'Hello world' yellow edit
	"
	self color: Color yellow
]
