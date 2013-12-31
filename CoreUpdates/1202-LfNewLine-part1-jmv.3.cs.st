'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 14 March 2012 at 11:50:46 am'!!Character methodsFor: 'testing' stamp: 'jmv 3/13/2012 15:41'!    isLineSeparator	"Answer whether the receiver is a line separator character:	cr, line feed, or form feed."	value = 10 ifTrue: [^true].	"line feed"	value = 13 ifTrue: [^true].	"cr"	value = 12 ifTrue: [^true].	"form feed"	^false! !!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/13/2012 11:18'!                     crCharacter	"Answer the Character representing a carriage return."	^self value: 13! !!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/13/2012 11:18'!         lfCharacter	"Answer the Character representing a linefeed."	^self value: 10! !!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/14/2012 09:20'!                newLineCharacter
	"Answer the Character representing a newLine, that is, a linefeed.
	This should be the only method in the system that knows about this detail.
	Everybody else should eventually ask us."

	^self lfCharacter! !!CharacterScanner methodsFor: 'stop conditions' stamp: 'jmv 3/14/2012 09:11'!             doNewLine
	^self subclassResponsibility! !!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 3/14/2012 09:11'!doNewLine
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a carriage return stop condition that has just been 
	encountered. The ParagraphEditor convention is to denote selections by 
	CharacterBlocks, sometimes including the carriage return (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex notNil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue:	["When off end of string, give data for next character"
				destY _ destY +  line lineHeight.
				lastCharacter _ nil.
				characterPoint _ leftMargin @ destY.
				lastIndex _ lastIndex + 1.
				self lastCharacterExtentSetX: 0.
				^ true].
		lastCharacter _ Character newLineCharacter.
		characterPoint _ destX @ destY.
		self lastCharacterExtentSetX: rightMargin - destX.
		^true! !!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 3/14/2012 09:11'!                            doNewLine
	"Answer true. Set up values for the text line interval currently being 
	composed."

	line stop: lastIndex.
	spaceX _ destX.
	line paddingWidth: rightMargin - spaceX - 1.
	^true! !!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 3/14/2012 09:13'!                 newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter! !!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 3/14/2012 09:13'!  newLine: aKeyboardEvent

	self addString: String newLineString.
	^false! !!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 3/13/2012 15:56'!          returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].

	aKeyboardEvent controlKeyPressed ifTrue: [
		"Unoperative on the Mac, as the VM sends the same event buffer for cmd-m and cmd-return..."
		self addString: String crString.
		^false ].
	aKeyboardEvent shiftPressed ifTrue: [
		self addString: String lfString.
		^false ].
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		self addString: String crlfString.
		^false ].

	^ self newLine: aKeyboardEvent! !!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 12:12'!                             newLine	"Append a newLine character to the receiver.	The Cuis convention is to use lf on output."	self bufferStream newLine! !!InputSensor class methodsFor: 'constants' stamp: 'jmv 3/13/2012 10:16'!                  returnKey	"For historic reasons, return key is sent by the VM as character 13, even if Cuis uses Lf as the line terminator character"	^ 13! !!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 3/13/2012 10:18'!           isReturnKey	"Answer true if the return key (called Enter in many PC keyboards) was pressed"	^keyValue = InputSensor returnKey! !!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 3/14/2012 09:11'!              doNewLine
	"When a newLine is encountered, simply increment the pointer 
	into the paragraph."

	lastIndex_ lastIndex + 1.
	^false! !!SocketStream methodsFor: 'stream out' stamp: 'jmv 3/14/2012 09:13'!                   newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPutAll: String newLineString! !!StrikeFont methodsFor: 'character shapes' stamp: 'jmv 3/13/2012 17:17'!      makeCrInvisible
	self characterToGlyphMap.
	characterToGlyphMap at: 14 put: (14 < minAscii ifFalse: [14] ifTrue: [maxAscii+1])! !!StrikeFont methodsFor: 'character shapes' stamp: 'jmv 3/13/2012 17:22'!                    makeCrVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self characterFormAt: (Character value: 182).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self characterFormAt: (Character value: 133) put: glyph.
	characterToGlyphMap at: 14 put: 133! !!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 3/13/2012 17:22'!       makeCrInvisible
	"
	Make carriage return characters invisible
	StrikeFont makeCrInvisible
	"
	self allInstances do: [ :font | font makeCrInvisible ]! !!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 3/13/2012 17:16'!                        makeCrVisible
	"
	Make carriage return characters visible
	StrikeFont makeCrVisible
	"
	self allInstances do: [ :font | font makeCrVisible ]! !!String methodsFor: 'converting' stamp: 'jmv 3/14/2012 09:14'!               withCuisLineEndings
	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Replace each occurence with a single Lf"

	^ self withLineEndings: String newLineString! !!String methodsFor: 'converting' stamp: 'jmv 3/14/2012 09:14'!                        withNewLines
	"Return a copy of the receiver in which backslash (\) characters have been replaced with newLine (i.e. Lf)."

	^ self collect: [ :c | c = $\ ifTrue: [ Character newLineCharacter ] ifFalse: [ c ]].! !!String class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 11:42'!                            crString	"Answer a string containing a single carriage return character."	^ self with: Character crCharacter! !!String class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 11:42'!crlfString	"Answer a string containing a carriage return and a linefeed."	^ self with: Character crCharacter with: Character lfCharacter! !!String class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 16:49'!   lfString	"Answer a string containing a single Lf character."	^ self with: Character lfCharacter! !!String class methodsFor: 'instance creation' stamp: 'jmv 3/14/2012 08:33'!             newLineString	"Answer a string containing a single newLine (i.e. Lf) character."	^ self with: Character newLineCharacter! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/14/2012 10:06'!                    fixSourceCodeLineEndings
	"
	Smalltalk fixSourceCodeLineEndings
	"
	"Scan all methods for source code with Carriage Returns.
	Replaces all occurrences of<CR> or  <CR><LF> by <LF>."

	| oldCodeString n newCodeString oldStamp oldCategory m oldClassComment newClassComment c o stamp |
'Scanning sources for old Line Ending conventions.
This will take a few moments...'
	displayProgressAt: Sensor mousePoint
	from: 0
	to: CompiledMethod instanceCount
	during: [ :bar |
		n _ 0.
		m _ 0.
		c _ 0.
		Smalltalk allBehaviorsDo: [ :cls | 
			cls selectors do: [ :selector | 
				(n _ n+1) \\ 100 = 0 ifTrue: [ bar value: n ].
				oldCodeString _ (cls sourceCodeAt: selector) asString.
				newCodeString _ oldCodeString withCuisLineEndings.
				newCodeString = oldCodeString ifFalse: [
					oldStamp _ Utilities timeStampForMethod: (cls compiledMethodAt: selector).
					oldCategory _ cls whichCategoryIncludesSelector: selector.
					cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.
					m _ m + 1].
				cls isMeta ifFalse: [
					o _ cls organization.
					oldClassComment _ o classComment.
					stamp _ o commentStamp.
					newClassComment _  oldClassComment withCuisLineEndings.
					newClassComment = oldClassComment ifFalse: [
						cls classComment: newClassComment stamp: stamp.
						c _ c + 1 ]]
				]].
	].
	Transcript newLine; show: m printString , ' methods were fixed.'.
	Transcript newLine; show: c printString , ' text class comments were fixed.'.! !!Text methodsFor: 'converting' stamp: 'jmv 3/14/2012 09:18'!          withCuisLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>"

	| newText wrongLineEnd |
	wrongLineEnd _ String crlfString detect: [ :char | (char = Character newLineCharacter) not ].
	(string includes: wrongLineEnd) ifFalse: [ ^self copy ].
	newText _ self copyReplaceAll: String crlfString with: String newLineString asTokens: false.
	(newText asString includes: wrongLineEnd) ifFalse: [ ^newText ].
	^newText copyReplaceAll: wrongLineEnd asString with: String newLineString asTokens: false.! !!TextEditor methodsFor: 'editing keys' stamp: 'jmv 3/13/2012 16:25'!                  changeLineEndsToLf: aKeyboardEvent	"Replace all CRs and CrLfs by LFs.	Triggered by Cmd-U -- useful when getting code from FTP sites"	"This is a user command, and generates undo"		self replaceSelectionWith: self selection string withCuisLineEndings.	^ true! !!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 3/14/2012 09:13'!  newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex |
	s _ self privateCurrentString.
	stopIndex _ self stopIndex.
	i _ stopIndex.
	tabCount _ 0.
	[ (i _ i-1) > 0 and: [ (char _ s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char = $[ ifTrue: [tabCount _ tabCount + 1].
		char = $] ifTrue: [tabCount _ tabCount - 1]].
	[ (i _ i + 1) < stopIndex and: [ (char _ s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount _ tabCount + 1 ]].
	"Now inject newline with tabCount tabs"
	self addString: (String streamContents: [ :strm | strm newLineTab: tabCount ]).
	^ false! !!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 3/13/2012 12:10'!       newLine	"WriteStream protocol.	In the older TranscriptStream, it added a CR character.	Now, finish the current incomplete entry."	self finishEntry! !!WriteStream methodsFor: 'character writing' stamp: 'jmv 3/14/2012 09:14'!                        newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter! !!WriteStream methodsFor: 'character writing' stamp: 'jmv 3/14/2012 09:14'!  newLineTab: anInteger
	"Append a newLine character, followed by anInteger tab characters, to the receiver."

	self nextPut: Character newLineCharacter.
	anInteger timesRepeat: [self nextPut: Character tab]! !