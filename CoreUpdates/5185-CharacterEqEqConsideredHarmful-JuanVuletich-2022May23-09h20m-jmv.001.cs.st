'From Cuis 6.0 [latest update: #5183] on 23 May 2022 at 9:40:11 am'!

!Browser methodsFor: 'class functions' stamp: 'jmv 5/21/2022 22:29:19'!
explainSpecial: string 
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last = $: ifFalse: [^nil].
			lits _ Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole _ lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply _ '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes _ Smalltalk allClassesImplementing: whole.
			classes _ 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 5/21/2022 22:29:24'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector class oldSelector |

	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [aString isEmpty]) ifTrue: [ ^ false].
		(aString count: [:char | char = $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: aString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: aString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!Integer methodsFor: 'arithmetic' stamp: 'di 11/6/1998 14:00'!
/ aNumber
	"Refer to the comment in Number / "
	| quoRem |
	aNumber isInteger ifTrue:
		[quoRem _ self digitDiv: aNumber abs	"*****I've added abs here*****"
						neg: self negative ~= aNumber negative.
		(quoRem at: 2) = 0
			ifTrue: [^ (quoRem at: 1) normalize]
			ifFalse: [^ (Fraction numerator: self denominator: aNumber) reduced]].
	^ aNumber adaptToInteger: self andSend: #/! !


!Character methodsFor: 'fileman-testing' stamp: 'jmv 5/21/2022 22:29:28'!
isDriveSeparator
	^self = $:
	! !


!String methodsFor: 'printing' stamp: 'jmv 5/21/2022 22:29:53'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do: [ :i |
		aStream nextPut: (x _ self at: i).
		x = $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !

!String methodsFor: 'private' stamp: 'jmv 5/21/2022 22:29:38'!
getEnclosedExpressionFrom: aStream 
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"
	| result currentChar |
	result := String new writeStream.

	[aStream atEnd 
		or: [(currentChar := aStream next) = $}]]
		whileFalse: [result nextPut: currentChar].

	^ result contents withBlanksTrimmed! !

!String methodsFor: 'formatting' stamp: 'jmv 5/21/2022 22:29:35'!
format: aCollection 
	"format the receiver with aCollection  
	 
	simplest example:  
	'foo {1} bar' format: {Date today}.
	 
	complete example:  
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}.  
	"
	| result stream |
	result := String new writeStream.
	stream := self readStream.

	[stream atEnd]
		whileFalse: [| currentChar | 
			currentChar := stream next.
			currentChar = ${
				ifTrue: [| expression | 
					expression := self getEnclosedExpressionFrom: stream.
					result
						nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
				ifFalse: [
					currentChar = $\
						ifTrue: [stream atEnd
								ifFalse: [result nextPut: stream next]]
						ifFalse: [result nextPut: currentChar]]].

	^ result contents! !


!Symbol class methodsFor: 'services' stamp: 'jmv 5/21/2022 22:30:35'!
possibleSelectorsFor: misspelled 
	"Answer an ordered collection of possible corrections
	for the misspelled selector in order of likelyhood"

	| numArgs candidates lookupString best binary short long first ss |
	lookupString _ misspelled asLowercase. "correct uppercase selectors to lowercase"
	numArgs _ lookupString numArgs.
	(numArgs < 0 or: [lookupString size < 2]) ifTrue: [^ OrderedCollection new: 0].
	first _ lookupString first.
	short _ lookupString size - (lookupString size // 4 max: 3) max: 2.
	long _ lookupString size + (lookupString size // 4 max: 3).

	"First assemble candidates for detailed scoring"
	candidates _ OrderedCollection new.
	self allSymbolTablesDo: [:s | (((ss _ s size) >= short	"not too short"
			and: [ss <= long			"not too long"
					or: [(s at: 1) = first]])	"well, any length OK if starts w/same letter"
			and: [s numArgs = numArgs])	"and numArgs is the same"
			ifTrue: [candidates add: s]].

	"Then further prune these by correctAgainst:"
	best _ lookupString correctAgainst: candidates.
	((misspelled last ~= $:) and: [misspelled size > 1]) ifTrue: [
		binary _ misspelled, ':'.		"try for missing colon"
		Symbol hasInterned: binary ifTrue: [:him | best addFirst: him]].
	^ best! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 5/21/2022 22:29:58'!
nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	FromCharTable ifNil: [ self class initializeTables ].
	[
		raw := mimeStream next.
		raw ifNil: [^ nil].	"end of stream"
		raw = $= ifTrue: [^ nil].
		num := FromCharTable at: raw asciiValue + 1.
		num ifNotNil: [^ num].
		"else ignore space, return, tab, ..."
	] repeat! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/21/2022 22:24:35'!
nextDelimited: terminator
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character.  For example: 'this '' was a quote'. Start postioned before the initial terminator."

	| out ch |
	out _ WriteStream on: (String new: 1000).
	self atEnd ifTrue: [^ ''].
	self next = terminator ifFalse: [self skip: -1].	"absorb initial terminator"
	[(ch _ self next) == nil] whileFalse: [
		(ch = terminator) ifTrue: [
			self peek = terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!!"
			].
		].
		out nextPut: ch.
	].
	^ out contents! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/21/2022 22:24:22'!
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator out ch |
	terminator _ $!!.
	out _ WriteStream on: (String new: 1000).
	self skipSeparators.
	[(ch _ self next) == nil] whileFalse: [
		(ch = terminator) ifTrue: [
			self peek = terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!!"
			].
		].
		out nextPut: ch.
	].
	^ out contents! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:26:26'!
isAt: aChar
	
	^ hereChar = aChar and: [aheadChar = aChar ifTrue: [self step. false] ifFalse: [true]]! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:26:59'!
readUpToNext: aChar ifNotFound: aNotFoundBlock

	self step.
	buffer reset.
	
	[self isAt: aChar]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = DoItCharacter and: [source atEnd]) ifTrue: [^aNotFoundBlock value ]].
	
	self step.
	token := buffer contents.! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:30:01'!
xColon
	"Allow := for assignment"
	
	aheadChar = $= ifTrue:
		[self step.
		tokenType := #leftArrow.
		self step.
		^ token := #':='].
	"Otherwise, just do what normal scan of colon would do"
	tokenType := #colon.
	^ token := self step asSymbol! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:38:28'!
xDigit
	"Form a number."

	tokenType := #number.
	(aheadChar = DoItCharacter and: [source atEnd
			and:  [source skip: -1. source next ~= DoItCharacter]])
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:38:36'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
    aStream := WriteStream on: (String new: 200).
    self step.
    [hereChar = $"]
        whileFalse:
            [(hereChar = stopChar and: [source atEnd])
                ifTrue: [^self offEnd: self class unmatechedCommentQuoteErrorDescription].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/21/2022 22:30:42'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore]]] whileTrue:
			["open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
							ifTrue: [DoItCharacter "doit"]
							ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~= $=]])
					ifTrue: 
						[buffer nextPut: self step.
						"Allow any number of embedded colons in literal symbols"
						[(self typeTableAt: hereChar) == #xColon] whileTrue:
							[buffer nextPut: self step].
						#keyword]
					ifFalse: 
						[#word].
	token := buffer contents! !


!Parser methodsFor: 'pragmas' stamp: 'jmv 5/21/2022 22:31:42'!
pragmaLiteral: selectorSoFar
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here = $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ (Scanner new scanTokens: self advance) first ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'! !

!Parser methodsFor: 'scanning' stamp: 'jmv 5/21/2022 22:38:43'!
compensateTwoCharacterLookahead

	^source position - (aheadChar = DoItCharacter ifTrue: [hereChar = DoItCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2])
	! !

!Parser methodsFor: 'error correction' stamp: 'jmv 5/21/2022 22:30:51'!
correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection delta adjustedSpots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection _ requestor selectionInterval.

	delta := self sourceDelta.
	adjustedSpots := aSpots collect: [ :interval | interval first - delta to: interval last - delta ].
	requestor selectFrom: adjustedSpots first first to: adjustedSpots last last.

	correctSelector _ UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: adjustedSpots.
	^(proposedKeyword last ~= $:
	   and: [correctSelector last = $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !


!TextEditor methodsFor: 'private' stamp: 'jmv 5/21/2022 23:02:52'!
indent: delta fromStream: inStream toStream: outStream
	"Append the contents of inStream to outStream, adding or deleting delta or -delta
	 tabs at the beginning, and after every NewLine except a final NewLine.  Do not add tabs
	 to totally empty lines, and be sure nothing but tabs are removed from lines."

	| ch skip tab prev atEnd |
	tab _ Character tab.
	delta > 0
		ifTrue: [
			"shift right"
			prev _ Character newLineCharacter.
			[
			ch _ (atEnd _ inStream atEnd)
				ifTrue: [ Character newLineCharacter ]
				ifFalse: [ inStream next ].
			(prev isLineSeparator and: [ ch isLineSeparator not ]) ifTrue: [ delta timesRepeat: [ outStream nextPut: tab ]].
			atEnd ] whileFalse: [
				outStream nextPut: ch.
				prev _ ch ]]
		ifFalse: [
			"shift left"
			skip _ delta.
			"a negative number"
			[ inStream atEnd ] whileFalse: [
				((ch _ inStream next) = tab and: [ skip < 0 ]) ifFalse: [ outStream nextPut: ch ].
				skip _ ch isLineSeparator
					ifTrue: [ delta ]
					ifFalse: [ skip + 1 ]]]! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 5/21/2022 22:31:49'!
explainPartSel: string 
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	(msg _ provider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last = $: ifFalse: [^ nil].
	"Name of this method"
	lits _ Array with: msg.
	(whole _ lits detect: [:each | (each keywords detect: [:frag | frag = string]
					ifNone: nil) notNil]
				ifNone: nil)
		ifNotNil: [
			reply _ ', which is the selector of this very method!!'.
			s _ self class plateA]
		ifNil: [ 
			"Selectors called from this method"
			lits _ (provider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(whole _ lits detect: [:each | (each keywords detect: [ :frag | frag = string ]
							ifNone: nil) notNil]
						ifNone: nil) notNil
				ifFalse: [string = 'primitive:'
					ifTrue: [^self explainChar: '<']
					ifFalse: [^nil]].
			reply _ '.'.
			s _ self class plateB].
		classes _ Smalltalk allClassesImplementing: whole.
		classes size > 12
			ifTrue: [classes _ 'many classes']
			ifFalse: [classes _ 'these classes ' , classes printString].
		^ String streamContents: [:str |
			str
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]! !


!TextAction methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 16:44'!
analyze: aString
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 _ aString indexOf: $<.
	b2 _ aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param _ self validate: aString.
		^ Array with: param with: (param size = 0 ifTrue: [nil] ifFalse: [param])].
	"Two parts"
	trim _ aString withBlanksTrimmed.
	trim first = $< 
		ifTrue: [ trim last = $>
			ifTrue: ["only instructions" 
				param _ self validate: (aString copyFrom: b1+1 to: b2-1).
				show _ param size = 0 ifTrue: [nil] ifFalse: [param]]
			ifFalse: ["at the front"
				param _ self validate: (aString copyFrom: b1+1 to: b2-1).
				show _ param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [ trim last = $>
			ifTrue: ["at the end"
				param _ self validate: (aString copyFrom: b1+1 to: b2-1).
				show _ param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show _ nil]].
	^ Array with: param with: show
! !


!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:32:35'!
classListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of classes and a 
	code pane, and I also have a listView that has a list of methods.  The 
	view knows how to get the list and selection."

	aChar = $r ifTrue: [^ model recent].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $x ifTrue: [^ model removeClass].
	aChar = $t ifTrue: [^ model runClassTests ].
	
	^ self messageListKey: aChar from: view! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:32:48'!
messageCatListKey: aChar from: view

	aChar = $o ifTrue: [^ model fileOutMessageCategories ].
	aChar = $t ifTrue: [^ model runMessageCategoryTests ].
	aChar = $x ifTrue: [^ model removeMessageCategory ].
	aChar = $R ifTrue: [ ^model renameCategory ].
	aChar = $n ifTrue: [^model addCategory ].
	aChar = $e ifTrue: [^model removeEmptyCategories ].
	aChar = $c ifTrue: [^model categorizeAllUncategorizedMethods ].
	aChar = $a ifTrue: [ ^ model alphabetizeMessageCategories ].
	aChar = $r ifTrue: [ ^ model editMessageCategories ]! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:38:59'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	aChar = $D ifTrue: [^ model toggleDiffing].

	sel _ model selectedMessageName.
	aChar = $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar = $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class _ model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar = $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar = $N ifTrue: [^ self browseClassRefs].
	aChar = $i ifTrue: [^ self methodInheritance].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $o ifTrue: [^ model fileOutMessage].
			aChar = $c ifTrue: [^ model copySelector].
			aChar = $v ifTrue: [^ self browseVersions].
			aChar = $C ifTrue: [^ model showHomeCategory].
			aChar = $O ifTrue: [^ self openSingleMessageBrowser].
			aChar = $x ifTrue: [^ model removeMessage].
			aChar = $t ifTrue: [^ model runMethodTest].
			aChar = $r ifTrue: [^ model debugMethodTest]]
		ifNil: [
			aChar = $R ifTrue: [^ model renameClass]]! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:20'!
systemCatListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also have a listView that has a list of methods.  The view knows how to get the list and selection."

	aChar = $f ifTrue: [ ^ self findClass ].
	aChar = $x ifTrue: [ ^ model removeSystemCategory ].
	aChar = $t ifTrue: [ ^ model runSystemCategoryTests ].
	aChar = $a ifTrue: [ ^ model addSystemCategory ].
	aChar = $A ifTrue: [ ^ model alphabetizeSystemCategories ].
	aChar = $b ifTrue: [ ^ self openSystemCategoryBrowser ].
	aChar = $B ifTrue: [ ^ self browseAllClasses ].
	aChar = $o ifTrue: [ ^ model fileOutSystemCategory ].
	aChar = $u ifTrue: [ ^ model updateSystemCategories ].
	aChar = $R ifTrue: [ ^ model renameSystemCategory ].
	
	^ self classListKey: aChar from: view! !


!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:25'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel |
	sel _ model selectedMessageName.
	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $R ifTrue: [^ self renameSelector].
			aChar = $U ifTrue: [^ self addParameter ].
			aChar = $I ifTrue: [^ self removeParameter ]].
	super messageListKey: aChar from: view! !

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:29'!
systemCatListKey: aChar from: view

	aChar = $r ifTrue: [^ model recent ].
	
	^super systemCatListKey: aChar from: view! !


!CodeFileBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:34'!
classListKey: aChar from: view
	aChar = $b ifTrue: [^ self browseMethodFull].
	aChar = $N ifTrue: [^ self browseClassRefs].
	self codeFileListKey: aChar from: view! !

!CodeFileBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:37'!
codeFileListKey: aChar from: view

	aChar = $f ifTrue: [^ self findClass]! !

!CodeFileBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:40'!
messageListKey: aChar from: view
	aChar = $b ifTrue: [^ self browseMethodFull].
	super messageListKey: aChar from: view! !


!HierarchyBrowserWindow class methodsFor: 'examples' stamp: 'jmv 5/21/2022 22:33:45'!
example2
	"Put up a ClassListBrowser that shows all classes whose names start with the letter S"

	self forClassesNamed: (Smalltalk allClasses collect: [:c | c name] thenSelect: [:aName | aName first = $S]) title: 'All classes starting with S'

"HierarchyBrowserWindow example2"! !


!MessageNamesWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:51'!
selectorListKey: aChar from: view
	"Respond to a Command key in the message-list pane."
	aChar = $n ifTrue: [ ^ self browseSenders ].
	aChar = $b ifTrue: [ ^ self browseMethodFull ].
	aChar = $m ifTrue: [ ^ self browseImplementors ].! !


!ProtocolBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:33:58'!
protocolClassListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of 
	classes and a list of methods.  The 
	view knows how to get the list and selection."

	aChar = $b ifTrue: [^ self browseMethodFull ].
	aChar = $h ifTrue: [^ self browseHierarchy].
	
	^ nil! !


!ChangeListWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:02'!
changeListKey: aChar from: view
	"Respond to a Command key in the list pane."

	aChar = $D ifTrue: [^ model toggleDiffing].
	aChar = $a ifTrue: [^ model selectAll]! !


!VersionsBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:07'!
changeListKey: aChar from: view
	"Respond to a Command key in the list pane. of the versions browser"

	aChar = $z ifTrue: [^ model fileInSelections].
	^ self messageListKey: aChar from: view! !


!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:18'!
changeSetListKey: aChar from: view
	"Respond to a Command key.  I am a model with a listView that has a list of changeSets."

	aChar = $D ifTrue: [^ model toggleDiffing]. 
	aChar = $o ifTrue: [^ model fileOutAndRemove].
	aChar = $k ifTrue: [^ model fileOutAndKeep].
	aChar = $r ifTrue: [^ model rename].
	aChar = $x ifTrue: [^ model remove].

	^ self messageListKey: aChar from: view! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:24'!
classListKey: aChar from: view
	"Respond to a Command key in the class-list pane."

	aChar = $x ifTrue: [^ model removeClass].
	aChar = $d ifTrue: [^ model forgetClass]. 

	^ self messageListKey: aChar from: view "picks up b,h,p"! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:28'!
messageListKey: aChar from: view
	"Respond to a Command key in the message-list pane."

	aChar = $d ifTrue: [^ model forget].
	super messageListKey: aChar from: view! !


!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:37'!
contextStackKey: aChar from: view
	"Respond to a keystroke in the context list"

	aChar = $e ifTrue: [^ self send].
	aChar = $t ifTrue: [^ self doStep].
	aChar = $T ifTrue: [^ self stepIntoBlock].
	aChar = $p ifTrue: [^ self proceed].
	aChar = $r ifTrue: [^ self restart].
	aChar = $f ifTrue: [^ self fullStack].
	aChar = $w ifTrue: [^ self where].

	^ self messageListKey: aChar from: view! !

!DebuggerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:47'!
inspectorKey: aChar from: view
	"Respond to a Command key issued while the cursor is over my field list"

	aChar = $i ifTrue: [^ self inspectSelectionIn: view model ].
	aChar = $I ifTrue: [^ self exploreSelectionIn: view model ].
	aChar = $b ifTrue:[^ self browseMethodFullIn: view model ].
	aChar = $h ifTrue:[^ self browseHierarchyIn: view model].
	aChar = $p ifTrue: [^ self browseFullProtocolIn: view model].
	aChar = $N ifTrue: [^ self browseClassRefsIn: view model]! !


!FileListWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:34:54'!
fileListKey: aChar from: aView

	aChar = $x ifTrue: [ ^ aView model deleteFile ].
	aChar = $R ifTrue: [ ^ aView model renameFile ].
	aChar = $n ifTrue: [ ^ aView model addNewFile ].
	aChar = $N ifTrue: [ ^ aView model addNewDirectory ].! !


!InspectorWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:35:03'!
inspectorKey: aChar from: view
	"Respond to a Command key issued while the cursor is over my field list"

	aChar = $i ifTrue: [^ self inspectSelection].
	aChar = $I ifTrue: [^ self exploreSelection].
	aChar = $b ifTrue:[^ self browseMethodFull].
	aChar = $h ifTrue:[^ self browseHierarchy].
	aChar = $p ifTrue: [^ self browseFullProtocol].
	aChar = $N ifTrue: [^ self browseClassRefs]! !


!ObjectExplorerWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/21/2022 22:35:17'!
explorerKey: aChar from: view

	model getCurrentSelection ifNotNil: [

		aChar = $i ifTrue: [^ self inspectSelection].
		aChar = $I ifTrue: [^ self exploreSelection].

		aChar = $b ifTrue: [^ self browseMethodFull].
		aChar = $h ifTrue: [^ self browseHierarchy].
		aChar = $p ifTrue: [^ self browseFullProtocol].
		aChar = $N ifTrue: [^ self browseClassRefs]]! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 5/21/2022 22:35:22'!
shouldOpenMorphWhenPrefixAt: currentPos and: currentChar 
	
	^ model textSize >= currentPos and: [ currentChar isAlphaNumeric or: [ currentChar = $: ]] 


	! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:49:54'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := String with: c.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c = $:]) ifFalse: [^currentToken].
	(c = $: and: [d = $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , (String with: d).
			self nextChar.
			^currentToken].
	"|| empty temp declaration"
	"This makes || colorize as an invalid method definition.
	It is better to allow #|| as valid selector, and mark an empty temp declaration as invalid instead!!"
	"(c = $| and: [d = $|])
		ifTrue: [^currentToken]."
	c _ d.
	[
		d _ self peekChar.
		c = $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken _ currentToken copyWith: c.
			c _ self nextChar ].
	^currentToken! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:45:35'!
scanComment
	| c s e |
	s := sourcePosition.
	
	[sourcePosition := sourcePosition + 1.
	(c := self currentChar) 
		ifNil: [
			self rangeType: #unfinishedComment start: s end: source size.
			^self error	": 'unfinished comment'"].
	c = $"] 
		whileFalse: [].
	e := sourcePosition.
	s < e ifTrue: [self rangeType: #comment start: s end: e].
	self nextChar.
	self scanWhitespace! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:45:41'!
scanIdentifier
	| c start |
	start _ sourcePosition.
	[ (c _ self nextChar) isValidInIdentifiers ] whileTrue: [].
	(c = $: and: [(self isBinarySelectorCharacter: self peekChar) not]) 
		ifTrue: [self nextChar].
	currentToken _ source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition _ start! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:39:36'!
scanNumber
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	('rx' includes: c)
		ifTrue: [
			base := Integer readFrom: (ReadStream on: (source copyFrom: start to: sourcePosition - 1)).
			self peekChar = $- ifTrue: [self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c = $. 
				ifTrue: [
					(self isBigDigit: self nextChar base: base) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipBigDigits: base]].
			c := self currentChar.
			('deqp'includes: c)
				ifTrue: [
					((nc := self nextChar) isDigit or: [nc = $- and:[self peekChar isDigit]]) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			c = $s 
				ifTrue: [
					self nextChar isDigit 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c = $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits.].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c = $. 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [
					sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^currentTokenSourcePosition := start]
				ifTrue: [self skipDigits]].
	c := self currentChar.
	('deqp' includes: c)
		ifTrue: [
			((nc := self nextChar) isDigit or: [nc = $-  and:[self peekChar isDigit]]) 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	c = $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^currentTokenSourcePosition := start! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/21/2022 22:45:45'!
scanWhitespace
	| c |
	
	[c := self currentChar.
	c notNil and: [c isSeparator]] 
		whileTrue: [sourcePosition := sourcePosition + 1].
	c = $" ifTrue: [self scanComment]! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 5/21/2022 22:35:28'!
isBinary
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	"Special case: '::' is not a binary selector but the Chain operator"
	(sourcePosition - currentTokenSourcePosition = 1 and: [
		(source at: currentTokenSourcePosition ifAbsent: nil) = $: and: [		
			(source at: sourcePosition ifAbsent: nil) = $: ]])
				ifTrue: [^ false ].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		((self isBinarySelectorCharacter: c) or: [c = $:])
			ifFalse: [^false]].
	^true! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 5/21/2022 22:35:32'!
isKeyword
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last = $: ]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:36'!
parseArray
	[currentTokenFirst = $)] whileFalse: [self parseLiteralArrayElement].
	self scanPast: #arrayEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:41'!
parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst = $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst = $`.
	self scanPast: #backtick.
	self leaveBlock! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:49'!
parseBlock

	"Just read $["
	blockDepths add: blockDepth+1.
	blockDepthsStartIndexes add: sourcePosition-1.

	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst = $: ifTrue: [self parseBlockArguments].
	currentTokenFirst = $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst = $].

	"Just read $]"
	blockDepths add: blockDepth-1.
	blockDepthsStartIndexes add: sourcePosition.

	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:55'!
parseBlockArguments
	[ currentTokenFirst = $: ]
		whileTrue: [
			self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg ].
	currentTokenFirst = $| 
		ifTrue: [ self scanPast: #blockArgsBar ]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:35:59'!
parseBlockTemporaries
	currentTokenFirst = $| 
		ifTrue: [
			self scanPast: #blockTempBar.
			[self isName] 
				whileTrue: [self scanPast: #blockPatternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #blockTempBar]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:05'!
parseBraceArray
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst = $}.
	self scanPast: #rightBrace level: braceDepth.
	braceDepth := braceDepth - 1! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:12'!
parseByteArray
	"Literal ByteArray or literal FloatArray"
	[currentTokenFirst = $]] whileFalse: [
		currentTokenFirst isDigit | (currentTokenFirst = $-)
			ifTrue: [
				"do not parse the number, can be time consuming"
				self scanPast: #number]
			ifFalse: [
				self failWhen: currentTokenFirst = $. .
				self error]].
	self scanPast: #arrayEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:15'!
parseCascadeAndChain

	self parseKeyword.
	currentTokenFirst = $; ifTrue:
		[
			self scanPast: #cascadeSeparator.
			^self parseCascadeAndChain
		].
	currentTokenFirst = $: ifTrue:
		[
			self scanPast: #chainSeparator.

			"These lines implement double colon chains"
			currentTokenFirst = $: ifFalse: [^self].
			self scanPast: #chainSeparator.
			
			^self parseCascadeAndChain
		]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:25'!
parseExternalCall
	self scanNext.
	self failWhen: currentToken isNil.
	self scanPast: #externalCallType.
	currentToken = '*' 
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: 	[
			self failUnless: currentTokenFirst = $'.
			self parseString].
	self failUnless: currentTokenFirst = $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~= $)] 
		whileTrue: [
			self failWhen: currentToken isNil.
			self scanPast: #externalCallType.
			currentToken = '*' 
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst = $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:44:55'!
parseLiteral: inArray 
	currentTokenFirst = $$ 
		ifTrue: [
			| pos |
			self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			self scanPast: #character start: pos end: pos.
			^self ].
	currentTokenFirst isDigit 
		ifTrue: [
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentToken = '-' 
		ifTrue: [
			| c |
			c := self currentChar.
			(inArray and: [c isNil or: [c isDigit not]]) 
				ifTrue: [
					"single - can be a symbol in an Array"
					self scanPast: #symbol.
					^self ].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			"token isNil ifTrue: [self error: 'Unexpected End Of Input']."
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentTokenFirst = $' ifTrue: [
		self parseString.
		^self ].
	currentTokenFirst = $# ifTrue: [
		self parseSymbol.
		^self ].
	(inArray and: [currentToken notNil]) ifTrue: [
		self scanPast: #symbol.
		^self ].
	self failWhen: currentTokenFirst = $. .
	self error	": 'argument missing'"! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:39'!
parseLiteralArrayElement
	 currentTokenFirst isValidStartOfIdentifiers ifTrue: [
		#true = currentToken ifTrue: [
			self scanPast: #true.
			^ self ].
		#false = currentToken ifTrue: [
			self scanPast: #false.
			^ self ].
		#nil = currentToken ifTrue: [
			self scanPast: #nil.
			^ self ].
		self scanPast: #symbol.
		^ self ].
	currentTokenFirst = $( ifTrue: [
		self scanPast: #arrayStart.
		self parseArray.
		^ self ].
	self parseLiteral: true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:42'!
parseMethodTemporaries
	currentTokenFirst = $| 
		ifTrue: [
			self scanPast: #methodTempBar.
			[self isName] 
				whileTrue: [self scanPast: #patternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #methodTempBar]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:49'!
parsePrimitive
	self scanNext.
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: [
			self failUnless: currentTokenFirst = $'.
			self parseString.
			currentToken = 'module:' 
				ifTrue: [
					self scanPast: #module.
					self failUnless: currentTokenFirst = $'.
					self parseString]].
	currentToken = 'error:' ifTrue: [
		self scanPast: #primitive. "there's no rangeType for error"
		self isName
			ifTrue: [ self scanPast: #patternTempVar ]
			ifFalse: [ self parseStringOrSymbol ] ].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:36:54'!
parseStatement
	currentTokenFirst = $^ ifTrue: [self scanPast: #return].
	self parseExpression! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:00'!
parseStatementList
	
	[[currentTokenFirst = $.] whileTrue: [self scanPast: #statementSeparator].
	(currentToken notNil and: [currentTokenFirst ~= $]]) 
		ifTrue: [self parseStatement].
	currentTokenFirst = $.] 
			whileTrue: [self scanPast: #statementSeparator]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:03'!
parseStatementListForBraceArray
	"same as parseStatementList, but does not allow empty statements e.g {...$a...}.
	A single terminating . IS allowed e.g. {$a.} "

	
	[currentTokenFirst ~= $} ifTrue: [self parseStatement].
	currentTokenFirst = $.] 
		whileTrue: [self scanPast: #statementSeparator]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:09'!
parseString
	| first c last |
	first _ sourcePosition.
	[
		(c _ self currentChar) ifNil: [
			self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error": 'unfinished string'" ].
		c ~= $' or: [
			self peekChar = $' and: [
				sourcePosition _ sourcePosition + 1.
				true ]]] whileTrue: [ sourcePosition _ sourcePosition + 1 ].
	last _ sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:15'!
parseStringOrSymbol

	currentTokenFirst = $' ifTrue: [ ^self parseString ].
	currentTokenFirst = $# ifTrue: [ ^self parseSymbol ].
	self error! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:45:04'!
parseSymbol
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c _ self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c = $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c = $' ifTrue: [
		self parseSymbolString.
		^self ].
	c = $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	(self isBinarySelectorCharacter: c) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c = $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:29'!
parseSymbolIdentifier
	| c start end |
	c _ self currentChar.
	self failUnless: ( c isValidStartOfIdentifiers or: [ c = $: ]).
	start _ sourcePosition.
	[
		c _ self nextChar.
		c isValidInIdentifiers or: [ c = $: ]
	] whileTrue: [].
	end _ sourcePosition - 1.
	self scanPast: #symbol start: start - 1 end: end! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:37:34'!
parseSymbolString
	| first c last |
	first := sourcePosition.
	self nextChar.
	[
		(c := self currentChar) 
			ifNil: [
				self rangeType: #unfinishedString start: first end: source size.
				self error	": 'unfinished string'"].
		c ~= $' or: [
			self peekChar = $' 
				ifTrue: [sourcePosition := sourcePosition + 1. true] 
				ifFalse: [false]]
	] whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self scanPast: #stringSymbol start: first - 1 end: last! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 5/21/2022 22:45:14'!
parseTerm
	self failWhen: currentToken isNil.
	currentTokenFirst = $( 
		ifTrue: [
			bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst = $).
			self scanPast: #rightParenthesis level: bracketDepth.
			bracketDepth := bracketDepth - 1.
			^self ].
	currentTokenFirst = $[ ifTrue: [^self parseBlock].
	currentTokenFirst = $` ifTrue: [^self parseBacktick].
	currentTokenFirst = ${ 
		ifTrue: [
			braceDepth := braceDepth + 1.
			self scanPast: #leftBrace level: braceDepth.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false! !

