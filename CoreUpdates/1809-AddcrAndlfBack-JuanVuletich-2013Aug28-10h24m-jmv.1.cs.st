'From Cuis 4.2 of 25 July 2013 [latest update: #1808] on 28 August 2013 at 10:31:27.330265 am'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/28/2013 10:25'!
cr
	"Answer the Character representing a carriage return."

	^self value: 13! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/28/2013 10:24'!
lf
	"Answer the Character representing a linefeed."

	^self value: 10! !


!FileList methodsFor: 'private' stamp: 'jmv 8/28/2013 10:26'!
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns |
	patterns _ OrderedCollection new.
	(pattern findTokens: (String with: Character cr with: Character lf with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $#)
				ifTrue: [ patterns add: each]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].

	list _ self listForPatterns: patterns.
	listIndex _ 0.
	fileName _ nil.
	acceptedContentsCache _ ''.
	self changed: #fileList.
	self changed: #updateButtonRow! !


!CharacterScanner class methodsFor: 'class initialization' stamp: 'jmv 8/28/2013 10:26'!
initialize
	"
	CharacterScanner initialize
	"
	| stopConditions |
	stopConditions _ Array new: 258.
	stopConditions atAllPut: nil.
	stopConditions at: Character space asciiValue + 1 put: nil.
	stopConditions at: Character tab asciiValue + 1 put: #tab.

	"This line makes lf be shown as a newLine"
	stopConditions at: Character lf asciiValue + 1 put: #doNewLine.

	"This line makes cr be shown as a newLine"
	stopConditions at: Character cr asciiValue + 1 put: #doNewLine.

	stopConditions at: CharacterScanner endOfRunCode put: #endOfRun.
	stopConditions at: CharacterScanner crossedXCode put: #crossedX.
	DefaultStopConditions _ stopConditions.! !


!String methodsFor: 'accessing' stamp: 'jmv 8/28/2013 10:27'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start _ 1.
	[
		end _ self indexOfAnyOf: CSLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters _ end _ self size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^self ].

		"Account for CrLf sequence"
		endWithoutDelimiters _ end - 1.
		(end < self size
			and: [(self at: end + 1) = Character lf
			and: [(self at: end) = Character cr ]])
				ifTrue: [ end _ end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start _ end + 1 ] repeat! !

!String methodsFor: 'converting' stamp: 'jmv 8/28/2013 10:27'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr _ Character cr.
	lf _ Character lf.
	crlf _ CharacterSet new.
	crlf add: cr; add: lf.

	inPos _ 1.
	outPos _ 1.
	lineEndingSize _ lineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString _ String new: self size * lineEndingSize.

	[
		lineEndPos _ self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos _ outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: self startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (lineEndingString at: i).
				newOutPos _ newOutPos + 1 ].
			outPos _ newOutPos.
			((self at: lineEndPos) = cr and: [ lineEndPos < self size and: [ (self at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos _ lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos _ lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos _ outPos + (self size - inPos).
	outString replaceFrom: outPos to: newOutPos with: self startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !


!String class methodsFor: 'instance creation' stamp: 'jmv 8/28/2013 10:27'!
crString
	"Answer a string containing a single carriage return character."

	^ self with: Character cr! !

!String class methodsFor: 'instance creation' stamp: 'jmv 8/28/2013 10:27'!
crlfString
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
! !

!String class methodsFor: 'instance creation' stamp: 'jmv 8/28/2013 10:28'!
lfString
	"Answer a string containing a single Lf character."

	^ self with: Character lf! !

!String class methodsFor: 'initialization' stamp: 'jmv 8/28/2013 10:27'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 63 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first asciiValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second asciiValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 63 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first asciiValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase asciiValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character allCharacters collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character allCharacters collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character allCharacters collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 8/28/2013 10:27'!
explainChar: string
	"Does string start with a special character?"

	| char |
	char _ string at: 1.
	char = $. ifTrue: [^'"Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float)."'].
	char = $' ifTrue: [^'"The characters between two single quotes are made into an instance of class String"'].
	char = $" ifTrue: [^'"Double quotes enclose a comment.  Smalltalk ignores everything between double quotes."'].
	char = $# ifTrue: [^'"The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants."'].
	(char = $( or: [char = $)]) ifTrue: [^'"Expressions enclosed in parenthesis are evaluated first"'].
	(char = $[ or: [char = $]]) ifTrue: [^'"The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockContext and is usually passed as an argument."'].
	(char = ${ or: [char = $}]) ifTrue: [^ '"A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array"'].
	(char = $< or: [char = $>]) ifTrue: [^'"<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails."'].
	char = $^ ifTrue: [^'"Uparrow means return from this method.  The value returned is the expression following the ^"'].
	char = $| ifTrue: [^'"Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code."'].
	char = $_ ifTrue: [^'"Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it."'].
	char = $; ifTrue: [^'"Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon."'].
	char = $: ifTrue: [^'"A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon)."'].
	char = $$ ifTrue: [^'"The single character following a dollar sign is made into an instance of class Character"'].
	char = $- ifTrue: [^'"A minus sign in front of a number means a negative number."'].
	char = $e ifTrue: [^'"An e in the middle of a number means that the exponent follows."'].
	char = $r ifTrue: [^'"An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base."'].
	char = Character space ifTrue: [^'"the space Character"'].
	char = Character tab ifTrue: [^'"the tab Character"'].
	char = Character cr ifTrue: [^'"the carriage return Character"'].
	char = Character lf ifTrue: [^'"the line feed Character"'].
	^nil! !


!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/28/2013 10:26'!
crCharacter
	"Answer the Character representing a carriage return."
	"Deprecated. To be removed soon. Please use #cr"

	^self cr! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/28/2013 10:25'!
lfCharacter
	"Answer the Character representing a linefeed."
	"Deprecated. To be removed soon. Please use #lf"

	^self lf! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/28/2013 10:28'!
newLineCharacter
	"Answer the Character representing a newLine, that is, a linefeed.
	This should be the only method in the system that knows about this detail.
	Everybody else should eventually ask us."

	^self lf! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 8/28/2013 10:27'!
crLfNextLine
	"Answer next line (may be empty), or nil if at end.
	Support any line ending convention"

	| answer lineSeparators c |
	self atEnd ifTrue: [^nil].
	lineSeparators _ {Character cr. Character lf}.
	answer _ self upToAny: lineSeparators.
	c _ self peek.
	c = Character cr ifTrue: [self next. c _ self peek].
	c = Character lf ifTrue: [self next].
	^answer! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 8/28/2013 10:28'!
classFromPattern: pattern withCaption: aCaption
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & cr characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch _  pattern copyWithoutAll:
			{Character space.  Character cr.  Character tab})
		isEmpty ifTrue: [^ nil].
	Symbol hasInterned: toMatch ifTrue:
		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:
			[:maybeClass | (maybeClass isKindOf: Class) ifTrue: [^ maybeClass]]].

	toMatch _ (toMatch copyWithout: $.) asLowercase.
	potentialClassNames _ Smalltalk classNames asOrderedCollection.
	classNames _ pattern last = $. 
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ nil].
	exactMatch _ classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].
	index = 0 ifTrue: [^ nil].
	^ Smalltalk at: (classNames at: index) asSymbol

"
	Utilities classFromPattern: 'CharRecog'
	Utilities classFromPattern: 'rRecog'
	Utilities classFromPattern: 'znak'
	Utilities classFromPattern: 'orph'
"
! !

String initialize!
CharacterScanner initialize!
