'From Cuis 4.2 of 25 July 2013 [latest update: #2824] on 16 June 2016 at 12:12:28.558905 pm'!

!String methodsFor: 'converting' stamp: 'jmv 6/16/2016 11:30'!
withoutEnclosing: aCharacter
	"
	'*Hello*' withoutEnclosing: $*
	"
	| s |
	s _ self size.
	^((self at: 1) = aCharacter and: [ (self at: s) = aCharacter ])
		ifTrue: [ self copyFrom: 2 to: s-1 ]
		ifFalse: [ self ]! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 6/16/2016 12:02'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry _ 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string _ self selection asString) isEmpty
		ifTrue: [reply _ '']
		ifFalse: [
			string _ string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars _ (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider _ self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars _ provider explainSpecial: string].
			tiVars _ tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol _ s])
				ifTrue: [
					cgVars _ (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors _ (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors _ self explainPartSel: string].
			cgVars _ cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors _ selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors _ string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers _ self explainNumber: string.
			numbers ifNil: [numbers _ ''].
			delimitors ifNil: [delimitors _ ''].
			reply _ tiVars , cgVars , selectors , delimitors , numbers].
	reply size = 0 ifTrue: [reply _ sorry].

	morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:44'!
explainAnySel: symbol 
	"Is this any message selector?"

	| list reply |
	list _ Smalltalk allClassesImplementing: symbol.
	list size = 0 ifTrue: [^nil].
	list size < 12
		ifTrue: [reply _ ' is a message selector which is defined in these classes ' , list printString]
		ifFalse: [reply _ ' is a message selector which is defined in many classes'].
	^'#' , symbol , reply , '.\' withNewLines, 'Smalltalk browseAllImplementorsOf: #' , symbol! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:50'!
explainChar: string
	"Does string start with a special character?"

	| char |
	char _ string at: 1.
	char = $. ifTrue: [^'Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float).'].
	char = $' ifTrue: [^'The characters between two single quotes are made into an instance of class String'].
	char = $" ifTrue: [^'Double quotes enclose a comment.  Smalltalk ignores everything between double quotes.'].
	char = $# ifTrue: [^'The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants.'].
	(char = $( or: [char = $)]) ifTrue: [^'Expressions enclosed in parenthesis are evaluated first'].
	(char = $[ or: [char = $]]) ifTrue: [^'The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockContext and is usually passed as an argument.'].
	(char = ${ or: [char = $}]) ifTrue: [^ 'A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array'].
	(char = $< or: [char = $>]) ifTrue: [^'<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails.'].
	char = $^ ifTrue: [^'Up arrow means return from this method.  The value returned is the expression following the ^'].
	char = $| ifTrue: [^'Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code.'].
	char = $_ ifTrue: [^'Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it.'].
	char = $; ifTrue: [^'Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon.'].
	char = $: ifTrue: [^'A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon).'].
	char = $$ ifTrue: [^'The single character following a dollar sign is made into an instance of class Character'].
	char = $- ifTrue: [^'A minus sign in front of a number means a negative number.'].
	char = $e ifTrue: [^'An e in the middle of a number means that the exponent follows.'].
	char = $r ifTrue: [^'An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base.'].
	char = Character space ifTrue: [^'the space Character'].
	char = Character tab ifTrue: [^'the tab Character'].
	char = Character cr ifTrue: [^'the carriage return Character'].
	char = Character lf ifTrue: [^'newline. The line feed Character'].
	^nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:36'!
explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| provider class reply classes |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass)
		ifFalse: [^ nil].
	(class _ provider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class _ class soleInstance].
	classes _ (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply _ classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a class variable, defined in class ';
				nextPutAll: reply printString, '\' withNewLines;
				nextPutAll: 'Smalltalk browseAllCallsOn: (';
				nextPutAll: reply printString;
				nextPutAll: ' classPool associationAt: #';
				nextPutAll: symbol;
				nextPutAll: ').']].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply _ pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ String streamContents: [:str |
						str
							nextPutAll: symbol;
							nextPutAll: ' is an undeclared variable.';
							nextPutAll: 'Smalltalk browseAllCallsOn: (Undeclared associationAt: #';
							nextPutAll: symbol;
							nextPutAll: ').']]]
		ifNotNil: 
			[classes _ WriteStream on: Array new.
			Smalltalk
				allBehaviorsDo: [:each | (each sharedPools
						detect: 
							[:pool | 
							pool == reply]
						ifNone: nil)
						notNil ifTrue: [classes nextPut: each]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes contents printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:59'!
explainCtxt: symbol 
	"Is symbol a context variable?"

	| reply classes text cls provider |
	symbol = #nil ifTrue: [reply _ ' is a constant.  It is the only instance of class UndefinedObject.  nil is the initial value of all variables.'].
	symbol = #true ifTrue: [reply _ ' is a constant.  It is the only instance of class True and is the receiver of many control messages.'].
	symbol = #false ifTrue: [reply _ ' is a constant.  It is the only instance of class False and is the receiver of many control messages.'].
	symbol = #thisContext ifTrue: [reply _ ' is a context variable.  Its value is always the MethodContext which is executing this method.'].
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass) ifTrue: [
		cls _ provider selectedClassOrMetaClass].
	cls ifNil: [^ reply].	  "no class known"
	symbol = #self ifTrue: 
			[classes _ cls withAllSubclasses.
			classes size > 12
				ifTrue: [text _ cls printString , ' or a subclass']
				ifFalse: 
					[classes _ classes printString.
					text _ 'one of these classes' , (classes copyFrom: 4 to: classes size)].
			reply _ ' is the receiver of this message; an instance of ' , text ].
	symbol = #super ifTrue: [reply _ 'is just like self.  Messages to super are looked up in the superclass (' , cls superclass printString , ')'].
	^reply ifNotNil: [symbol, reply]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:57'!
explainDelimitor: string
	"Is string enclosed in delimitors?"

	| str |
	(string at: 1) isLetter ifTrue: [^nil].  "only special chars"
	(string first = string last) ifTrue:
			[^ self explainChar: (String with: string first)]
		ifFalse:
			[(string first = $( and: [string last = $)]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $[ and: [string last = $]]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = ${ and: [string last = $}]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $< and: [string last = $>]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $# and: [string last = $)]) ifTrue:
				[^'An instance of class Array.  The Numbers, Characters, or Symbols between the parenthesis are the elements of the Array.'].
			string first = $# ifTrue:
				[^'An instance of class Symbol.'].
			(string first = $$ and: [string size = 2]) ifTrue:
				[^'An instance of class Character.  This one is the character ', (String with: string last), '.'].
			(string first = $:) ifTrue:
				[str := string allButFirst.
				(self explainTemp: str) ifNotNil: [
					^'An argument to this block will be bound to the temporary variable ',
						str, '.']]].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:39'!
explainGlobal: symbol 
	"Is symbol a global variable?"
	| reply classes |
	reply _ Smalltalk at: symbol ifAbsent: [^nil].
	(reply class == Dictionary or:[reply isKindOf: SharedPool class])
		ifTrue: 
			[classes _ Set new.
			Smalltalk allBehaviorsDo: [:each | (each sharedPools detect: [:pool | pool == reply]
					ifNone: nil)
					ifNotNil: [classes add: each]].
			classes _ classes printString.
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a global variable.  It is a pool which is used by the following classes ';
					nextPutAll: (classes allButFirst: 5) ]].
	(reply isKindOf: Behavior)
		ifTrue: [^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a global variable.  ';
				nextPutAll: symbol;
				nextPutAll: ' is a class in category ';
				nextPutAll: reply category, '.', '\' withNewLines;
				nextPutAll: 'BrowserWindow fullOnClass: ';
				nextPutAll: symbol;
				nextPutAll: ' selector: nil';
				nextPut: $.]].
	symbol == #Smalltalk 
		ifTrue: [^ symbol, ' is a global.  Smalltalk is the only instance of SystemDictionary and holds all global variables.'].
	^ String streamContents: [:str |
		str
			nextPutAll: symbol;
			nextPutAll: ' is a global variable.  ';
			nextPutAll: symbol;
			nextPutAll: ' is ';
			nextPutAll: reply printString ]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:39'!
explainInst: string 
	"Is string an instance variable of this class?"
	| classes cls provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass) ifTrue: [
		cls _ provider selectedClassOrMetaClass].
	cls ifNil: [^ nil].	  "no class known"
	classes _ (Array with: cls)
				, cls allSuperclasses.
	classes _ classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes _ classes printString.
	^ String streamContents: [:str |
		str
			nextPutAll: string;
			nextPutAll: ' is an instance variable of the receiver; defined in class ';
			nextPutAll: classes, '\' withNewLines;
			nextPutAll: 'Smalltalk browseAllAccessesTo: ''';
			nextPutAll: string;
			nextPutAll: ''' from: ';
			nextPutAll: classes;
			nextPut: $.]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:41'!
explainMySel: symbol 
	"Is symbol the selector of this method?  Is it sent by this method?  If 
	not, then expalin will call (explainPartSel:) to see if it is a fragment of a 
	selector sent here.  If not, explain will call (explainAnySel:) to catch any 
	selector. "

	| provider lits classes msg |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	(msg _ provider selectedMessageName) ifNil: [^nil].	"not in a message"
	classes _ Smalltalk allClassesImplementing: symbol.
	classes size > 12
		ifTrue: [classes _ 'many classes']
		ifFalse: [classes _ 'these classes ' , classes printString].
	msg = symbol
		ifTrue: [
			^ String streamContents: [:str |
				str
					nextPut: $#;
					nextPutAll: symbol;
					nextPutAll: ' is the selector of this very method!!  It is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]
		ifFalse: [
			lits _ (provider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(lits detect: [:each | each == symbol]
				ifNone: nil)
					ifNil: [^nil].
			^ String streamContents: [:str |
				str
					nextPut: $#;
					nextPutAll: symbol;
					nextPutAll: ' is a message selector which is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]
! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 12:01'!
explainNumber: string 
	"Is string a Number?"

	| strm c |
	(c _ string at: 1) isDigit ifFalse: [(c = $- and: [string size > 1 and: [(string at: 2) isDigit]])
			ifFalse: [^nil]].
	strm _ ReadStream on: string.
	c _ Number readFrom: strm.
	strm atEnd ifFalse: [ ^nil ].
	c printString = string
		ifTrue: [ ^ string , ' is a ' , c class name ]
		ifFalse: [ ^ string , ' (= ' , c printString , ') is a ' , c class name ]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:40'!
explainPartSel: string 
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	(msg _ provider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last == $: ifFalse: [^ nil].
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

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/16/2016 12:03'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	provider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := provider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [' is an argument to this ']
		ifFalse: [' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])! !


!SmalltalkEditor class methodsFor: 'explain' stamp: 'jmv 6/16/2016 11:35'!
plateX
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ ' go to the message list pane, get the menu, and select ''implementors of...''.'! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 6/16/2016 11:23'!
showBalloon: msgString hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w balloon h |
	(w _ self world) ifNil: [^ self].
	h _ aHand.
	h ifNil:[
		h _ w activeHand].
	balloon _ HoverHelpMorph contents: msgString.
	
	"Do it in a while. In some cases, processing the event that might have triggered us might also remove any Help Balloon"
	WorldState addDeferredUIMessage: [
		balloon popUpForHand: h ]! !


!Transcript class methodsFor: 'private' stamp: 'jmv 6/16/2016 11:54'!
addEntry: aString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| msg now |
	logToFile ifTrue: [
		now _ DateAndTime now.
		msg _ String streamContents: [ :strm |
			now printWithMsOn: strm.
			strm
				nextPutAll: ' process:';
				nextPutAll: Processor activeProcess priority printString;
				nextPut: $ ;
				nextPutAll: Processor activeProcess hash printString;
				nextPut: $ ;
				nextPutAll: aString;
				newLine ]].

	self addEntry: (aString copyReplaceAll: String newLineString with: ' ') logToFile: msg! !


!SmalltalkEditor reorganize!
('editing keys' browseIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail: exploreIt inspectIt printIt)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn getMenu2 implementorsOfIt methodSourceContainingIt methodStringsContainingit referencesToIt selectedSelector selectedSymbol sendersOfIt)
('typing/selecting keys' argAdvance: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
!

