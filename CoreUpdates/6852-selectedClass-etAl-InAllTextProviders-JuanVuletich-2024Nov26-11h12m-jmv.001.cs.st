'From Cuis7.1 [latest update: #6850] on 26 November 2024 at 11:32:33 am'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:28:44'!
currentCompiledMethod
	"Answer the CompiledMethod installed in the system forthe currently selected message.
	Answer nil if no such concept exists."
	^ nil! !

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:27:56'!
selectedClass
	"Answer the class that is currently selected.
	Answer nil if no concept of selectedClass exists."
	^ nil! !

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:28:23'!
selectedClassOrMetaClass
	"Answer the selected class or metaclass.
	Answer nil if no concept of selectedClassOrMetaClass exists."
	^ nil! !

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:28:28'!
selectedMessageName
	"Answer the message selector of the currently selected message.
	Answer nil if no concept of selectedMessageName exists."
	^ nil! !


!TextProvider methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:28:47'!
currentCompiledMethod
	"Answer the CompiledMethod installed in the system forthe currently selected message.
	Answer nil if no such concept exists."
	^ nil! !

!TextProvider methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:24:43'!
selectedClass
	"Answer the class that is currently selected.
	Answer nil if no concept of selectedClass exists."
	^ nil! !

!TextProvider methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:28:34'!
selectedClassOrMetaClass
	"Answer the selected class or metaclass.
	Answer nil if no concept of selectedClassOrMetaClass exists."
	^ nil! !

!TextProvider methodsFor: 'accessing' stamp: 'jmv 11/26/2024 11:28:37'!
selectedMessageName
	"Answer the message selector of the currently selected message.
	Answer nil if no concept of selectedMessageName exists."
	^ nil! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:23:03'!
explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| class reply classes |
	(class := self codeProvider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class := class soleInstance].
	classes := (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply := classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
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
						[reply := pool.
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
			[classes := Array streamContents: [ :strm |
				Smalltalk
					allBehaviorsDo: [:each | (each sharedPools
							detect: 
								[:pool | 
								pool == reply]
							ifNone: nil)
							notNil ifTrue: [strm nextPut: each]]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:24:02'!
explainCtxt: symbol 
	"Is symbol a context variable?"

	| reply classes text cls |
	symbol = #nil ifTrue: [reply := ' is a constant.  It is the only instance of class UndefinedObject.  nil is the initial value of all variables.'].
	symbol = #true ifTrue: [reply := ' is a constant.  It is the only instance of class True and is the receiver of many control messages.'].
	symbol = #false ifTrue: [reply := ' is a constant.  It is the only instance of class False and is the receiver of many control messages.'].
	symbol = #thisContext ifTrue: [reply := ' is a context variable.  Its value is always the MethodContext which is executing this method.'].
	cls := self codeProvider selectedClassOrMetaClass.
	cls ifNil: [^ reply].	  "no class known"
	symbol = #self ifTrue: 
			[classes := cls withAllSubclasses.
			classes size > 12
				ifTrue: [text := cls printString , ' or a subclass']
				ifFalse: 
					[classes := classes printString.
					text := 'one of these classes' , (classes copyFrom: 4 to: classes size)].
			reply := ' is the receiver of this message; an instance of ' , text ].
	symbol = #super ifTrue: [reply := 'is just like self.  Messages to super are looked up in the superclass (' , cls superclass printString , ')'].
	^reply ifNotNil: [symbol, reply]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:24:17'!
explainInst: string 
	"Is string an instance variable of this class?"
	| classes cls |
	cls := self codeProvider selectedClassOrMetaClass.
	cls ifNil: [^ nil].	  "no class known"
	classes := (Array with: cls)
				, cls allSuperclasses.
	classes := classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes := classes printString.
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

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:25:06'!
explainMySel: symbol 
	"Is symbol the selector of this method?  Is it sent by this method?  If 
	not, then expalin will call (explainPartSel:) to see if it is a fragment of a 
	selector sent here.  If not, explain will call (explainAnySel:) to catch any 
	selector. "

	| lits classes msg |
	(msg := self codeProvider selectedMessageName) ifNil: [^nil].	"not in a message"
	classes := Smalltalk allClassesImplementing: symbol.
	classes size > 12
		ifTrue: [classes := 'many classes']
		ifFalse: [classes := 'these classes ' , classes printString].
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
			lits := (self codeProvider selectedClassOrMetaClass compiledMethodAt: msg) messages.
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

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:25:33'!
explainPartSel: string 
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg |
	(msg := self codeProvider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last = $: ifFalse: [^ nil].
	"Name of this method"
	lits := Array with: msg.
	(whole := lits detect: [:each | (each keywords detect: [:frag | frag = string]
					ifNone: nil) notNil]
				ifNone: nil)
		ifNotNil: [
			reply := ', which is the selector of this very method!!'.
			s := self class plateA]
		ifNil: [ 
			"Selectors called from this method"
			lits := (self codeProvider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(whole := lits detect: [:each | (each keywords detect: [ :frag | frag = string ]
							ifNone: nil) notNil]
						ifNone: nil) notNil
				ifFalse: [string = 'primitive:'
					ifTrue: [^self explainChar: '<']
					ifFalse: [^nil]].
			reply := '.'.
			s := self class plateB].
		classes := Smalltalk allClassesImplementing: whole.
		classes size > 12
			ifTrue: [classes := 'many classes']
			ifFalse: [classes := 'these classes ' , classes printString].
		^ String streamContents: [:str |
			str
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 11/26/2024 10:25:57'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	self codeProvider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/26/2024 10:26:28'!
referencesToIt
	"Open a references browser on the selected symbol: a variable name or class name"

	| selectedSymbol environment selectedString |
	self hasSelection ifFalse: [ self selectWord ].
	selectedSymbol := self selectedSymbol.

	"convenient access to class variables, including those in SharedPools"
	environment := self codeProvider selectedClassOrMetaClass ifNil: [ Smalltalk ].

	(selectedSymbol ifNotNil: [environment bindingOf: selectedSymbol]) ifNotNil: [ :reference |
		Smalltalk browseAllCallsOn: reference.
		^ self ].

	selectedString := self selectedString withoutSeparators.
	(environment ~= Smalltalk and: [ environment definesInstanceVariableNamedInHierarchy: selectedString ]) ifTrue: [
		Smalltalk browseAllAccessesTo: selectedString from: environment.
		^ self ].

	self referencesToSelectedLiteral.! !

