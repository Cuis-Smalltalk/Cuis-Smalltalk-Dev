'From Cuis 4.2 of 25 July 2013 [latest update: #2823] on 16 June 2016 at 10:31:35.331655 am'!

!SmalltalkEditor class methodsFor: 'explain' stamp: 'dhn 6/13/2016 17:37'!
plateA
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ '.  To see the other definitions,', self plateX! !

!SmalltalkEditor class methodsFor: 'explain' stamp: 'dhn 6/13/2016 17:36'!
plateB
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ '.  To see the definitions,', self plateX! !

!SmalltalkEditor class methodsFor: 'explain' stamp: 'dhn 6/13/2016 17:35'!
plateX
	"As in boiler plate. Answer a string which appears in several explanations"
	
	^ ' go to the message list pane, get the menu, and select ''implementors of...''."'! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/13/2016 19:51'!
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
				nextPutAll: '"is a class variable, defined in class ';
				nextPutAll: reply printString, '"\' withNewLines;
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
							nextPutAll: '"is an undeclared variable.';
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
					nextPutAll: '"is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes contents printString , '"\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/13/2016 20:38'!
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
					nextPutAll: '"is a global variable.  It is a pool which is used by the following classes ';
					nextPutAll: (classes allButFirst: 5);
					nextPut: $"]].
	(reply isKindOf: Behavior)
		ifTrue: [^ String streamContents: [:str |
			str
				nextPutAll: '"is a global variable.  ';
				nextPutAll: symbol;
				nextPutAll: ' is a class in category ';
				nextPutAll: reply category, '."', '\' withNewLines;
				nextPutAll: 'BrowserWindow fullOnClass: ';
				nextPutAll: symbol;
				nextPutAll: ' selector: nil';
				nextPut: $.]].
	symbol == #Smalltalk 
		ifTrue: [^'"is a global.  Smalltalk is the only instance of SystemDictionary and holds all global variables."'].
	^ String streamContents: [:str |
		str
			nextPutAll: '"is a global variable.  ';
			nextPutAll: symbol;
			nextPutAll: ' is ';
			nextPutAll: reply printString;
			nextPut: $"]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/13/2016 18:21'!
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
			nextPutAll: '"is an instance variable of the receiver; defined in class ';
			nextPutAll: classes, '"\' withNewLines;
			nextPutAll: 'Smalltalk browseAllAccessesTo: ''';
			nextPutAll: string;
			nextPutAll: ''' from: ';
			nextPutAll: classes;
			nextPut: $.]! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/13/2016 17:33'!
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
					nextPut: $";
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
					nextPut: $";
					nextPutAll: symbol;
					nextPutAll: ' is a message selector which is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]
! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/13/2016 18:10'!
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
				nextPut: $";
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]! !

