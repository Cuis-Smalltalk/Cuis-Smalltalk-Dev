'From Cuis 4.2 of 25 July 2013 [latest update: #2820] on 13 June 2016 at 8:41:47.716191 pm'!

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
