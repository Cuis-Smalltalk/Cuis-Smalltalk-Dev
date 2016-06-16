'From Cuis 4.2 of 25 July 2013 [latest update: #2820] on 13 June 2016 at 8:18:51.347191 pm'!

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
