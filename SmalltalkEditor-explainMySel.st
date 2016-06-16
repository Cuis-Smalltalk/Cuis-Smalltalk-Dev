'From Cuis 4.2 of 25 July 2013 [latest update: #2820] on 13 June 2016 at 8:18:41.989191 pm'!

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
