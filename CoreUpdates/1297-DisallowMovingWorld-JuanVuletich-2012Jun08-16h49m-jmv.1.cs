'From Cuis 4.0 of 21 April 2012 [latest update: #1296] on 8 June 2012 at 4:59:10 pm'!

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 6/7/2012 08:23'!
position: aPoint
	"Prevent moving a world"

	self isWorldMorph ifFalse: [^super position: aPoint].
"
	super position: aPoint.
	self viewBox ifNotNil: [self viewBox: (aPoint extent: self viewBox extent)].

"! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/7/2012 08:00'!
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
			^ '"' , symbol , ' is the selector of this very method!!  It is defined in ', classes , 
			'.  To see the other definitions, go to the message list pane, get the menu, and select ''implementors of...''."']
		ifFalse: [
			lits _ (provider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(lits detect: [:each | each == symbol]
				ifNone: nil)
					ifNil: [^nil].
			^ '"' , symbol , ' is a message selector which is defined in ', classes , 
			'.  To see the definitions, go to the message list pane, get the menu from, and select ''implementors of...''."'].! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 6/7/2012 08:00'!
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
			s _ '.  To see the other definitions, go to the message list pane, get the menu, and select ''implementors of...''."']
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
			s _ '.  To see the definitions, go to the message list pane, get the menu, and select ''implementors of...''."'].
	classes _ Smalltalk allClassesImplementing: whole.
	classes size > 12
		ifTrue: [classes _ 'many classes']
		ifFalse: [classes _ 'these classes ' , classes printString].
	^ '"' , string , ' is one part of the message selector ' , whole, reply , '  It is defined in ' , classes , s! !

!methodRemoval: Utilities class #graphicsFileSuffixes!
Utilities class removeSelector: #graphicsFileSuffixes!
!methodRemoval: PasteUpMorph #cartesianOrigin!
PasteUpMorph removeSelector: #cartesianOrigin!
