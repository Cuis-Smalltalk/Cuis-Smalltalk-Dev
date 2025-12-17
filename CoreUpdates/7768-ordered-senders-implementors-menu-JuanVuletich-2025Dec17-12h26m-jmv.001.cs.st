'From Cuis7.5 [latest update: #7766] on 17 December 2025 at 12:27:10 pm'!

!InstructionStream methodsFor: 'scanning' stamp: 'jmv 17/Dec/2025 12:14:16'!
addSelectorOnceTo: aCollection auxSet: set
	"If this instruction is a send, add its selector to aCollection.
	Use set to only add each once."

	| selectorOrSelf |
	(selectorOrSelf := self selectorToSendOrSelf) == self ifFalse: [
		(set includes: selectorOrSelf) ifFalse: [
			set add: selectorOrSelf.
			aCollection add: selectorOrSelf ]].! !


!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 17/Dec/2025 12:23:19'!
messages
	"Answer a Set of all the message selectors sent by this method."

	| scanner selectors auxSet |
	selectors := OrderedCollection new.
	auxSet := Set new.
	scanner := InstructionStream on: self.
	scanner	
		scanFor: [ :x |
			scanner addSelectorOnceTo: selectors auxSet: auxSet.
			false	"keep scanning"].
	^selectors! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 17/Dec/2025 12:17:00'!
allUnimplementedCalls
	"Answer an Array of each message that is sent by an expression in a 
	method but is not implemented by any object in the system."

	| aStream all  selectors |
	all := self allImplementedMessages.
	aStream := WriteStream on: (Array new: 50).
	self allBehaviorsDo: [ :cl |
		 cl selectorsDo: [ :sel |
			selectors := OrderedCollection new.
			(cl compiledMethodAt: sel) messagesDo: [ :m |
				(all includes: m) ifFalse: [selectors add: m ]].
			selectors notEmpty ifTrue: [
				aStream nextPut: (MethodReference class: cl selector: sel).
				(cl name , '>>#' , sel , (String new: (70-sel size * 15//10 max: 0) withAll: $ ), ' calls:          ', selectors asArray printString) print.
				"aStream nextPut: cl name , ' ' , sel , ' calls: ', secondStream contents" ]]].
	^aStream contents! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 17/Dec/2025 12:17:18'!
unimplemented
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all := IdentitySet new.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented := IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messagesDo: [ :m |
				(all includes: m) ifFalse: [
					entry := unimplemented at: m ifAbsent: [#()].
					entry := entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 17/Dec/2025 12:21:23'!
showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption
	"Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters.  Use aCaption as the menu title, if it is not nil.  Evaluate choiceBlock if a message is chosen."
	| index menuLabels orderedList aMenu |
	orderedList := selectorCollection isSequenceable
		ifTrue: [ selectorCollection ]
		ifFalse: [ selectorCollection asArray sort ].
	menuLabels := String streamContents: [ :strm |
		strm nextPutAll: (firstItem contractTo: 40).
		orderedList do: [ :sel |
			strm
				 newLine;
				 nextPutAll: (sel contractTo: 40) ]].
	aMenu := PopUpMenu
		labels: menuLabels
		lines: #(1 ).
	index := aCaption
		ifNil: [ aMenu startUpMenu ]
		ifNotNil: [ aMenu startUpWithCaption: aCaption ].
	index = 1 ifTrue: [ choiceBlock value: firstItem ].
	index > 1 ifTrue: [ choiceBlock value: (orderedList at: index - 1) ].! !

!methodRemoval: InstructionStream #addSelectorTo: stamp: 'jmv 17/Dec/2025 12:26:48'!
InstructionStream removeSelector: #addSelectorTo:!
