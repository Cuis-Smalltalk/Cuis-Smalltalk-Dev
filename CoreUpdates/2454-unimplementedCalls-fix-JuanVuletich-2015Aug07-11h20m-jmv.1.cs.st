'From Cuis 4.2 of 25 July 2013 [latest update: #2453] on 7 August 2015 at 11:39:32.90059 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 8/7/2015 11:39'!
allUnimplementedCalls
	"Answer an Array of each message that is sent by an expression in a 
	method but is not implemented by any object in the system."

	| aStream all  selectors |
	all _ self allImplementedMessages.
	aStream _ WriteStream on: (Array new: 50).
	self allBehaviorsDo: [ :cl |
		 cl selectorsDo: [ :sel |
			selectors _ OrderedCollection new.
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [selectors add: m ]].
			selectors notEmpty ifTrue: [
				aStream nextPut: (MethodReference class: cl selector: sel).
				(cl name , '>>#' , sel , (String new: (70-sel size * 15//10 max: 0) withAll: $ ), ' calls:          ', selectors asArray printString) print.
				"aStream nextPut: cl name , ' ' , sel , ' calls: ', secondStream contents" ]]].
	^aStream contents! !

