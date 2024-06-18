'From Cuis7.1 [latest update: #6472] on 17 June 2024 at 7:50:56 pm'!

!Object methodsFor: 'as yet unclassified' stamp: 'jmv 6/17/2024 19:42:51'!
run: aSelector with: arguments in: aReceiver

	self halt: 'This object can not behave like a CompiledMethod!!'.
! !


!Behavior methodsFor: 'testing method dictionary' stamp: 'jmv 6/17/2024 19:50:23'!
thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a 
	literal. Dives into the compact literal notation, making it slow but 
	thorough "

	| who |
	who := Set new.
	self selectorsAndMethodsDo: [ :sel :method |
		(method is: #CompiledMethod) ifTrue: [
			((method hasLiteralThorough: literal) or: [specialFlag and: [method scanFor: specialByte]])
				ifTrue: [
					((literal isVariableBinding) not
						or: [method sendsToSuper not
						"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
						Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
						or: [(method indexOfLiteral: literal) ~= 0]])
							ifTrue: [who add: sel]]]].
	^ who.
! !

