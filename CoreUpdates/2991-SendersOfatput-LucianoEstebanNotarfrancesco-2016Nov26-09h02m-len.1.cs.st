'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 9:03:51 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'len 11/26/2016 09:03:25'!
allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection special aList byte |

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	aCollection _ OrderedCollection new.
	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class |
		aList _ class whichSelectorsReferTo: aLiteral special: special byte: byte.
		aList do: [ :sel |
			"For special selectors, look for the literal in the source code.
			Otherwise, for example, searching for senders of #== will include senders of #ifNil.
			Except for #at:put:, because it has two arguments and won't find it in the source code like that."
			(byte isNil or: [aLiteral = #at:put: or: [
				((class sourceCodeAt: sel)
					findString: aLiteral) > 0]]) ifTrue: [

				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel
				)
			]
		]
	].
	^ aCollection! !

