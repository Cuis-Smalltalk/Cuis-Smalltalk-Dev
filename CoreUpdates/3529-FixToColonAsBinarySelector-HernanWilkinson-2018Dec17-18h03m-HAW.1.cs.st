'From Cuis 5.0 of 7 November 2016 [latest update: #3526] on 17 December 2018 at 6:04:42 pm'!

!String methodsFor: 'converting' stamp: 'HAW 12/17/2018 18:03:42'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| kwd char keywords isAllValidInIdentifiers |
	keywords _ Array streamContents: [ :kwds |
		kwd _ WriteStream on: (String new: 16).
		isAllValidInIdentifiers _ true.
		1
			to: self size
			do: [ :i |
				char _ self at: i.
				kwd nextPut: char.
				char = $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers _ true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers _ false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.! !

