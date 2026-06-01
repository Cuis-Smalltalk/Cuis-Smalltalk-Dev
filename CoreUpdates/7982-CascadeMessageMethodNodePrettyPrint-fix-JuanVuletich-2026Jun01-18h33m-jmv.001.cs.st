'From Cuis7.9 [latest update: #7981] on 1 June 2026 at 7:38:37 pm'!

!MessageNode methodsFor: 'printing' stamp: 'jmv 1/Jun/2026 18:14:00'!
printKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd keywordsInSeparateLines indexLimit |
	args size = 0 ifTrue: [
		receiver ifNotNil: [ aStream space ].
		aStream nextPutAll: key.
		^ self ].
	keywords := key keywords.
	keywordsInSeparateLines := args size > 1 and: [ args anySatisfy: [ :a | a isComplex ] ].
	indexLimit := args size min: keywords size.
	1
		to: indexLimit
		do: [ :i |
			arg := args at: i.
			kwd := keywords at: i.
			(keywordsInSeparateLines and: [receiver notNil])
				ifTrue: [
					indent := 1.
					aStream newLineTab: level + 1.
					"newline after big args, explicit receiver (not a cascade)" ]
				ifFalse: [
					indent := 0.
					(keywordsInSeparateLines and: [i> 1])
						ifTrue: [
							"big args in a cascade, each keyword in a new line"
							aStream newLineTab: level ]
						ifFalse: [
							receiver ifNotNil: [ i = 1 ifTrue: [aStream space] ]]].
			aStream nextPutAll: kwd.
			arg printsInNewLine
				ifTrue: [
					aStream newLineTab: level + indent + 1 ]
				ifFalse: [
					aStream space ].
			arg
				printOn: aStream
				indent: level + 1 + indent
				precedence:
					(precedence = 2
						ifTrue: [ 1 ]
						ifFalse: [ precedence ]).
			(i~=indexLimit and: [args size > 1 and: [ keywordsInSeparateLines not ]]) ifTrue: [ aStream space ]].! !

