'From Cuis7.9 [latest update: #8108] on 29 July 2026 at 4:48:53 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 29/Jul/2026, 7:46:41 pm (UTC)'!
scanSpecificChangeType
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens firstToken secondToken |

	itemPosition := file position.
	item := file nextChunk.

	tokens := Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [

		firstToken := tokens first.
		firstToken == #classDefinition:
			ifTrue: [
				self scanClassDefinition: tokens.
				^self ].
		firstToken == #classRemoval:
			ifTrue: [
				self scanClassRemoval: tokens.
				^self ].
		firstToken == #methodRemoval:
			ifTrue: [
				self scanMethodRemoval: tokens.
				^self ].
		firstToken == #provides:
			ifTrue: [
				self scanFeatureProvision: tokens.
				^self ].
		firstToken == #requires:
			ifTrue: [
				self scanFeatureRequirement: tokens.
				^self ].
		firstToken == #classRenamed:
			ifTrue: [
				self scanClassRenamed: tokens.
				^self ].

		secondToken := tokens second.
		(secondToken == #methodsFor: or: [ 
			tokens size > 2 and: [ tokens third == #methodsFor: ] ])
			ifTrue: [
				self scanMethodDefinition: tokens.
				^self ].
		secondToken == #commentStamp:
			ifTrue: [
				self scanClassComment: tokens.
				^self ].

		].

		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition! !

!methodRemoval: ChangeList class #knownPreambles stamp: 'jmv 29/Jul/2026, 7:46:57 pm (UTC)'!
ChangeList class removeSelector: #knownPreambles!
!methodRemoval: ChangeList #itemIsRecognized: stamp: 'jmv 29/Jul/2026, 7:46:49 pm (UTC)'!
ChangeList removeSelector: #itemIsRecognized:!
