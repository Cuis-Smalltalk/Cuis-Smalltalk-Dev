'From Cuis7.9 [latest update: #8111] on 30 July 2026 at 10:24:22 am'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Jul/2026, 1:03:33 pm (UTC)'!
scanMethodDefinitionNoCats: tokens

	| className |
	
	className := tokens first.
	
	tokens second == #methods ifTrue: [
		self scanCategory: ClassOrganizer defaultMethodCategory class: className meta: false stamp: '' prior: nil libraryName: nil.
		^self ].

	(tokens second == #class and: [ tokens third == #methods]) ifTrue: [
		self scanCategory: ClassOrganizer defaultMethodCategory class: className meta: true stamp: '' prior: nil libraryName: nil.
		^self ].
	
	self error: 'Unsupported method definition' 
! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Jul/2026, 1:04:28 pm (UTC)'!
scanSpecificChangeType
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens keyword |

	itemPosition := file position.
	item := file nextChunk.

	tokens := Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		
		"Stand alone keywords are the first token and are not message sends."
		keyword := tokens first.
		keyword == #classDefinition:
			ifTrue: [
				self scanClassDefinition: tokens.
				^self ].
		keyword == #classRemoval:
			ifTrue: [
				self scanClassRemoval: tokens.
				^self ].
		keyword == #methodRemoval:
			ifTrue: [
				self scanMethodRemoval: tokens.
				^self ].
		keyword == #provides:
			ifTrue: [
				self scanFeatureProvision: tokens.
				^self ].
		keyword == #requires:
			ifTrue: [
				self scanFeatureRequirement: tokens.
				^self ].
		keyword == #classRenamed:
			ifTrue: [
				self scanClassRenamed: tokens.
				^self ].
		
		"The first tokens are {aClassName} or {aClassName. 'class'}, following token is traditionally
		the first keyword of a message send to the class or metaclass."
		keyword := tokens second.
		(keyword = #class and: [tokens size > 2])
			ifTrue: [ keyword := tokens third ].
		keyword == #methodsFor:
			ifTrue: [
				self scanMethodDefinition: tokens.
				^self ].
		keyword == #methods
			ifTrue: [
				self scanMethodDefinitionNoCats: tokens.
				^self ].
		keyword == #commentStamp:
			ifTrue: [
				self scanClassComment: tokens.
				^self ].
		
		].

		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition! !

