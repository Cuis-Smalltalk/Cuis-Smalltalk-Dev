'From Cuis 5.0 [latest update: #4506] on 30 December 2020 at 2:38:52 pm'!

!String methodsFor: 'converting' stamp: 'jmv 12/30/2020 14:38:23'!
findSelector
	"Revised to use scanner for better removal of extraneous stuff"
	| potentialSelector colonIndex |
	potentialSelector _ self withBlanksTrimmed withoutEnclosing: $".
	colonIndex _ potentialSelector indexOf: $:.
	"possible keyword selector"
	(colonIndex > 1 and: [ (potentialSelector at: colonIndex - 1) isValidInIdentifiers ]) ifTrue: [
		potentialSelector _ [ Scanner findSelectorIn: potentialSelector ] on: Error do: [ :anError | 
			anError messageText = Scanner unmatechedCommentQuoteErrorDescription 
				ifTrue: [ 
					potentialSelector _ potentialSelector copyWithout: $".
					anError retry ]
				ifFalse: [ anError return: '']]].
		
	potentialSelector isEmpty ifTrue: [ ^ nil ].
	Symbol
		hasInterned: potentialSelector
		ifTrue: [ :aSymbol | ^ aSymbol ].
		
	^ nil.! !

