'From Cuis 5.0 [latest update: #4505] on 29 December 2020 at 4:43:13 pm'!

!Scanner class methodsFor: 'error descriptions' stamp: 'HAW 12/29/2020 13:17:59'!
unmatechedCommentQuoteErrorDescription

	^'Unmatched comment quote'! !


!String methodsFor: 'converting' stamp: 'jmv 12/29/2020 16:46:01'!
withoutEnclosing: aCharacter
	"
	'*Hello*' withoutEnclosing: $*
	"
	| s |
	s _ self size.
	s = 0 ifTrue: [ ^ self ].
	^((self at: 1) = aCharacter and: [ (self at: s) = aCharacter ])
		ifTrue: [ self copyFrom: 2 to: s-1 ]
		ifFalse: [ self ]! !


!String methodsFor: 'converting' stamp: 'jmv 12/29/2020 16:42:03'!
findSelector
	"Revised to use scanner for better removal of extraneous stuff"
	| potentialSelector colonIndex |
	potentialSelector _ self withBlanksTrimmed withoutEnclosing: $".
	colonIndex _ potentialSelector indexOf: $:.
	"possible keyword selector"
	(colonIndex > 1 and: [ (potentialSelector at: colonIndex - 1) isLetter ]) ifTrue: [
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


!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 12/29/2020 13:18:18'!
xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
    aStream := WriteStream on: (String new: 200).
    self step.
    [hereChar == $"]
        whileFalse:
            [(hereChar == stopChar and: [source atEnd])
                ifTrue: [^self offEnd: self class unmatechedCommentQuoteErrorDescription].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken! !

