'From Cuis 4.2 of 25 July 2013 [latest update: #2628] on 9 December 2015 at 9:37:31.409727 am'!

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/9/2015 09:27'!
derivativeFont: aStrikeFontOrNil at: index
	"Store aStrikeFontOrNil at index
	If arg is nil, then remove font at index. But if index = 0, then remove all derivatives."

	(aStrikeFontOrNil isNil and: [ index = 0 ]) 
		ifTrue: [
			derivativeFonts _ nil.
			^ self].

	self isBaseFont ifFalse: [ 
		derivativeFonts _ nil.
		self error: 'Derivative fonts can not have derivatives' ].
	
	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	aStrikeFontOrNil
		ifNil: [ derivativeFonts removeKey: index ]
		ifNotNil: [
			derivativeFonts at: index put: aStrikeFontOrNil.
			aStrikeFontOrNil baseFont: self ]! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/9/2015 09:36'!
reset
	"Reset the cache of derivative emphasized fonts
	StrikeFont allInstancesDo: [ :f | f reset ]
	"
	
	| newDict |
	derivativeFonts ifNotNil: [
		newDict _ Dictionary new.
		"all after 16 are synthetic, and can be recreated on demand"
		1 to: 15 do: [ :i |
			(derivativeFonts at: i) ifNotNil: [ :f |
				newDict at: i put: f ]].
		derivativeFonts _ newDict ]! !

!StrikeFont methodsFor: 'private' stamp: 'jmv 12/9/2015 09:24'!
fixDerivatives
	self isBaseFont
		ifTrue: [
			baseFont _ nil.
			derivativeFonts ifNotNil: [
				derivativeFonts valuesDo: [ :der |
					der ifNotNil: [
						der baseFont: self.
						der fixDerivatives ]]]]
		ifFalse: [
			derivativeFonts _ nil ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
StrikeFont allInstancesDo: [ :f | f reset ]!

