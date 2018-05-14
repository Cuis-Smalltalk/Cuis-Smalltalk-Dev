'From Cuis 5.0 of 7 November 2016 [latest update: #3327] on 13 May 2018 at 7:48:23 pm'!

!ContentPack methodsFor: 'accessing' stamp: 'jmv 5/13/2018 19:47:44'!
from: key get: anArray

"
	Theme content from: #Theme get: #('16x16' 'actions' 'view-refresh.png' )
	Answer nil when the object isn't found.
		--cbr	"
		
	| object | object _ self at: key ifAbsent: [ ^ nil ].
	
	anArray do: [ :i | object _ object at: i ifAbsent: [ ^ nil ]].
	
	^ object! !


!Theme methodsFor: 'icon lookup' stamp: 'jmv 5/13/2018 19:47:13'!
fetch: aTuple "	#( 'resolution' 'context' 'filename' )	"

	"Get an icon from Content. See icons protocol."

	| contentSpecifier icon themeGuess |
	
	icon _ nil.
	themeGuess _ self class.
	contentSpecifier _ self appendExtensionToContentSpec: aTuple.

	[ icon isNil ] 
		whileTrue: [
			icon _ self class content
				from: themeGuess name
				get: contentSpecifier.
	
			icon ifNotNil: [ ^ icon ].
	
			themeGuess = Theme content
				ifTrue: [ ^ nil "See comment in ContentPack>>get: --cbr" ].
	
			themeGuess _ themeGuess superclass
		]! !

!methodRemoval: ContentPack #get:!
ContentPack removeSelector: #get:!
