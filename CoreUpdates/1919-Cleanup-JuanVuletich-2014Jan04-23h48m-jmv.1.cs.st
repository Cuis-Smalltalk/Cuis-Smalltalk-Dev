'From Cuis 4.2 of 25 July 2013 [latest update: #1918] on 4 January 2014 at 11:50:23.900827 pm'!

!CodeFile methodsFor: 'private' stamp: 'jmv 1/4/2014 22:21'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Smalltalk actualScannerClass new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) class == String]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) class == String]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass metaClass classComment: tokens last].
! !


!PseudoClass methodsFor: 'class' stamp: 'jmv 1/4/2014 22:21'!
comment: aString
	self classComment: aString.! !

!PseudoClass methodsFor: 'removing' stamp: 'jmv 1/4/2014 22:20'!
removeAllUnmodified
	| stClass |
	self exists ifFalse:[^self].
	self removeUnmodifiedMethods: self selectors.
	stClass := self realClass.
	(self hasDefinition and:[stClass definition = self definition]) ifTrue:[definition := nil].
	(self hasComment and:[stClass comment asString = self comment asString]) ifTrue:[ self classComment: nil].
	metaClass ifNotNil: [ metaClass removeAllUnmodified]! !


!Theme methodsFor: 'icon lookup' stamp: 'jmv 1/4/2014 22:11'!
prepend: aTheme toContentSpec: anArray

	^ anArray copyWithFirst: aTheme printString! !

!methodRemoval: PseudoClass #commentString!
PseudoClass removeSelector: #commentString!
!methodRemoval: PseudoClass #commentString:!
PseudoClass removeSelector: #commentString:!
!methodRemoval: String #stringRepresentation!
String removeSelector: #stringRepresentation!
!methodRemoval: Object #stringRepresentation!
Object removeSelector: #stringRepresentation!
