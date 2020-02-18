'From Cuis 5.0 [latest update: #4034] on 10 February 2020 at 1:34:40 pm'!

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 2/10/2020 13:34:12'!
fontPreferenceChanged
	self recreateDefaultDesktop.
	super fontPreferenceChanged! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 2/10/2020 13:34:19'!
defaultFamilyName: aStringOrNil defaultPointSize: aNumberOrNil
	"Nil just means leave it as it is now."
	| family |
	aStringOrNil ifNotNil: [DefaultFamilyName _ aStringOrNil].
	aNumberOrNil ifNotNil: [DefaultPointSize _ aNumberOrNil].
	self revisar. "Please remove this hack meant to install larger DejaVu StrikeFonts on demand."
	DefaultFamilyName =  'DejaVu' ifTrue: [
		family _ AvailableFamilies at: DefaultFamilyName.
		(family pointSizes includes: DefaultPointSize) ifFalse: [ StrikeFont install: DefaultFamilyName]]! !

