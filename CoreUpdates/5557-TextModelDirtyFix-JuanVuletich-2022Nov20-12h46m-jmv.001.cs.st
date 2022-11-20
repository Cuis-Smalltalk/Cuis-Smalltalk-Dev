'From Cuis 6.0 [latest update: #5556] on 20 November 2022 at 1:11:55 pm'!

!SHTextStylerST80 class methodsFor: 'services' stamp: 'jmv 11/20/2022 13:09:47'!
unFormat: aString
	^CodeProvider normalizeCodeForCodeFiles: aString! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 11/20/2022 13:11:43'!
isDirty

	| savedContents |
	textGetter ifNil: [ ^super isDirty ].
	savedContents _ textProvider perform: textGetter.
	savedContents = actualContents ifTrue: [ ^false ].
	self textStylerClass ifNotNil: [ :stylerClass |
		"For Smalltalk code, auto conversion of assignment and return arrows doesn't mean dirty."
		savedContents = (stylerClass unFormat: actualContents) ifTrue: [ ^false ]].
	^true! !


!CodeProvider class methodsFor: 'services' stamp: 'jmv 11/20/2022 12:49:01'!
normalizeCodeForCodeFiles: aTextOrString
	"Adapt edited code to be suitable or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' and/or '_' for assignment, '^' for return, and other replacements of
	ASCII by Unicode symbols done by Shout."

	| answer |
	answer _ aTextOrString asPlainString.
	answer _ answer copyReplaceAll: Character smalltalkLeftArrow asString with: '_'.
	answer _ answer copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	^answer! !


!SHTextStylerST80 class reorganize!
('preferences' ansiAssignmentPreferenceChanged leftArrowAssignmentPreferenceChanged)
('style table' attributeArrayForColor:emphasis: attributesFor: defaultStyleTable initialTextAttributes styleTable)
('class initialization' initialize releaseClassCachedState)
('services' unFormat:)
!

