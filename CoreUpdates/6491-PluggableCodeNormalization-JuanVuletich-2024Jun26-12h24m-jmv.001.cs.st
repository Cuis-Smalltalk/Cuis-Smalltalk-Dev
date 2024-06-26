'From Cuis7.1 [latest update: #6490] on 26 June 2024 at 12:37:36 pm'!
!classDefinition: #CodeProvider category: #'System-Text'!
TextProvider subclass: #CodeProvider
	instanceVariableNames: 'currentCompiledMethod contentsSymbol '
	classVariableNames: 'CodeNormalizers '
	poolDictionaries: ''
	category: 'System-Text'!

!CodeProvider class methodsFor: 'services' stamp: 'jmv 6/26/2024 12:27:35'!
addCodeNormalizer: aBlock
	"aBlock should have one argument, the code to process,
	and should evaluate to the processed code
	CodeProvider addCodeNormalizer: [ :code :cls | code, 'This is an extra comment1' asSmalltalkComment ].
	CodeProvider addCodeNormalizer: [ :code :cls | cls compilerClass new format: code in: cls notifying: nil. ].
	CodeProvider addCodeNormalizer: [ :code :cls | code, 'This is an extra comment2' asSmalltalkComment ].
	""
	CodeProvider clearCodeNormalizers.
	"
	CodeNormalizers := CodeNormalizers
		ifNil: [ { aBlock } ]
		ifNotNil: [ CodeNormalizers copyWith: aBlock ].! !

!CodeProvider class methodsFor: 'services' stamp: 'jmv 6/26/2024 11:11:31'!
clearCodeNormalizers
	"Just forget them."

	CodeNormalizers := nil.
! !


!CodeProvider class methodsFor: 'services' stamp: 'jmv 6/26/2024 12:28:03'!
normalizeCodeForSaving: aTextOrString in: aClassOrMetaclass
	"Adapt edited code to be suitable for saving to sources or changes files.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout.
	See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Clipboard >> #storeObject:"

	| answer |
	answer := aTextOrString asPlainString.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkUpArrow asString` with: '^'.
	"Apply code any normalization services. For example, try:
	CodeProvider addCodeNormalizer: [ :code :cls | code, ' 						This is an extra comment1' asSmalltalkComment ].
	CodeProvider addCodeNormalizer: [ :code :cls | cls compilerClass new format: code in: cls notifying: nil. ].
	CodeProvider addCodeNormalizer: [ :code :cls | code, ' 						This is an extra comment2' asSmalltalkComment ].
	""
	CodeProvider clearCodeNormalizers.
	"
	CodeNormalizers ifNotNil: [
		CodeNormalizers do: [ :each |
			answer := each value: answer value: aClassOrMetaclass ]].
	^answer.! !

!classDefinition: #CodeProvider category: #'System-Text'!
TextProvider subclass: #CodeProvider
	instanceVariableNames: 'currentCompiledMethod contentsSymbol'
	classVariableNames: 'CodeNormalizers'
	poolDictionaries: ''
	category: 'System-Text'!
