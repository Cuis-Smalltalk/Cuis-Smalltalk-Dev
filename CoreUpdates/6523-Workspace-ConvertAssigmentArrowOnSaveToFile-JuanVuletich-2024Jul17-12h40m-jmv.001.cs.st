'From Cuis7.1 [latest update: #6522] on 17 July 2024 at 12:46:28 pm'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 7/17/2024 12:28:38'!
contentsForFileSaving
	^self actualContents asPlainString! !


!Workspace methodsFor: 'accessing' stamp: 'jmv 7/17/2024 12:34:18'!
contentsForFileSaving
	"See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Clipboard >> #storeObject:
	See also CodeProvider class >> #normalizeCodeForSaving:in:	"
	| normalized |
	normalized := self actualContents asPlainString.
	normalized := normalized copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	normalized := normalized copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	normalized := normalized copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	^normalized! !


!CodeProvider class methodsFor: 'services' stamp: 'jmv 7/17/2024 12:33:18'!
normalizeCodeForSaving: aTextOrString in: aClassOrMetaclass
	"Adapt edited code to be suitable for saving to sources or changes files.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout.
	See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Clipboard >> #storeObject:
	See also Workspace >> #contentsForFileSaving"

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


!SystemWindow methodsFor: 'user interface' stamp: 'jmv 7/17/2024 12:29:13'!
forceSaveContentsTo: aFileEntry
	"Saves the contents to the given filename.
	
	Force overwrting the file if it exists."

	self hasSaveAs ifFalse: [^self ].
	aFileEntry forceWriteStreamDo: [ :stream |
		stream nextPutAll: model contentsForFileSaving ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName.! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 7/17/2024 12:29:08'!
saveContentsTo: aFileEntry
	"Saves the contents to the given filename"

	self hasSaveAs ifFalse: [^self].
	aFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: model contentsForFileSaving ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName;
		notifyUserWith: 'Contents saved to', String newLineString, aFileEntry pathName.! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 7/17/2024 12:36:42'!
storeObject: anObject
	"Set new contents on the clipboard. Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isString or: [ anObject is: #Text])
		ifTrue: [
			contents := anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents := anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present.
	Warning: This has not yet been adapted to UTF-8 Unicode"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id := self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat := self stringOrIdFor: contents.
	"See also CodeProvider class >> #normalizeCodeForSaving:in:	
	See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Workspace >> #contentsForFileSaving"
	primitiveFormat := primitiveFormat copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	primitiveFormat := primitiveFormat asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat! !


!SmalltalkEditor methodsFor: 'clipboard' stamp: 'jmv 7/17/2024 12:38:55'!
clipboardStringOrText
	"When pasting possibly external text into Smalltalk code,
	assume any Unicode left and up arrows are meant to be Assignment and Return.
	This is just a help to users. Not really necessary for the system itself."
	| clipContents |
	clipContents := super clipboardStringOrText.
	self replaceUnicodeArrowsWithSmalltalkArrows ifTrue: [
		clipContents := clipContents copyReplaceAll: `(Character codePoint: 16r2190) asString` with: `(Character smalltalkLeftArrow ) asString`.
		clipContents := clipContents copyReplaceAll: `(Character codePoint: 16r2191) asString` with: `(Character smalltalkUpArrow ) asString`.
		].
	^clipContents.! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 7/17/2024 12:36:15'!
privateConvertAssignmentsOffset: delta
	"See also CodeProvider class >> #normalizeCodeForSaving:in:	
	See also Clipboard >> #storeObject:
	See also Workspace >> #contentsForFileSaving"
	"Turn $_ into a left arrow"

	self
		replaceStringForRangesWithType: #assignment
		with: `Character smalltalkLeftArrow asString`
		offset: delta.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow. Include a #zeroWidthSpace so as not to modify ranges, etc."
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString`
				offset: delta.
			"Turn $^into up arrow"
			self
				replaceStringForRangesWithType: #return
				with: `Character smalltalkUpArrow asString`
				offset: delta ].! !

!methodRemoval: TextModel #saveOn: stamp: 'jmv 7/17/2024 12:42:05'!
TextModel removeSelector: #saveOn:!
