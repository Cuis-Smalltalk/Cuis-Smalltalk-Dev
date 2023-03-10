'From Cuis 6.0 [latest update: #5680] on 9 March 2023 at 6:05:33 pm'!

!CodeProvider class methodsFor: 'services' stamp: 'jmv 3/9/2023 17:24:58'!
normalizeCodeForCodeFiles: aTextOrString
	"Adapt edited code to be suitable or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout."

	| answer |
	answer := aTextOrString asPlainString.
	answer := answer copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	answer := answer copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	^answer! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 3/9/2023 17:00:33'!
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
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	primitiveFormat := primitiveFormat asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/9/2023 18:05:25'!
privateFormatAndConvert

	"Turn := into an arrow, only if requested"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment)
		ifTrue: [
			"Turn $_ and := into a left arrow"
			self replaceStringForRangesWithType: #ansiAssignment with: Character smalltalkLeftArrow asString.
			self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: Character smalltalkUpArrow asString ]
		ifFalse: [
			"Turn $_ into :="
			self replaceStringForRangesWithType: #assignment with: ':='  ].! !

