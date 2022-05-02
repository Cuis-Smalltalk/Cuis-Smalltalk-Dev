'From Cuis 6.0 [latest update: #5123] on 29 April 2022 at 5:21:48 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 4/29/2022 16:42:11'!
replaceAll: oldObject with: newObject 
	"Replace all occurences of oldObject with newObject"
	| index |
	index _ self indexOf: oldObject startingAt: 1.
	[index = 0] whileFalse: [
		self at: index put: newObject.
		index _ self indexOf: oldObject startingAt: index + 1 ].! !


!String methodsFor: 'converting' stamp: 'jmv 4/29/2022 15:07:16'!
asUtf8
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	See #fromUtf8: "

	^self asUtf8: true.! !

!String methodsFor: 'converting' stamp: 'jmv 4/29/2022 16:20:22'!
withCuisLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings.
	Answer a new instance, replacing each occurence with a single Lf.
	('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	"
	^ self withLineEndings: `String newLineString`.! !

!String methodsFor: 'converting' stamp: 'jmv 4/29/2022 15:00:21'!
withDescriptiveLineEndings
	"
	('Hello', String newLineString, 'World') withDescriptiveLineEndings
	"
	^ self withLineEndings: '[enter]'! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 16:36:42'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
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
	(anObject isStringLike or: [ anObject is: #Text])
		ifTrue: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id _ self idFor: contents.
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
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8: true.
	self primitiveClipboardString: primitiveFormat! !

!methodRemoval: Text #fixLineEndings stamp: 'jmv 4/29/2022 17:20:09'!
Text removeSelector: #fixLineEndings!
!methodRemoval: Text #withCuisLineEndings stamp: 'jmv 4/29/2022 17:20:09'!
Text removeSelector: #withCuisLineEndings!
!methodRemoval: Text #copyReplaceTokens:with: stamp: 'jmv 4/29/2022 17:20:09'!
Text removeSelector: #copyReplaceTokens:with:!
