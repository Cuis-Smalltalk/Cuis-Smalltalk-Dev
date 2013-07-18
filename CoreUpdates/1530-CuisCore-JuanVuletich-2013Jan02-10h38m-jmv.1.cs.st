'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 10:48:42 am'!

!Clipboard methodsFor: 'accessing' stamp: 'jmv 1/2/2013 10:48'!
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
	Another copy must be made again when pasting, as the same object could be pasted many times."
	contents _ (anObject isString or: [ anObject is: #Text])
		ifTrue: [ anObject withCuisLineEndings ]
		ifFalse: [ anObject copyForClipboard ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface | | interface |
		interface _ clipboardInterface current.
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
	primitiveFormat _ (self stringOrIdFor: contents) iso8859s15ToUtf8.
	self primitiveClipboardString: primitiveFormat! !


!String methodsFor: 'converting' stamp: 'jmv 1/2/2013 10:45'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr _ Character crCharacter.
	lf _ Character lfCharacter.
	crlf _ CharacterSet new.
	crlf add: cr; add: lf.

	inPos _ 1.
	outPos _ 1.
	lineEndingSize _ lineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString _ String new: self size * lineEndingSize.

	[
		lineEndPos _ self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos _ outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: self startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (lineEndingString at: i).
				newOutPos _ newOutPos + 1 ].
			outPos _ newOutPos.
			((self at: lineEndPos) = cr and: [ lineEndPos < self size and: [ (self at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos _ lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos _ lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos _ outPos + (self size - inPos).
	outString replaceFrom: outPos to: newOutPos with: self startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !

