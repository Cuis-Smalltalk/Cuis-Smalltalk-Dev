'From Cuis 5.0 [latest update: #4307] on 27 July 2020 at 2:54:38 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/27/2020 14:52:25'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb |
	savedPatch _ prevSavedPatch.
	((savedPatch is: #Form) not or: [savedPatch extent ~= aRectangle extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: aRectangle extent depth: form depth ].
	bb _ BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	savedPatch offset: aRectangle topLeft.
	^savedPatch! !

