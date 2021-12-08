'From Cuis 5.0 [latest update: #4903] on 8 October 2021 at 9:38:53 am'!

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 10/8/2021 09:36:16'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb prevExtent extent |
	savedPatch _ prevSavedPatch.
	prevExtent _ 0@0.
	((savedPatch is: #Form) and: [
		prevExtent _ savedPatch extent.
		prevExtent >= aRectangle extent]) 
			ifTrue: [
				savedPatch offset: 0@0 ]
			ifFalse: [
				"allocate new patch form if needed"
				extent _ aRectangle extent max: prevExtent.
				extent _ (extent x // 64 + 2 * 64) @ (extent y + 64).						"Make it slightly larger, and even width."
				savedPatch _ Form extent: extent depth: form depth ].
	bb _ BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^savedPatch! !

